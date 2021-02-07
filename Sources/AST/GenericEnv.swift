import Basic

/// A key in the existential lookup table.
fileprivate typealias ExistentialKey = HashableBox<ValType, ReferenceHashWitness<ValType>>

/// An environment describing mappings between generic types and existential types.
///
/// Generic types have to be "contextualized" before they can be assigned to an expression. This
/// consists of substituting either fresh variables or existential types for each of their generic
/// type parameters, depending on the declaration space from which they are being used. If a type
/// parameter is used *within* the generic space that introduces it, it has to be substituted by an
/// existential type. In contrast, if it is used *outside* of its declaration space, then it must
/// be "opened" as a fresh variable.
public final class GenericEnv {

  public init(space: GenericDeclSpace) {
    self.space = space
    self.params = []
    self.typeReqs = []
  }

  public init?(
    space   : GenericDeclSpace,
    params  : [GenericParamType],
    typeReqs: [TypeReq],
    context : Context
  ) {
    self.space = space
    self.params = params
    self.typeReqs = typeReqs

    let prepare = context.prepareGenericEnv
    precondition(prepare != nil, "no generic environment delegate")
    guard prepare!(self) else { return nil }
  }

  /// The declaration space to which the environment is attached.
  public unowned let space: GenericDeclSpace

  // The generic parameters of the environment.
  public let params: [GenericParamType]

  /// The type requirements on the parameters of the environment.
  public let typeReqs: [TypeReq]

  /// The equivalence classes of the environment.
  ///
  /// This describes the equivalence classes of the set of generic type parameters, as inferred
  /// from related type requirements. This is used by the constraint type solver to decide whether
  /// two existential types are equivalent.
  public var equivalences = EquivalenceClassSet()

  /// The known conformances of the environment's generic type parameters.
  private var conformanceTables: [ExistentialKey: [ViewConformance]] = [:]

  /// The set of conformances for the given existential type.
  public func conformances(of type: ExistentialType) -> [ViewConformance]? {
    return conformanceTables[ExistentialKey(type)]
  }

  /// Returns the information describing the given existential type's conformance to the specified
  /// view, or `nil` if such a conformance was never established.
  public func conformance(of type: ExistentialType, to viewType: ViewType) -> ViewConformance? {
    guard let list = conformanceTables[ExistentialKey(type)] else { return nil }
    return list.first(where: { $0.viewDecl === viewType.decl })
  }

  /// Inserts a new entry into the conformance lookup table for the given type.
  public func insert(conformance: ViewConformance, for type: ExistentialType) {
    conformanceTables[ExistentialKey(type), default: []].append(conformance)
  }

  /// Maps the given generic type to its contextual type, depending on its use site.
  ///
  /// - Parameters:
  ///   - type: A generic type. This method returns `type` unchanged if it does not contain any
  ///     generic type parameter.
  ///   - useSite: The declaration space from which the type is being referred.
  public func contextualize(_ type: ValType, from useSite: DeclSpace) -> ValType {
    // FIXME: A lot of magic will have to happen here to handle associated and dependent types.
    let contextualizer = Contextualizer(env: self, useSite: useSite)
    return contextualizer.walk(type)
  }

  /// Contextualizes the given generic parameter type.
  ///
  /// - Parameter param: A generic parameter type. `param` is assumed to be defined either within
  ///   this generic environment or within one of its parent.
  public func existential(of param: GenericParamType) -> ExistentialType {
    if params.contains(param) {
      return param.context.existentialType(interface: param, genericEnv: self)
    }

    let gds = space.parentDeclSpace!.innermostGenericSpace!
    guard let parentEnv = gds.prepareGenericEnv() else {
      preconditionFailure("bad generic environment")
    }
    return parentEnv.existential(of: param)
  }

}

fileprivate final class Contextualizer: TypeWalker {

  init(env: GenericEnv, useSite: DeclSpace) {
    self.env = env
    self.useSite = useSite
  }

  /// The generic environment for which the type walked type is begin contextualized.
  unowned let env: GenericEnv

  /// The space from wich the visited type is being used.
  unowned var useSite: DeclSpace

  /// The substitution table keeping track of the type variables that were used to open each
  /// specific generic type parameter.
  var substitutions: [ExistentialKey: TypeVar] = [:]

  override func willVisit(_ type: ValType) -> TypeWalker.Action {
    guard let param = type as? GenericParamType else {
      return type.props.contains(.hasTypeParams)
        ? .stepInto(type)
        : .stepOver(type)
    }

    guard env.params.contains(param) else {
      let gds = env.space.parentDeclSpace!.innermostGenericSpace!
      guard let parentEnv = gds.prepareGenericEnv() else {
        return .stepOver(param.context.errorType)
      }
      return .stepOver(parentEnv.contextualize(param, from: useSite))
    }

    // Determine whether the generic parameter is being referred to internally or externally.
    let isInternal: Bool
    if useSite is NominalTypeDecl {
      // Members of a nominal type reside directly in its declaration space. Thus, references from
      // the type's own declaration space is internal.
      isInternal = (useSite === env.space) || useSite.isDescendant(of: env.space)
    } else {
      // The body of a function is nested within the function's declaration space. Thus, references
      // from the function's own declaration space are external.
      isInternal = useSite.isDescendant(of: env.space)
    }

    if isInternal {
      // The generic parameter is being referred to internally, so it must be susbstituted by an
      // existential type.
      let existential = env.existential(of: param)
      return .stepOver(existential)
    } else {
      // The generic parameter is being referred to externally, so it must be substituted by a
      // type variable.
      if let variable = substitutions[ExistentialKey(param)] {
        return .stepOver(variable)
      }

      let variable = TypeVar(context: param.context)
      substitutions[ExistentialKey(param)] = variable
      return .stepOver(variable)
    }
  }

}
