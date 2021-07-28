import Basic

/// A key in a conformance lookup table.
fileprivate typealias SkolemKey = HashableBox<SkolemType, ReferenceHashWitness<SkolemType>>

/// An environment describing mappings between generic types and skolem types.
///
/// Generic types have to be "contextualized" before they can be assigned to an expression. This
/// consists of substituting either fresh or skolem type variable for each of their generic type
/// parameters, depending on the declaration space from which they are being used. If a parameter
/// is being used *within* the generic space that declares it, it is substituted by a skolem. In
/// contrast, if it is used *outside* of its environment space, then it must be be "opened" as a
/// fresh type variable.
public final class GenericEnv {

  /// A prototype of a constraint on an opened generic parameter, described by a type requirement.
  public struct ConstraintPrototype {

    public enum Kind {

      /// The two types must be equal.
      case equality

      /// The left side must conform to the right side.
      case conformance

    }

    /// The kind of contraint described by the prototype.
    public let kind: Kind

    /// The constraint's left operand.
    public let lhs: ValType

    /// The constraint's right operand.
    public let rhs: ValType

    fileprivate func contextualized(with contextualizer: Contextualizer) -> ConstraintPrototype {
      return ConstraintPrototype(
        kind: kind, lhs: contextualizer.walk(lhs), rhs: contextualizer.walk(rhs))
    }

    public static func equality(lhs: ValType, rhs: ValType) -> ConstraintPrototype {
      ConstraintPrototype(kind: .equality, lhs: lhs, rhs: rhs)
    }

    public static func conformance(lhs: ValType, rhs: ViewType) -> ConstraintPrototype {
      ConstraintPrototype(kind: .conformance, lhs: lhs, rhs: rhs)
    }

  }

  /// The declaration space to which the environment is attached.
  public unowned let space: GenericDeclSpace

  /// The generic parameters of the environment.
  public let params: [GenericParamType]

  /// The type requirements on the parameters of the environment.
  public let typeReqs: [TypeReq]

  /// The prototypes of the constraints to apply on opened parameters.
  ///
  /// - Note: This property is initialized by the type checker.
  /// - SeeAlso: `Context.prepareGenericEnv`
  public var constraintPrototypes: [ConstraintPrototype] = []

  /// The equivalence classes of the environment.
  ///
  /// This describes the equivalence classes of the set of generic type parameters, as inferred
  /// from related type requirements. This is used by the constraint type solver to decide whether
  /// two skolems are equivalent.
  ///
  /// - Note: This property is initialized by the type checker.
  /// - SeeAlso: `Context.prepareGenericEnv`
  public var equivalences = EquivalenceClassSet()

  /// The known conformances of the environment's generic type parameters.
  ///
  /// - Note: This property is initialized by the type checker.
  /// - SeeAlso: `Context.prepareGenericEnv`
  private var conformanceTables: [SkolemKey: [ViewConformance]] = [:]

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

    // Initialize semantic properties.
    let prepare = context.prepareGenericEnv
    precondition(prepare != nil, "no generic environment delegate")
    guard prepare!(self) else { return nil }
  }

  /// The set of conformances for the given skolem type.
  public func conformances(of type: SkolemType) -> [ViewConformance]? {
    return conformanceTables[SkolemKey(type)]
  }

  /// Returns the information describing the given skolem type's conformance to the specified view,
  /// or `nil` if such a conformance was never established.
  public func conformance(of type: SkolemType, to viewType: ViewType) -> ViewConformance? {
    guard let list = conformanceTables[SkolemKey(type)] else { return nil }
    return list.first(where: { $0.viewDecl === viewType.decl })
  }

  /// Inserts a new entry into the conformance lookup table for the given type.
  public func insert(conformance: ViewConformance, for type: SkolemType) {
    conformanceTables[SkolemKey(type), default: []].append(conformance)
  }

  /// Returns whether the given declaration space is contained within this generic environment.
  ///
  /// - Parameter useSite: A declaration space.
  public func isContained(useSite: DeclSpace) -> Bool {
    // Determine whether the generic parameter is being referred to internally or externally.
    if useSite is NominalTypeDecl {
      // Members of a nominal type reside directly in its declaration space. Thus, references from
      // the type's own declaration space is internal.
      return (useSite === space) || useSite.isDescendant(of: space)
    } else {
      // The body of a function is nested within the function's declaration space. Thus, references
      // from the function's own declaration space are external.
      return useSite.isDescendant(of: space)
    }
  }

  /// Maps the given generic type to its contextual type, depending on its use site.
  ///
  /// - Parameters:
  ///   - type: A generic type. This method returns `type` unchanged if it does not contain any
  ///     generic type parameter.
  ///   - useSite: The declaration space from which the type is being referred.
  ///   - handleConstraint: A closure that accepts contextualized contraint prototypes. It is
  ///     called only if the contextualized type contains opened generic types for which there
  ///     exist type requirements.
  public func contextualize(
    _ type: ValType,
    from useSite: DeclSpace,
    processingContraintsWith handleConstraint: (ConstraintPrototype) -> Void = { _ in }
  ) -> ValType {
    // FIXME: A lot of magic will have to happen here to handle associated and dependent types.

    // Contextualize the type.
    let contextualizer = Contextualizer(env: self, useSite: useSite)
    let newType = contextualizer.walk(type)

    // Contextualize the type constraint prototypes for each opened parameter.
    contextualizeTypeReqs(with: contextualizer, processingContraintsWith: handleConstraint)

    return newType
  }

  /// Contextualize the environment's type constraint prototypes with the given contextualizer.
  ///
  /// - Parameter contextualizer: The contextualizer that was used to open generic type parameters.
  fileprivate func contextualizeTypeReqs(
    with contextualizer: Contextualizer,
    processingContraintsWith handleConstraint: (ConstraintPrototype) -> Void
  ) {
    var next: GenericEnv? = self
    while let env = next {
      guard !env.isContained(useSite: contextualizer.useSite) else { break }
      env.constraintPrototypes.forEach({ prototype in
        handleConstraint(prototype.contextualized(with: contextualizer))
      })
      next = env.space.parentDeclSpace?.innermostGenericSpace?.prepareGenericEnv()
    }
  }

  /// Contextualizes the given generic parameter type.
  ///
  /// - Parameter param: A generic parameter type. `param` is assumed to be defined either within
  ///   this generic environment or within one of its parent.
  public func skolemize(_ param: GenericParamType) -> SkolemType {
    if params.contains(param) {
      return param.context.skolemType(interface: param, genericEnv: self)
    }

    let gds = space.parentDeclSpace!.innermostGenericSpace!
    guard let parentEnv = gds.prepareGenericEnv() else {
      preconditionFailure("bad generic environment")
    }
    return parentEnv.skolemize(param)
  }

}

fileprivate final class Contextualizer: TypeWalker {

  /// The generic environment for which the type walked type is begin contextualized.
  unowned var env: GenericEnv

  /// The space from wich the visited type is being used.
  unowned var useSite: DeclSpace

  /// The substitution table keeping track of the type variables that were used to open each
  /// specific generic type parameter.
  var substitutions: [GenericParamType: TypeVar] = [:]

  init(env: GenericEnv, useSite: DeclSpace) {
    self.env = env
    self.useSite = useSite
  }

  override func willVisit(_ type: ValType) -> TypeWalker.Action {
    guard let param = type as? GenericParamType else {
      return type.hasTypeParams
        ? .stepInto(type)
        : .stepOver(type)
    }

    // Search the environment that defines the current generic parameter.
    var definingEnv = env
    while !definingEnv.params.contains(param) {
      let parentDeclSpace = definingEnv.space.parentDeclSpace
      let parentGenericSpace = parentDeclSpace?.innermostGenericSpace
      guard let parentEnv = parentGenericSpace?.prepareGenericEnv() else {
        return .stepOver(param.context.errorType)
      }
      definingEnv = parentEnv
    }

    if definingEnv.isContained(useSite: useSite) {
      // The generic parameter is being referred to internally, so it must be susbstituted by a
      // skolem type.
      let skolem = definingEnv.skolemize(param)
      return .stepOver(skolem)
    } else {
      // The generic parameter is being referred to externally, so it must be substituted by a
      // fresh type variable.
      if let variable = substitutions[param] {
        return .stepOver(variable)
      }

      let variable = TypeVar(context: param.context)
      substitutions[param] = variable
      return .stepOver(variable)
    }
  }

}
