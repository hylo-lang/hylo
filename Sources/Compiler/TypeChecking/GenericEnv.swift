import Utils

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

    fileprivate func contextualized(
      with contextualizer: inout Contextualizer
    ) -> ConstraintPrototype {
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
  /// This property is used during type checking to determine whether two skolems and/or associated
  /// types are equivalent. The equivalence classes are inferred from the type requirements of the
  /// environment.
  ///
  /// - Note: This property is initialized by the type checker.
  /// - SeeAlso: `Context.prepareGenericEnv`
  public var equivalences = EquivalenceClassSet()

  /// The known conformances of the environment's generic type parameters.
  ///
  /// - Note: This property is initialized by the type checker.
  /// - SeeAlso: `Context.prepareGenericEnv`
  private var conformanceTables: [ReferenceBox<ValType>: [ViewConformance]] = [:]

  public init(space: GenericDeclSpace) {
    self.space = space
    self.params = []
    self.typeReqs = []
  }

  public init?(
    space: GenericDeclSpace,
    params: [GenericParamType],
    typeReqs: [TypeReq],
    context: Compiler
  ) {
    self.space = space
    self.params = params
    self.typeReqs = typeReqs

    // Initialize semantic properties.
    guard TypeChecker.prepare(genericEnv: self) else { return nil }
  }

  /// The set of conformances registered in this environment for the given type.
  ///
  /// - Parameter type: A generic type parameter, or an associated type rooted at a parameter.
  public func conformances(of type: ValType) -> [ViewConformance]? {
    return conformanceTables[ReferenceBox(type)]
  }

  /// Returns the conformance registered in this environment for the given type to the given view,
  /// or `nil` if such a conformance was never established.
  ///
  /// - Parameters:
  ///   - type: A generic type parameter, or an associated type rooted at a parameter.
  ///   - view: The view to which `type` is supposed to conform.
  public func conformance(of type: ValType, to view: ViewType) -> ViewConformance? {
    guard let list = conformanceTables[ReferenceBox(type)] else { return nil }
    return list.first(where: { $0.viewDecl === view.decl })
  }

  /// Inserts a new entry into the conformance lookup table for the given type.
  ///
  /// - Parameters:
  ///   - conformance: A view conformance.
  ///   - type: A generic type parameter, or an associated type rooted at a parameter.
  public func insert(conformance: ViewConformance, for type: ValType) {
    assert(type is GenericParamType || (type as? AssocType)?.root is GenericParamType)
    conformanceTables[ReferenceBox(type), default: []].append(conformance)
  }

  /// Returns whether the given declaration space is contained within this generic environment.
  ///
  /// - Parameter useSite: A declaration space.
  public func contains(useSite: DeclSpace) -> Bool {
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

  /// Returns whether the given type is rooted in this environment.
  ///
  /// - Parameter type: A type.
  /// - Returns: `true` if `type` is either a generic parameter defined by this environment, or
  ///   an associated type whose root is defined in this environment. Otherwise, `false`.
  public func defines(type: ValType) -> Bool {
    switch type {
    case let type as GenericParamType:
      return params.contains(type)
    case let type as AssocType:
      return defines(type: type.base)
    default:
      return false
    }
  }

  /// Maps the given generic type to its contextual type, depending on its use site.
  ///
  /// - Parameters:
  ///   - type: A (presumably generic) type. `type` is returned unchanged if it does not contain
  ///     any generic type parameter.
  ///   - useSite: The declaration space from which `type` is being referred.
  ///   - handleConstraint: A closure that accepts contextualized contraint prototypes generated
  ///     for each opened generic associated with type requirements.
  /// - Returns: `(contextualType, openedParams)` where `contextualType` is the contextualized type
  ///   and `openedParams` is a table mapping opened generic type parameters to the corresponding
  ///   type variable.
  public func contextualize(
    _ type: ValType,
    from useSite: DeclSpace,
    processingContraintsWith handleConstraint: (ConstraintPrototype) -> Void = { _ in }
  ) -> (contextualType: ValType, openedParams: [GenericParamType: TypeVar]) {
    // Contextualize the type.
    var contextualizer = Contextualizer(env: self, useSite: useSite)
    let contextualType = contextualizer.walk(type)

    // Contextualize the type constraint prototypes for each opened parameter.
    contextualizeTypeReqs(with: &contextualizer, processingContraintsWith: handleConstraint)

    return (contextualType: contextualType, openedParams: contextualizer.substitutions)
  }

  /// Contextualizes the environment's type constraint prototypes with the given contextualizer.
  ///
  /// - Parameter contextualizer: The contextualizer that was used to open generic type parameters.
  fileprivate func contextualizeTypeReqs(
    with contextualizer: inout Contextualizer,
    processingContraintsWith handleConstraint: (ConstraintPrototype) -> Void
  ) {
    var next: GenericEnv? = self
    while let env = next {
      guard !env.contains(useSite: contextualizer.useSite) else { break }
      env.constraintPrototypes.forEach({ prototype in
        handleConstraint(prototype.contextualized(with: &contextualizer))
      })
      next = env.space.parentDeclSpace?.innermostGenericSpace?.prepareGenericEnv()
    }
  }

  /// Skolemizes the given generic parameter type in this environment.
  ///
  /// - Precondition: `param` is assumed to be defined either within this generic environment or
  ///   within one of its parent.
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

  /// Skolemizes the generic parameter types that occur in `type` in this environment.
  ///
  /// - Precondition: All generic parameter types are assumed to be defined either within this
  ///   generic environment or within one of its parent.
  public func skolemize(paramsIn type: ValType) -> ValType {
    var skolemizer = Skolemizer(env: self)
    return skolemizer.walk(type)
  }

}

fileprivate final class Contextualizer: TypeWalker {

  var parent: ValType?

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

  func willVisit(_ type: ValType) -> TypeWalkerAction {
    switch type {
    case let type as GenericParamType:
      return .stepOver(contextualize(param: type))

    default:
      return type[.hasTypeParams]
        ? .stepInto(type)
        : .stepOver(type)
    }
  }

  private func contextualize(param: GenericParamType) -> ValType {
    guard let definingEnv = param.genericEnv else {
      // Not sure that's really unreachable.
      fatalError("unreachable")
    }

    // FIXME: Should we contextualize abstract type declarations as associated types here?

    if definingEnv.contains(useSite: useSite) {
      // The reference is internal; the parameter must be subsituted by a skolem.
      return definingEnv.skolemize(param)
    } else {
      // The reference is external; the parameter must be subsituted by a fresh type variable.
      if let variable = substitutions[param] {
        return variable
      }

      let variable = TypeVar(context: param.context)
      substitutions[param] = variable
      return variable
    }
  }

}

struct Skolemizer: TypeWalker {

  var parent: ValType?

  /// The generic environment for which the type walked type is begin skolemized.
  unowned var env: GenericEnv

  init(env: GenericEnv) {
    self.env = env
  }

  func willVisit(_ type: ValType) -> TypeWalkerAction {
    switch type {
    case let type as GenericParamType:
      return .stepOver(env.skolemize(type))

    default:
      return type[.hasTypeParams]
        ? .stepInto(type)
        : .stepOver(type)
    }
  }

}
