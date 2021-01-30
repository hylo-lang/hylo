import Basic

/// A key in the existential lookup table.
typealias ExistentialKey = HashableBox<ValType, ReferenceHashWitness<ValType>>

/// An environment describing mappings between generic types and existential types.
public final class GenericEnv {

  public init() {
    instantiator = TypeInstantiator(env: self)
  }

  /// The declaration space to which the environment is attached.
  public weak var space: GenericDeclSpace?

  /// The generic signature of the environment.
  public var signature: GenericSignature?

  /// A lookup table that keep tracks of the existential types that have been created.
  fileprivate var existentials: [ExistentialKey: ExistentialType] = [:]

  fileprivate var instantiator: TypeInstantiator!

  /// Maps the given generic type to its contextual type, depending on its use site.
  ///
  /// A generic types has to be instanciated as a contextual types before it can be assigned to an
  /// expression. This consists of substituting either a fresh variable or an existential type for
  /// each generic type parameter, depending on the space from which the type is being used. If a
  /// type parameter is is referred to *within* the generic space that introduces it, it has to be
  /// substituted by an existential type. In contrast, if it is being used *outside* of its
  /// declaration space, then it must be opened as a fresh variable.
  ///
  /// - Parameters:
  ///   - type: A generic type. This method returns `type` unchanged if it does not contain any
  ///     generic type parameter.
  ///   - useSite: The declaration space from which the type is being referred.
  public func instantiate(_ type: ValType, from useSite: DeclSpace) -> ValType {
    // FIXME: A lot of magic will have to happen here to handle associated and dependent types.
    instantiator.useSite = useSite
    return instantiator.walk(type)
  }

}

fileprivate final class TypeInstantiator: TypeWalker {

  init(env: GenericEnv) {
    self.env = env
  }

  /// The generic environment for which the type walked type is begin instantiated.
  unowned let env: GenericEnv

  /// The space from wich the visited type is being used.
  unowned var useSite: DeclSpace!

  private var substitutions: [ExistentialKey: TypeVar] = [:]

  override func willVisit(_ type: ValType) -> TypeWalker.Action {
    guard let param = type as? GenericParamType else {
      return type.props.contains(.hasTypeParams)
        ? .stepInto(type)
        : .stepOver(type)
    }

    guard env.signature!.genericParams.contains(param) else {
      let gds = env.space!.parentDeclSpace!.innermostGenericSpace!
      gds.prepareGenericEnv()
      return .stepOver(gds.genericEnv.instantiate(type, from: useSite))
    }

    if useSite.isDescendant(of: env.space!) {
      // The generic parameter is being referred to internally, so it must be susbstituted by an
      // existential type.
      if let existential = env.existentials[HashableBox(type)] {
        return .stepOver(existential)
      }

      let existential = type.context.existentialType()
      env.existentials[HashableBox(type)] = existential
      return .stepOver(existential)
    } else {
      // The generic parameter is being referred to externally, so it must be substituted by a
      // type variable.
      if let variable = substitutions[HashableBox(type)] {
        return .stepOver(variable)
      }

      let variable = TypeVar(context: type.context)
      substitutions[HashableBox(type)] = variable
      return .stepOver(variable)
    }
  }

}
