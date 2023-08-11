import Utils

/// A protocol describing the API of a Hylo type.
public protocol TypeProtocol: Hashable {

  /// A set of flags describing recursive properties.
  var flags: TypeFlags { get }

  /// Apply `transform(_:_:)` on `m` and the types that are part of `self`.
  func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self

}

extension TypeProtocol {

  /// Creates an instance with the value of `container.base` or returns `nil` if that value has
  /// a different type.
  public init?(_ container: AnyType) {
    if let t = container.base as? Self {
      self = t
    } else {
      return nil
    }
  }

  /// Creates an instance with the value of `container.base` or returns `nil` if either that value
  /// has a different type or `container` is `nil`.
  public init?(_ container: AnyType?) {
    if let t = container.flatMap(Self.init(_:)) {
      self = t
    } else {
      return nil
    }
  }

  /// Returns whether the specified flags are raised on this type.
  public subscript(fs: TypeFlags) -> Bool { flags.contains(fs) }

  /// Returns this type transformed with `transformer` applied to `m`.
  ///
  /// This method visits the structure of the type and calls `transformer` on `m` and each type
  /// composing that structure. The result of the call is substituted for the visited type. If
  /// `transformer` returns `stepInto(t)`, `t` is visited after the substitution. Otherwise, the
  /// method directly moves to the next type in the structure.
  public func transform<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> AnyType {
    switch transformer(&m, AnyType(self)) {
    case .stepInto(let type):
      return type.transformParts(mutating: &m, transformer)
    case .stepOver(let type):
      return type
    }
  }

  /// Applies `TypeProtocol.transform(mutating:_:)` on `m` and the types that are part of `self`.
  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    // Default implementation is the identity.
    self
  }

  /// Returns this type transformed with `transformer`.
  ///
  /// This method visits the structure of the type and calls `transformer` on each type composing
  /// that structure. The result of the call is substituted for the visited type. If `transformer`
  /// returns `stepInto(t)`, `t` is visited after the substitution. Otherwise, the method directly
  /// moves to the next type in the structure.
  public func transform(_ transformer: (AnyType) -> TypeTransformAction) -> AnyType {
    var ignored: Void = ()
    return transform(mutating: &ignored) { (_, t) in transformer(t) }
  }

  /// Applies `TypeProtocol.transformParts(mutating:_:)` on a discarded value and the types that are
  /// part of `self`.
  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    var ignored: Void = ()
    return transformParts(mutating: &ignored) { (_, t) in transformer(t) }
  }

  /// `self` with all generic parameters replaced by skolems.
  public var skolemized: AnyType {
    func _impl(type: AnyType) -> TypeTransformAction {
      switch type.base {
      case let base as AssociatedTypeType:
        return .stepOver(^SkolemType(quantifying: base))

      case let base as GenericTypeParameterType:
        return .stepOver(^SkolemType(quantifying: base))

      case is AssociatedValueType:
        fatalError("not implemented")

      default:
        // Nothing to do if `type` isn't parameterized.
        if type[.hasGenericTypeParameter] || type[.hasGenericValueParameter] {
          return .stepInto(type)
        } else {
          return .stepOver(type)
        }
      }
    }

    return transform(_impl(type:))
  }

}
