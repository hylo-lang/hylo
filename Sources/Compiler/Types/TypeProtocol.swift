import Utils

/// A protocol describing the API of a Val type.
public protocol TypeProtocol: Hashable {

  /// A set of flags describing recursive properties.
  var flags: TypeFlags { get }

  /// Apply `transform(_:)` on the types that are part of `self`.
  func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self

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

  /// Returns this type transformed with `transformer`.
  ///
  /// This method visits the structure of the type and calls `transformer` on each type composing
  /// that structure. The result of the call substitutes the visited type. If `transformer` returns
  /// `stepInto(t)`, `t` is visited after the substitution. Otherwise, the method directly moves to
  /// the next type in the structure.
  public func transform(_ transformer: (AnyType) -> TypeTransformAction) -> AnyType {
    switch transformer(AnyType(self)) {
    case .stepInto(let type):
      return type.transformParts(transformer)
    case .stepOver(let type):
      return type
    }
  }

  /// Applies `TypeProtocol.transform(_:)` on the types that are part of `self`.
  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    // Default implementation is the identity.
    self
  }

  /// `self` with all generic parameters replaced by skolems.
  public var skolemized: AnyType {
    func _impl(type: AnyType) -> TypeTransformAction {
      switch type.base {
      case let base as AssociatedTypeType:
        return .stepOver(^SkolemType(quantifying: base))

      case let base as GenericTypeParameterType:
        return .stepOver(^SkolemType(quantifying: base))

      case is AssociatedValueType,
           is GenericValueParameterType:
        fatalError("not implemented")

      default:
        // Nothing to do if `type` isn't parameterized.
        if type[.hasGenericTypeParam] || type[.hasGenericValueParam] {
          return .stepInto(type)
        } else {
          return .stepOver(type)
        }
      }
    }

    return transform(_impl(type:))
  }

}
