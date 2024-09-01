/// The type of the value stored in an existential type.
public struct WitnessType: TypeProtocol {

  /// The existential type wrapping this witness.
  public let container: ExistentialType

  /// Creates the witness type of `e`.
  public init(of e: ExistentialType) {
    self.container = e
  }

  /// A set of flags describing recursive properties.
  public var flags: ValueFlags { container.flags }

  /// Apply `transform(_:_:)` on `m` and the types that are part of `self`.
  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    WitnessType(of: container.transformParts(mutating: &m, transformer))
  }

}

extension WitnessType: CustomStringConvertible {

  public var description: String {
    "witness of (\(container)"
  }

}
