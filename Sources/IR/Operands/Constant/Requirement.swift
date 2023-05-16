import Core

/// The identifier of a trait requirement in Val IR.
public struct Requirement: Constant, Hashable {

  /// The trait declaring this requirement.
  public let concept: TraitType

  /// The identifier of this requirement in `concept`.
  public let id: Int

  /// The Val IR type of this instance.
  public var type: LoweredType { .object(OpaqueObjectType()) }

}

extension Requirement: CustomStringConvertible {

  public var description: String {
    "\(concept).\(id)"
  }

}
