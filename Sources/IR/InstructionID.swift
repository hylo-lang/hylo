import Utils

/// The stable identity of an instruction in its function.
///
/// - SeeAlso: `AbsoluteInstructionID`
public struct InstructionID: Hashable {

  /// The identity of the instruction in its block.
  public let address: Function.Instructions.Address

  /// Creates an instance with the given properties.
  public init(
    _ address: Function.Instructions.Address
  ) {
    self.address = address
  }

  /// Creates an instance from an absolute instruction ID.
  public init(_ i: AbsoluteInstructionID) {
    self.address = i.address
  }

  /// Creates an instance from an absolute instruction ID.
  public init?(_ i: AbsoluteInstructionID?) {
    guard let i = i else { return nil }
    self.address = i.address
  }

}

extension InstructionID: CustomStringConvertible {

  public var description: String { "i\(address)" }

}
