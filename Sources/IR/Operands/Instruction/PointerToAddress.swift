import FrontEnd

/// Converts a built-in pointer value to an address.
///
/// This instruction doesn't extend the lifetime of its operand. The address unsafely refers to the
/// memory referenced by the pointer.
public struct PointerToAddress: Instruction {

  /// The pointer to convert.
  public private(set) var source: Operand

  /// The type of the converted address.
  public let target: RemoteType

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(source: Operand, target: RemoteType, site: SourceRange) {
    self.source = source
    self.target = target
    self.site = site
  }

  public var result: IR.`Type`? {
    .address(target.bareType)
  }

  public var operands: [Operand] {
    [source]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    source = new
  }

}

extension PointerToAddress: CustomStringConvertible {

  public var description: String {
    "pointer_to_address \(source) as \(target)"
  }

}

extension Module {

  /// Creates a `pointer_to_address` anchored at `site` that converts `source`, which is a
  /// built-in pointer value, to an address of type `target`.
  func makePointerToAddress(
    _ source: Operand, to target: RemoteType, at site: SourceRange
  ) -> PointerToAddress {
    precondition(target.access != .yielded)
    return .init(source: source, target: target, site: site)
  }

}
