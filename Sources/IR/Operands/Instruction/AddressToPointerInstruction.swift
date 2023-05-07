import Core

/// Converts an address to a built-in pointer value.
///
/// This instruction doesn't extend the lifetime of its operand. The value of the converted pointer
/// is only valid within the scope of the source address.
public struct AddressToPointerInstruction: Instruction {

  /// The address to convert.
  public private(set) var source: Operand

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(source: Operand, site: SourceRange) {
    self.source = source
    self.site = site
  }

  public var types: [LoweredType] { [.object(BuiltinType.ptr)] }

  public var operands: [Operand] { [source] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    source = new
  }

}

extension Module {

  /// Creates an `address_to_pointer` anchored at `anchor` that converts `source` to a built-in
  /// pointer value.
  func makeAddressToPointer(
    _ source: Operand,
    anchoredAt anchor: SourceRange
  ) -> AddressToPointerInstruction {
    .init(source: source, site: anchor)
  }

}
