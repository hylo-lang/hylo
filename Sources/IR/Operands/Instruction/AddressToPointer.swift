import FrontEnd

/// Converts an address to a built-in pointer value.
///
/// This instruction doesn't extend the lifetime of its operand. The value of the converted pointer
/// is only valid within the scope of the source address.
public struct AddressToPointer: Instruction {

  /// The address to convert.
  public private(set) var source: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(source: Operand, site: SourceRange) {
    self.source = source
    self.site = site
  }

  public var result: IR.`Type`? {
    .object(BuiltinType.ptr)
  }

  public var operands: [Operand] {
    [source]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    source = new
  }

}

extension Module {

  /// Creates an `address_to_pointer` anchored at `site` that converts `source` to a built-in
  /// pointer value.
  func makeAddressToPointer(_ source: Operand, at site: SourceRange) -> AddressToPointer {
    .init(source: source, site: site)
  }

}
