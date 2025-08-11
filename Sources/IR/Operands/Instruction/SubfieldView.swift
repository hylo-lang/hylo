import FrontEnd

/// Computes the address of storage for a field or sub-field of a record, given the record's address.
///
/// Does not access memory.
public struct SubfieldView: Instruction {

  /// The address of the whole record.
  public private(set) var recordAddress: Operand

  /// The subfield of the whole record whose address is computed.
  public let subfield: RecordPath

  /// The type of the resulting address.
  public let resultType: IR.`Type`

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    base: Operand,
    subfield: RecordPath,
    subfieldType: IR.`Type`,
    site: SourceRange
  ) {
    self.recordAddress = base
    self.subfield = subfield
    self.resultType = subfieldType
    self.site = site
  }

  public var result: IR.`Type`? {
    resultType
  }

  public var operands: [Operand] {
    [recordAddress]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    recordAddress = new
  }

}

extension SubfieldView: CustomStringConvertible {

  public var description: String {
    "subfield_view \(recordAddress)\(subfield.isEmpty ? "" : ", ")\(list: subfield)"
  }

}

extension Module {

  /// Creates a `subfield_view` anchored at `site` computing the address of the given `subfield` of
  /// some record at `recordAddress`.
  ///
  /// - Note: `base` is returned unchanged if `elementPath` is empty.
  func makeSubfieldView(
    of recordAddress: Operand, subfield elementPath: RecordPath, in f: Function.ID, at site: SourceRange
  ) -> SubfieldView {
    precondition(type(of: recordAddress, in: f).isAddress)
    let l = AbstractTypeLayout(of: self.type(of: recordAddress, in: f).ast, definedIn: program)
    let t = l[elementPath].type

    return .init(
      base: recordAddress,
      subfield: elementPath,
      subfieldType: .address(t),
      site: site)
  }

}
