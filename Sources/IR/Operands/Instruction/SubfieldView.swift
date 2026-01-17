import FrontEnd

/// Computes the place of storage for a field or sub-field of a record, given the record's place.
///
/// Does not access memory.
public struct SubfieldView: Instruction {

  /// The place of the whole record.
  public private(set) var recordPlace: Operand

  /// The subfield of the whole record whose place is computed.
  public let subfield: RecordPath

  /// The type of the resulting place.
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
    self.recordPlace = base
    self.subfield = subfield
    self.resultType = subfieldType
    self.site = site
  }

  public var result: IR.`Type`? {
    resultType
  }

  public var operands: [Operand] {
    [recordPlace]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    recordPlace = new
  }

}

extension SubfieldView: CustomStringConvertible {

  public var description: String {
    "subfield_view \(recordPlace)\(subfield.isEmpty ? "" : ", ")\(list: subfield)"
  }

}

extension Module {

  /// Creates a `subfield_view` anchored at `site` computing the place of the given `subfield` of
  /// some record at `recordPlace`.
  ///
  /// - Note: `base` is returned unchanged if `elementPath` is empty.
  func makeSubfieldView(
    of recordPlace: Operand, subfield elementPath: RecordPath, in f: Function.ID, at site: SourceRange
  ) -> SubfieldView {
    precondition(self[f].type(of: recordPlace).isPlace)
    let l = AbstractTypeLayout(of: self[f].type(of: recordPlace).ast, definedIn: program)
    let t = l[elementPath].type

    return .init(
      base: recordPlace,
      subfield: elementPath,
      subfieldType: .place(t),
      site: site)
  }

}
