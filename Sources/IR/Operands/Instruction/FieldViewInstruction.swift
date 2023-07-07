import Core

/// Computes the address of storage for a field or sub-field of a record, given the record's address.
///
/// Does not access memory.
public struct FieldViewInstruction: Instruction {

  /// The address of the whole record.
  public private(set) var recordAddress: Operand

  /// A sequence of indices identifying a part of the value at `base`.
  public let subfieldPath: SubfieldPath

  /// The type of the derived address.
  public let subfieldType: LoweredType

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    base: Operand,
    subfield: SubfieldPath,
    subfieldType: LoweredType,
    site: SourceRange
  ) {
    self.recordAddress = base
    self.subfieldPath = subfield
    self.subfieldType = subfieldType
    self.site = site
  }

  public var types: [LoweredType] { [subfieldType] }

  public var operands: [Operand] { [recordAddress] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    recordAddress = new
  }

}

extension FieldViewInstruction: CustomStringConvertible {

  public var description: String {
    "field_view \(recordAddress)\(subfieldPath.isEmpty ? "" : ", ")\(list: subfieldPath)"
  }

}

extension Module {

  /// Creates a `field_view` anchored at `site` computing the address of the 
  /// given `subfield` of some record at `recordAddress`.
  /// - Note: `base` is returned unchanged if `elementPath` is empty.
  func makeFieldView(
    of recordAddress: Operand, subfield elementPath: SubfieldPath, at site: SourceRange
  ) -> FieldViewInstruction {
    precondition(type(of: recordAddress).isAddress)
    let l = AbstractTypeLayout(of: type(of: recordAddress).ast, definedIn: program)
    return .init(
      base: recordAddress,
      subfield: elementPath,
      subfieldType: .address(l[elementPath].type),
      site: site)
  }

}
