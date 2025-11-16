import FrontEnd
import Utils

/// Releases accesses captures in an allocation.
public struct ReleaseCaptures: Instruction {

  /// The operands of the instruction.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  init(container: Operand, site: SourceRange) {
    self.operands = [container]
    self.site = site
  }

  /// The storage containing the captures to release.
  public var container: Operand { operands[0] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}
