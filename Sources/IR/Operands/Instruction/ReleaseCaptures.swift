import FrontEnd
import Utils

/// Releases accesses captures in an allocation.
public struct ReleaseCaptures: Instruction {

  /// The operands of the instruction.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(container: Operand, site: SourceRange) {
    self.operands = [container]
    self.site = site
  }

  /// The storage containing the captures to release.
  public var container: Operand { operands[0] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension Module {

  /// Creates a `release_capture` anchored at `site` that releases the accesses previously captured
  /// in `container`.
  func makeReleaseCapture(_ container: Operand, at site: SourceRange) -> ReleaseCaptures {
    precondition(container.instruction.map({ self[$0] is AllocStack }) ?? false)
    return .init(container: container, site: site)
  }

}
