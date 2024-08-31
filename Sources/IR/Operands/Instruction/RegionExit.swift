import FrontEnd

/// An instruction marking an exit from a region.
public struct RegionExit<Entry: RegionEntry> {

  /// The operands of the instruction.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(start: Operand, site: SourceRange) {
    self.operands = [start]
    self.site = site
  }

  /// The exited region.
  public var start: Operand { operands[0] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension RegionExit: CustomStringConvertible {

  public var description: String {
    switch Entry.self {
    case let s where s == Access.self:
      return "end_access \(start)"
    case let s where s == OpenCapture.self:
      return "close_capture \(start)"
    case let s where s == OpenUnion.self:
      return "close_union \(start)"
    case let s where s == Project.self:
      return "end_project \(start)"
    default:
      return "end_region \(start)"
    }
  }

}

extension Module {

  /// Creates a region exit anchored at `site` marking an exit of the regions started by `start`.
  func makeRegionExit<Entry: RegionEntry>(
    _ start: Operand, at anchor: SourceRange
  ) -> RegionExit<Entry> {
    precondition(start.instruction.map({ self[$0] is Entry }) ?? false)
    return .init(start: start, site: anchor)
  }

}
