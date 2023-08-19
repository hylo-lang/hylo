import Core

/// Projects a the witness of an existential container.
public struct ProjectWitness: RegionEntry {

  public typealias Exit = EndProjectWitness

  /// The type of the projected value.
  public let projection: RemoteType

  /// The operands of the instruction.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(projection: RemoteType, container: Operand, site: SourceRange) {
    self.projection = projection
    self.operands = [container]
    self.site = site
  }

  /// The container whose witness is projected.
  public var container: Operand { operands[0] }

  /// The types of the instruction's results.
  public var result: IR.`Type`? { .address(projection.bareType) }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension ProjectWitness: CustomStringConvertible {

  public var description: String {
    "project_witness [\(projection.access)] \(container)"
  }

}

extension Module {

  /// Creates a `project_witness` anchored at `site` that projects the witness of `container`,
  /// which is an existential container, as an address of type `projection`.
  func makeProjectWitness(
    of container: Operand, as projection: RemoteType, at site: SourceRange
  ) -> ProjectWitness {
    precondition(projection[.isCanonical])
    return .init(projection: projection, container: container, site: site)
  }

}
