import FrontEnd

/// Ends the lifetime of a projection.
public typealias EndProject = RegionExit<Project>

extension Function {

  /// Creates an `end_project` anchored at `site` that ends the projection created by `start`.
  func makeEndProject(_ start: Operand, at site: SourceRange) -> EndProject {
    makeRegionExit(start, at: site)
  }

  /// Creates an `end_project` anchored at `site` that ends the projection created by `start`, inserting it at `p`.
  mutating func makeEndProject(_ start: Operand, at site: SourceRange, insertingAt p: InsertionPoint) -> InstructionID {
    insert(makeEndProject(start, at: site), at: p)
  }

}
