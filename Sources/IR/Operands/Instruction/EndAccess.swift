import FrontEnd

/// Ends the lifetime of a projection.
public typealias EndAccess = RegionExit<Access>

extension Function {

  /// Creates an `end_access` anchored at `site` that ends the projection created by `start`.
  func makeEndAccess(_ start: Operand, at site: SourceRange) -> EndAccess {
    makeRegionExit(start, at: site)
  }

  /// Creates an `end_access` anchored at `site` that ends the projection created by `start`, inserting it at `p`.
  mutating func makeEndAccess(_ start: Operand, at site: SourceRange, insertingAt p: InsertionPoint) -> InstructionID {
    insert(makeEndAccess(start, at: site), at: p)
  }

}
