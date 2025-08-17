import FrontEnd

/// Ends the lifetime of a projection.
public typealias CloseUnion = RegionExit<OpenUnion>

extension Function {

  /// Creates an `close_union` anchored at `site` that ends an access to the payload of a union
  /// opened previously by `start`.
  func makeCloseUnion(_ start: Operand, at site: SourceRange) -> CloseUnion {
    makeRegionExit(start, at: site)
  }

  /// Creates an `close_union` anchored at `site` that ends an access to the payload of a union
  /// opened previously by `start`, inserting it at `p`.
  mutating func makeCloseUnion(
    _ start: Operand, at site: SourceRange, insertingAt p: InsertionPoint
  ) -> InstructionID {
    insert(makeCloseUnion(start, at: site), at: p)
  }

}
