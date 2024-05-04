import FrontEnd

/// Ends the lifetime of a projection.
public typealias EndAccess = RegionExit<Access>

extension Module {

  /// Creates an `end_access` anchored at `site` that ends the projection created by `start`.
  func makeEndAccess(_ start: Operand, at site: SourceRange) -> EndAccess {
    makeRegionExit(start, at: site)
  }

}
