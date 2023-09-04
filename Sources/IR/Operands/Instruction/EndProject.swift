import Core

/// Ends the lifetime of a projection.
public typealias EndProject = RegionExit<Project>

extension Module {

  /// Creates an `end_project` anchored at `site` that ends the projection created by `start`.
  func makeEndProject(_ start: Operand, at site: SourceRange) -> EndProject {
    makeRegionExit(start, at: site)
  }

}
