import FrontEnd

/// Ends the lifetime of a projection.
public typealias EndProject = RegionExit<Project>

extension Module {

  /// Creates an `end_project` anchored at `site` that ends the projection created by `start`.
  func makeEndProject(_ start: Operand, in f: Function.ID, at site: SourceRange) -> EndProject {
    makeRegionExit(start, in: f, at: site)
  }

}
