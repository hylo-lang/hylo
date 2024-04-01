import FrontEnd

/// Ends the lifetime of a projection.
public typealias EndProjectWitness = RegionExit<ProjectWitness>

extension Module {

  /// Creates an `end_project` anchored at `site` that ends the projection created by `start`.
  func makeEndProjectWitness(_ start: Operand, at site: SourceRange) -> EndProjectWitness {
    makeRegionExit(start, at: site)
  }

}
