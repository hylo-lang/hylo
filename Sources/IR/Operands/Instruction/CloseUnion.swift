import Core

/// Ends the lifetime of a projection.
public typealias CloseUnion = RegionExit<OpenUnion>

extension Module {

  /// Creates an `close_union` anchored at `site` that ends an access to the payload of a union
  /// opened previously by `start`.
  func makeCloseUnion(_ start: Operand, at site: SourceRange) -> CloseUnion {
    makeRegionExit(start, at: site)
  }

}
