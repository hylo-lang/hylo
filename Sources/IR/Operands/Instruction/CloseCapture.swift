import FrontEnd

/// Ends the lifetime of a projection.
public typealias CloseCapture = RegionExit<OpenCapture>

extension Module {

  /// Creates a `close_capture` anchored at `site` that ends the exposition of a captured access.
  func makeCloseCapture(_ start: Operand, in f: Function.ID, at site: SourceRange) -> CloseCapture {
    makeRegionExit(start, in: f, at: site)
  }

}
