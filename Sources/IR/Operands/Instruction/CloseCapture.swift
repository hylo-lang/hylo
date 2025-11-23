import FrontEnd

/// Ends the lifetime of a projection.
public typealias CloseCapture = RegionExit<OpenCapture>

extension Function {

  /// Creates a `close_capture` anchored at `site` that ends the exposition of a captured access.
  func makeCloseCapture(_ start: Operand, at site: SourceRange) -> CloseCapture {
    makeRegionExit(start, at: site)
  }

}
