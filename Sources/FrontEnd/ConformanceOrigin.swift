/// The declaration and site from which an explicit conformance originates.
public struct ConformanceOrigin {

  /// The declaration of the conformance.
  public let source: AnyDeclID

  /// The site at which diagnostics related to the conformance are reported.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  public init<T: ConformanceSource>(_ source: T.ID, at site: SourceRange) {
    self.source = AnyDeclID(source)
    self.site = site
  }

}

extension ConformanceOrigin: Hashable {}
