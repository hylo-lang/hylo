/// A wildcard pattern.
public struct WildcardPattern: Pattern, Sendable {

  public let site: SourceRange

  public init(site: SourceRange) {
    self.site = site
  }

}
