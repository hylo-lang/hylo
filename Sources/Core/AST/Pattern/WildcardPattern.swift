/// A wildcard pattern.
public struct WildcardPattern: Pattern {

  public let site: SourceRange

  public init(site: SourceRange) {
    self.site = site
  }

}
