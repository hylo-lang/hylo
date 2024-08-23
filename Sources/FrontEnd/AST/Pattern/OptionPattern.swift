/// A pattern that matches the presence of a value in an optional.
public struct OptionPattern: Pattern {

  public let site: SourceRange

  /// The expression of the pattern.
  public let name: NamePattern.ID

  public init(name: NamePattern.ID, site: SourceRange) {
    self.site = site
    self.name = name
  }

}
