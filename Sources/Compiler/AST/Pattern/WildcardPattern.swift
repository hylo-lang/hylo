/// A wildcard pattern.
public struct WildcardPattern: Pattern {

  public let origin: SourceRange?

  public init(origin: SourceRange?) { self.origin = origin }

}
