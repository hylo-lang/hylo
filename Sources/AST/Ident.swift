import Basic

/// An identifier referring to a type or value declaration.
public final class Ident {

  public init(name: String, range: SourceRange) {
    self.name = name
    self.range = range
  }

  public var name: String

  public var range: SourceRange

}
