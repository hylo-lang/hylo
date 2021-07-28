import Basic

/// An identifier referring to a type or value declaration.
public struct Ident {

  public var range: SourceRange

  public var name: String

  public init(name: String, range: SourceRange) {
    self.name = name
    self.range = range
  }

}
