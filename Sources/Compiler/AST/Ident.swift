/// An identifier referring to a type or value declaration.
public struct Ident {

  public var range: SourceRange?

  public var name: String

  public init(name: String, range: SourceRange? = nil) {
    self.name = name
    self.range = range
  }

}

extension Ident: CustomStringConvertible {

  public var description: String { name }

}
