/// An identifier.
public struct Identifier: SourceRepresentable {

  public var range: SourceRange?

  /// The value of the identifier
  public var value: String

}

extension Identifier: CustomStringConvertible {

  public var description: String { value }
  
}
