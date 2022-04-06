/// A static modifier.
public struct StaticModifier: MemberModifier {

  public var range: SourceRange?

}

extension StaticModifier: CustomStringConvertible {

  public var description: String { "static" }

}
