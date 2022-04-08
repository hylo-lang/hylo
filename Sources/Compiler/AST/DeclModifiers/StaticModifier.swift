/// A static modifier.
public struct StaticModifier: MemberModifier {}

extension StaticModifier: CustomStringConvertible {

  public var description: String { "static" }

}
