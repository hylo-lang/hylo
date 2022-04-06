/// An access modifier.
public struct AccessModifier: SourceRepresentable {

  public enum Kind {

    /// Denotes a public declaration.
    case `public`

  }

  public var range: SourceRange?

  /// The kind of the modifier.
  public var kind: Kind

}

extension AccessModifier: CustomStringConvertible {

  public var description: String { "\(kind)" }

}
