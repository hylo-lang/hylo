/// The declaration of a subscript implementation.
public struct SubscriptImplDecl: Decl, SourceRepresentable {

  public struct Introducer: SourceRepresentable {

    public enum Kind {

      case `let`

      case sink

      case `inout`

      case assign

    }

    public var range: SourceRange?

    public var kind: Kind

  }

  public var range: SourceRange?

  /// The introducer of the method.
  public var introducer: Introducer

  /// The body of the subscript, if any.
  public var body: BraceStmt?

}
