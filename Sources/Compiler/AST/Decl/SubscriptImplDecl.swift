/// The declaration of a subscript implementation.
public struct SubscriptImplDecl: Decl {

  public enum Introducer: Hashable {

    case `let`

    case sink

    case `inout`

    case assign

  }

  /// The introducer of the method.
  public var introducer: SourceRepresentable<Introducer>

  /// The body of the subscript, if any.
  public var body: NodeIndex<BraceStmt>?

}
