/// A pattern which binds an identifier.
public struct NamePattern: Pattern {

  public static let kind = NodeKind.namePattern

  /// The variable declaration introducing the pattern's name
  public var decl: NodeID<VarDecl>

}
