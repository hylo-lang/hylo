/// A pattern which binds an identifier.
public struct NamePattern: Pattern {

  public static let kind = NodeKind.namePattern

  /// The variable declaration introducing the pattern's name.
  public let decl: NodeID<VarDecl>

  public init(decl: NodeID<VarDecl>) {
    self.decl = decl
  }

}
