/// A break statement.
public struct CondBindingStmt: Stmt {

  public enum Fallback: Codable {

    case expr(AnyExprID)

    case exit(AnyStmtID)

  }

  public let origin: SourceRange

  /// The conditional binding.
  public let binding: NodeID<BindingDecl>

  /// The fallback expression or statement.
  public let fallback: Fallback

  public init(binding: NodeID<BindingDecl>, fallback: Fallback, origin: SourceRange) {
    self.origin = origin
    self.binding = binding
    self.fallback = fallback
  }

}
