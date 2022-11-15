/// A break statement.
public struct CondBindingStmt: Stmt {

  public enum Fallback: Codable {

    case expr(AnyExprID)

    case exit(AnyStmtID)

  }

  /// The conditional binding.
  public let binding: NodeID<BindingDecl>

  /// The fallback expression or statement.
  public let fallback: Fallback

  public init(binding: NodeID<BindingDecl>, fallback: Fallback) {
    self.binding = binding
    self.fallback = fallback
  }

}
