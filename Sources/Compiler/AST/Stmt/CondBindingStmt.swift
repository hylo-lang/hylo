/// A break statement.
public struct CondBindingStmt: Stmt {

  public static let kind = NodeKind.condBindingStmt

  public enum Fallback: Codable {

    case expr(AnyExprID)

    case exit(AnyStmtID)

  }

  /// The conditional binding.
  public var binding: NodeID<BindingDecl>

  /// The fallback expression or statement.
  public var fallback: Fallback

  public init(binding: NodeID<BindingDecl>, fallback: Fallback) {
    self.binding = binding
    self.fallback = fallback
  }

}
