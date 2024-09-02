/// A binding to a pattern that breaks control flow if the scrutinee doesn't match.
public struct ConditionalBindingStmt: Stmt {

  public let site: SourceRange

  /// The conditional binding.
  public let binding: BindingDecl.ID

  /// The branch that to which control flow jumps if the binding fails.
  public let fallback: BraceStmt.ID

  /// Creates an instance with the given properties.
  public init(binding: BindingDecl.ID, fallback: BraceStmt.ID, site: SourceRange) {
    self.site = site
    self.binding = binding
    self.fallback = fallback
  }

}
