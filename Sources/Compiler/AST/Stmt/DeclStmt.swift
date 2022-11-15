/// A declaration statement.
public struct DeclStmt: Stmt {

  /// The declaration.
  public let decl: AnyDeclID

  public init(decl: AnyDeclID) {
    self.decl = decl
  }

}
