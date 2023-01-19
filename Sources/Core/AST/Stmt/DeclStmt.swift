/// A declaration statement.
public struct DeclStmt: Stmt {

  public let origin: SourceRange

  /// The declaration.
  public let decl: AnyDeclID

  public init(decl: AnyDeclID, origin: SourceRange) {
    self.origin = origin
    self.decl = decl
  }

}
