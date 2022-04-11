/// The declaration of a conformance.
public struct ConformanceDecl: Decl, LexicalScope {

  public static let kind = NodeKind.conformanceDecl

  /// The expression of the conforming type.
  public var subject: AnyTypeExprIndex

  /// The names of traits to which conformance is declared.
  public var conformances: [NodeIndex<NameTypeExpr>]

  /// The condition of the conformance, if any.
  public var whereClause: SourceRepresentable<WhereClause>?

  /// The member declarations in the lexical scope of the conformance.
  public var members: [AnyDeclIndex]

}
