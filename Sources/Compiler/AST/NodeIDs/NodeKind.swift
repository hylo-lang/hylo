import Utils

/// An identifier uniquely identifying the type of a node referred by an ID.
public struct NodeKind: Hashable {

  private var rawValue: Int32

  private init(_ rawValue: Int32) {
    self.rawValue = rawValue
  }

  public static func <= (l: Self, r: Self) -> Bool {
    l.rawValue & r.rawValue == r.rawValue
  }

  // MARK: Declarations

  public static let decl                      = NodeKind(1 << 0)
  public static let typeDecl                  = NodeKind(1 << 6 | decl.rawValue)

  public static let associatedSizeDecl        = NodeKind(1 << 10 | decl.rawValue)
  public static let associatedTypeDecl        = NodeKind(1 << 11 | typeDecl.rawValue)
  public static let bindingDecl               = NodeKind(1 << 12 | decl.rawValue)
  public static let conformanceDecl           = NodeKind(1 << 13 | decl.rawValue)
  public static let extensionDecl             = NodeKind(1 << 14 | decl.rawValue)
  public static let funDecl                   = NodeKind(1 << 15 | decl.rawValue)
  public static let genericSizeParamDecl      = NodeKind(1 << 16 | typeDecl.rawValue)
  public static let genericTypeParamDecl      = NodeKind(1 << 17 | typeDecl.rawValue)
  public static let methodImplDecl            = NodeKind(1 << 18 | decl.rawValue)
  public static let moduleDecl                = NodeKind(1 << 19 | typeDecl.rawValue)
  public static let namespaceDecl             = NodeKind(1 << 20 | typeDecl.rawValue)
  public static let paramDecl                 = NodeKind(1 << 21 | decl.rawValue)
  public static let productTypeDecl           = NodeKind(1 << 22 | typeDecl.rawValue)
  public static let subscriptDecl             = NodeKind(1 << 23 | decl.rawValue)
  public static let subscriptImplDecl         = NodeKind(1 << 24 | decl.rawValue)
  public static let traitDecl                 = NodeKind(1 << 25 | typeDecl.rawValue)
  public static let typeAliasDecl             = NodeKind(1 << 26 | typeDecl.rawValue)
  public static let varDecl                   = NodeKind(1 << 27 | decl.rawValue)

  // MARK: Value expressions

  public static let expr                      = NodeKind(1 << 1)

  public static let asyncExpr                 = NodeKind(1 << 10 | expr.rawValue)
  public static let awaitExpr                 = NodeKind(1 << 11 | expr.rawValue)
  public static let boolLiteralExpr           = NodeKind(1 << 12 | expr.rawValue)
  public static let bufferLiteralExpr         = NodeKind(1 << 13 | expr.rawValue)
  public static let charLiteralExpr           = NodeKind(1 << 14 | expr.rawValue)
  public static let condExpr                  = NodeKind(1 << 15 | expr.rawValue)
  public static let floatLiteralExpr          = NodeKind(1 << 16 | expr.rawValue)
  public static let funCallExpr               = NodeKind(1 << 17 | expr.rawValue)
  public static let intLiteralExpr            = NodeKind(1 << 18 | expr.rawValue)
  public static let lambdaExpr                = NodeKind(1 << 19 | expr.rawValue)
  public static let mapLiteralExpr            = NodeKind(1 << 20 | expr.rawValue)
  public static let matchExpr                 = NodeKind(1 << 21 | expr.rawValue)
  public static let matchCaseExpr             = NodeKind(1 << 22 | expr.rawValue)
  public static let nameExpr                  = NodeKind(1 << 23 | expr.rawValue)
  public static let nilExpr                   = NodeKind(1 << 24 | expr.rawValue)
  public static let storedProjectionExpr      = NodeKind(1 << 25 | expr.rawValue)
  public static let stringLiteralExpr         = NodeKind(1 << 26 | expr.rawValue)
  public static let subscriptCallExpr         = NodeKind(1 << 27 | expr.rawValue)
  public static let tupleExpr                 = NodeKind(1 << 28 | expr.rawValue)
  public static let unfoldedExpr              = NodeKind(1 << 29 | expr.rawValue)

  // MARK: Patterns

  public static let pattern                   = NodeKind(1 << 2)

  public static let bindingPattern            = NodeKind(1 << 10 | pattern.rawValue)
  public static let exprPattern               = NodeKind(1 << 11 | pattern.rawValue)
  public static let namePattern               = NodeKind(1 << 12 | pattern.rawValue)
  public static let tuplePattern              = NodeKind(1 << 13 | pattern.rawValue)
  public static let wildcardPattern           = NodeKind(1 << 14 | pattern.rawValue)

  // MARK: Statements

  public static let stmt                      = NodeKind(1 << 3)

  public static let braceStmt                 = NodeKind(1 << 10 | stmt.rawValue)
  public static let breakStmt                 = NodeKind(1 << 11 | stmt.rawValue)
  public static let continueStmt              = NodeKind(1 << 12 | stmt.rawValue)
  public static let declStmt                  = NodeKind(1 << 13 | stmt.rawValue)
  public static let doWhileStmt               = NodeKind(1 << 14 | stmt.rawValue)
  public static let exprStmt                  = NodeKind(1 << 15 | stmt.rawValue)
  public static let forStmt                   = NodeKind(1 << 16 | stmt.rawValue)
  public static let returnStmt                = NodeKind(1 << 17 | stmt.rawValue)
  public static let whileStmt                 = NodeKind(1 << 18 | stmt.rawValue)
  public static let yieldStmt                 = NodeKind(1 << 19 | stmt.rawValue)

  // MARK: Type expressions

  /// The kind of type expression nodes.
  public static let typeExpr                  = NodeKind(1 << 4)

  public static let asyncTypeExpr             = NodeKind(1 << 10 | typeExpr.rawValue)
  public static let conformanceLensTypeExpr   = NodeKind(1 << 11 | typeExpr.rawValue)
  public static let existentialTypeExpr       = NodeKind(1 << 12 | typeExpr.rawValue)
  public static let indirectTypeExpr          = NodeKind(1 << 13 | typeExpr.rawValue)
  public static let lambdaTypeExpr            = NodeKind(1 << 14 | typeExpr.rawValue)
  public static let nameTypeExpr              = NodeKind(1 << 15 | typeExpr.rawValue)
  public static let paramTypeExpr             = NodeKind(1 << 16 | typeExpr.rawValue)
  public static let storedProjectionTypeExpr  = NodeKind(1 << 17 | typeExpr.rawValue)
  public static let tupleTypeExpr             = NodeKind(1 << 18 | typeExpr.rawValue)
  public static let unionTypeExpr             = NodeKind(1 << 19 | typeExpr.rawValue)

}

extension NodeKind: CustomStringConvertible {

  public var description: String {
    switch self {
    case .decl                      : return "Decl"
    case .typeDecl                  : return "TypeDecl"
    case .associatedSizeDecl        : return "AssociatedSizeDecl"
    case .associatedTypeDecl        : return "AssociatedTypeDecl"
    case .bindingDecl               : return "BindingDecl"
    case .conformanceDecl           : return "ConformanceDecl"
    case .extensionDecl             : return "ExtensionDecl"
    case .funDecl                   : return "FunDecl"
    case .genericSizeParamDecl      : return "GenericSizeParamDecl"
    case .genericTypeParamDecl      : return "GenericTypeParamDecl"
    case .methodImplDecl            : return "MethodImplDecl"
    case .moduleDecl                : return "ModuleDecl"
    case .namespaceDecl             : return "NamespaceDecl"
    case .paramDecl                 : return "ParamDecl"
    case .productTypeDecl           : return "ProductTypeDecl"
    case .subscriptDecl             : return "SubscriptDecl"
    case .subscriptImplDecl         : return "SubscriptImplDecl"
    case .traitDecl                 : return "TraitDecl"
    case .typeAliasDecl             : return "TypeAliasDecl"
    case .varDecl                   : return "VarDecl"

    case .expr                      : return "Expr"
    case .asyncExpr                 : return "AsyncExpr"
    case .awaitExpr                 : return "AwaitExpr"
    case .boolLiteralExpr           : return "BoolLiteralExpr"
    case .bufferLiteralExpr         : return "BufferLiteralExpr"
    case .charLiteralExpr           : return "CharLiteralExpr"
    case .condExpr                  : return "CondExpr"
    case .floatLiteralExpr          : return "FloatLiteralExpr"
    case .funCallExpr               : return "FunCallExpr"
    case .intLiteralExpr            : return "IntLiteralExpr"
    case .lambdaExpr                : return "LambdaExpr"
    case .mapLiteralExpr            : return "MapLiteralExpr"
    case .matchExpr                 : return "MatchExpr"
    case .nameExpr                  : return "NameExpr"
    case .nilExpr                   : return "NilExpr"
    case .storedProjectionExpr      : return "StoredProjectionExpr"
    case .stringLiteralExpr         : return "StringLiteralExpr"
    case .subscriptCallExpr         : return "SubscriptCallExpr"
    case .tupleExpr                 : return "TupleExpr"
    case .unfoldedExpr              : return "UnfoldedExpr"

    case .pattern                   : return "Pattern"
    case .bindingPattern            : return "BindingPattern"
    case .exprPattern               : return "ExprPattern"
    case .namePattern               : return "NamePattern"
    case .tuplePattern              : return "TuplePattern"
    case .wildcardPattern           : return "WildcardPattern"

    case .stmt                      : return "Stmt"
    case .braceStmt                 : return "BraceStmt"
    case .breakStmt                 : return "BreakStmt"
    case .continueStmt              : return "ContinueStmt"
    case .declStmt                  : return "DeclStmt"
    case .doWhileStmt               : return "DoWhileStmt"
    case .exprStmt                  : return "ExprStmt"
    case .forStmt                   : return "ForStmt"
    case .returnStmt                : return "ReturnStmt"
    case .whileStmt                 : return "WhileStmt"
    case .yieldStmt                 : return "YieldStmt"

    case .typeExpr                  : return "TypeExpr"
    case .asyncTypeExpr             : return "AsyncTypeExpr"
    case .conformanceLensTypeExpr   : return "ConformanceLensTypeExpr"
    case .existentialTypeExpr       : return "ExistentialTypeExpr"
    case .indirectTypeExpr          : return "IndirectTypeExpr"
    case .lambdaTypeExpr            : return "LambdaTypeExpr"
    case .nameTypeExpr              : return "NameTypeExpr"
    case .paramTypeExpr             : return "ParamTypeExpr"
    case .storedProjectionTypeExpr  : return "StoredProjectionTypeExpr"
    case .tupleTypeExpr             : return "TupleTypeExpr"
    case .unionTypeExpr             : return "UnionTypeExpr"

    default                         : return("Unknown")
    }
  }

}
