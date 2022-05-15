import Utils

/// An identifier uniquely identifying the type of a node referred by an ID.
public struct NodeKind: Hashable {

  /// The raw value of the identifier.
  ///
  /// - Bits 1 to 5 designate the node category.
  /// - Bits 6 to 7 designate the node sub-category.
  /// - Bits 8 to 9 designate the node's lexical scope category.
  /// - Bits 16 to 32 designate the node type.
  private var rawValue: Int32

  private init(_ rawValue: Int32) {
    self.rawValue = rawValue
  }

  public static func <= (l: Self, r: Self) -> Bool {
    l.rawValue & r.rawValue == r.rawValue
  }

  public static let lexicalScope = NodeKind(
    1 << 8)
  public static let genericScope = NodeKind(
    1 << 9 | lexicalScope.rawValue)

  // MARK: Declarations

  public static let decl = NodeKind(
    1 << 0)
  public static let typeDecl = NodeKind(
    1 << 6 | decl.rawValue)

  public static let associatedSizeDecl = NodeKind(
     1 << 16 | decl.rawValue)
  public static let associatedTypeDecl = NodeKind(
     2 << 16 | typeDecl.rawValue)
  public static let bindingDecl = NodeKind(
     3 << 16 | decl.rawValue)
  public static let conformanceDecl = NodeKind(
     4 << 16 | decl.rawValue | genericScope.rawValue)
  public static let extensionDecl = NodeKind(
     5 << 16 | decl.rawValue | genericScope.rawValue)
  public static let funDecl = NodeKind(
     6 << 16 | decl.rawValue | genericScope.rawValue)
  public static let genericSizeParamDecl = NodeKind(
     7 << 16 | typeDecl.rawValue)
  public static let genericTypeParamDecl = NodeKind(
     8 << 16 | typeDecl.rawValue)
  public static let methodImplDecl = NodeKind(
     9 << 16 | decl.rawValue)
  public static let moduleDecl = NodeKind(
    10 << 16 | typeDecl.rawValue | lexicalScope.rawValue)
  public static let namespaceDecl = NodeKind(
    11 << 16 | typeDecl.rawValue | lexicalScope.rawValue)
  public static let parameterDecl = NodeKind(
    12 << 16 | decl.rawValue)
  public static let productTypeDecl = NodeKind(
    13 << 16 | typeDecl.rawValue | genericScope.rawValue)
  public static let subscriptDecl = NodeKind(
    14 << 16 | decl.rawValue | genericScope.rawValue)
  public static let subscriptImplDecl = NodeKind(
    15 << 16 | decl.rawValue)
  public static let traitDecl = NodeKind(
    16 << 16 | typeDecl.rawValue | genericScope.rawValue)
  public static let typeAliasDecl = NodeKind(
    17 << 16 | typeDecl.rawValue | genericScope.rawValue)
  public static let varDecl = NodeKind(
    18 << 16 | decl.rawValue)

  // MARK: Value expressions

  public static let expr = NodeKind(
    1 << 1)

  public static let assignExpr = NodeKind(
     1 << 16 | expr.rawValue)
  public static let asyncExpr = NodeKind(
     2 << 16 | expr.rawValue)
  public static let awaitExpr = NodeKind(
     3 << 16 | expr.rawValue)
  public static let boolLiteralExpr = NodeKind(
     4 << 16 | expr.rawValue)
  public static let bufferLiteralExpr = NodeKind(
     5 << 16 | expr.rawValue)
  public static let charLiteralExpr = NodeKind(
     6 << 16 | expr.rawValue)
  public static let castExpr = NodeKind(
     7 << 16 | expr.rawValue)
  public static let condExpr = NodeKind(
     8 << 16 | expr.rawValue | lexicalScope.rawValue)
  public static let floatLiteralExpr = NodeKind(
     9 << 16 | expr.rawValue)
  public static let funCallExpr = NodeKind(
    10 << 16 | expr.rawValue)
  public static let integerLiteralExpr = NodeKind(
    11 << 16 | expr.rawValue)
  public static let lambdaExpr = NodeKind(
    12 << 16 | expr.rawValue)
  public static let mapLiteralExpr = NodeKind(
    13 << 16 | expr.rawValue)
  public static let matchExpr = NodeKind(
    14 << 16 | expr.rawValue | lexicalScope.rawValue)
  public static let matchCaseExpr = NodeKind(
    15 << 16 | expr.rawValue)
  public static let nameExpr = NodeKind(
    16 << 16 | expr.rawValue)
  public static let nilExpr = NodeKind(
    17 << 16 | expr.rawValue)
  public static let sequenceExpr = NodeKind(
    18 << 16 | expr.rawValue)
  public static let storedProjectionExpr = NodeKind(
    19 << 16 | expr.rawValue)
  public static let stringLiteralExpr = NodeKind(
    20 << 26 | expr.rawValue)
  public static let subscriptCallExpr = NodeKind(
    21 << 16 | expr.rawValue)
  public static let tupleExpr = NodeKind(
    22 << 16 | expr.rawValue)

  // MARK: Patterns

  public static let pattern = NodeKind(
    1 << 2)

  public static let bindingPattern = NodeKind(
    1 << 16 | pattern.rawValue)
  public static let exprPattern = NodeKind(
    2 << 16 | pattern.rawValue)
  public static let namePattern = NodeKind(
    3 << 16 | pattern.rawValue)
  public static let tuplePattern = NodeKind(
    4 << 16 | pattern.rawValue)
  public static let wildcardPattern = NodeKind(
    5 << 16 | pattern.rawValue)

  // MARK: Statements

  public static let stmt = NodeKind(
    1 << 3)

  public static let braceStmt = NodeKind(
     1 << 16 | stmt.rawValue | lexicalScope.rawValue)
  public static let breakStmt = NodeKind(
     2 << 16 | stmt.rawValue)
  public static let continueStmt = NodeKind(
     3 << 16 | stmt.rawValue)
  public static let declStmt = NodeKind(
     4 << 16 | stmt.rawValue)
  public static let doWhileStmt = NodeKind(
     5 << 16 | stmt.rawValue)
  public static let exprStmt = NodeKind(
     6 << 16 | stmt.rawValue)
  public static let forStmt = NodeKind(
     7 << 16 | stmt.rawValue | lexicalScope.rawValue)
  public static let returnStmt = NodeKind(
     8 << 16 | stmt.rawValue)
  public static let whileStmt = NodeKind(
     9 << 16 | stmt.rawValue | lexicalScope.rawValue)
  public static let yieldStmt = NodeKind(
    10 << 16 | stmt.rawValue)

  // MARK: Type expressions

  /// The kind of type expression nodes.
  public static let typeExpr = NodeKind(
    1 << 4)

  public static let asyncTypeExpr = NodeKind(
     1 << 16 | typeExpr.rawValue)
  public static let conformanceLensTypeExpr = NodeKind(
     2 << 16 | typeExpr.rawValue)
  public static let existentialTypeExpr = NodeKind(
     3 << 16 | typeExpr.rawValue)
  public static let indirectTypeExpr = NodeKind(
     4 << 16 | typeExpr.rawValue)
  public static let lambdaTypeExpr = NodeKind(
     5 << 16 | typeExpr.rawValue)
  public static let nameTypeExpr = NodeKind(
     6 << 16 | typeExpr.rawValue)
  public static let parameterTypeExpr = NodeKind(
     7 << 16 | typeExpr.rawValue)
  public static let storedProjectionTypeExpr = NodeKind(
     8 << 16 | typeExpr.rawValue)
  public static let tupleTypeExpr = NodeKind(
     9 << 16 | typeExpr.rawValue)
  public static let unionTypeExpr = NodeKind(
    10 << 16 | typeExpr.rawValue)
  public static let wildcardTypeExpr = NodeKind(
    11 << 16 | typeExpr.rawValue)

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
    case .parameterDecl             : return "ParameterDecl"
    case .productTypeDecl           : return "ProductTypeDecl"
    case .subscriptDecl             : return "SubscriptDecl"
    case .subscriptImplDecl         : return "SubscriptImplDecl"
    case .traitDecl                 : return "TraitDecl"
    case .typeAliasDecl             : return "TypeAliasDecl"
    case .varDecl                   : return "VarDecl"

    case .expr                      : return "Expr"
    case .assignExpr                : return "AssignExpr"
    case .asyncExpr                 : return "AsyncExpr"
    case .awaitExpr                 : return "AwaitExpr"
    case .boolLiteralExpr           : return "BoolLiteralExpr"
    case .bufferLiteralExpr         : return "BufferLiteralExpr"
    case .charLiteralExpr           : return "CharLiteralExpr"
    case .condExpr                  : return "CondExpr"
    case .floatLiteralExpr          : return "FloatLiteralExpr"
    case .funCallExpr               : return "FunCallExpr"
    case .integerLiteralExpr        : return "IntegerLiteralExpr"
    case .lambdaExpr                : return "LambdaExpr"
    case .mapLiteralExpr            : return "MapLiteralExpr"
    case .matchExpr                 : return "MatchExpr"
    case .nameExpr                  : return "NameExpr"
    case .nilExpr                   : return "NilExpr"
    case .sequenceExpr              : return "SequenceExpr"
    case .storedProjectionExpr      : return "StoredProjectionExpr"
    case .stringLiteralExpr         : return "StringLiteralExpr"
    case .subscriptCallExpr         : return "SubscriptCallExpr"
    case .tupleExpr                 : return "TupleExpr"

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
    case .parameterTypeExpr         : return "ParameterTypeExpr"
    case .storedProjectionTypeExpr  : return "StoredProjectionTypeExpr"
    case .tupleTypeExpr             : return "TupleTypeExpr"
    case .unionTypeExpr             : return "UnionTypeExpr"
    case .wildcardTypeExpr          : return "WildcardTypeExpr"

    default                         : return("Unknown")
    }
  }

}
