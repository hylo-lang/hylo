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
    precondition(r.rawValue >> 16 == 0, "RHS is not a node category")
    return l.rawValue & r.rawValue == r.rawValue
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

  public static let associatedTypeDecl = NodeKind(
     1 << 16 | typeDecl.rawValue)
  public static let associatedValueDecl = NodeKind(
     2 << 16 | decl.rawValue)
  public static let bindingDecl = NodeKind(
     3 << 16 | decl.rawValue)
  public static let builtinDecl = NodeKind(
     4 << 16 | decl.rawValue)
  public static let conformanceDecl = NodeKind(
     5 << 16 | decl.rawValue | genericScope.rawValue)
  public static let extensionDecl = NodeKind(
     6 << 16 | decl.rawValue | genericScope.rawValue)
  public static let funDecl = NodeKind(
     7 << 16 | decl.rawValue | genericScope.rawValue)
  public static let genericTypeParamDecl = NodeKind(
     8 << 16 | typeDecl.rawValue)
  public static let genericValueParamDecl = NodeKind(
     9 << 16 | typeDecl.rawValue)
  public static let importDecl = NodeKind(
    10 << 16 | typeDecl.rawValue)
  public static let methodImplDecl = NodeKind(
    11 << 16 | decl.rawValue | lexicalScope.rawValue)
  public static let moduleDecl = NodeKind(
    12 << 16 | typeDecl.rawValue | lexicalScope.rawValue)
  public static let namespaceDecl = NodeKind(
    13 << 16 | typeDecl.rawValue | lexicalScope.rawValue)
  public static let operatorDecl = NodeKind(
    14 << 16 | decl.rawValue)
  public static let parameterDecl = NodeKind(
    15 << 16 | decl.rawValue)
  public static let productTypeDecl = NodeKind(
    16 << 16 | typeDecl.rawValue | genericScope.rawValue)
  public static let subscriptDecl = NodeKind(
    17 << 16 | decl.rawValue | genericScope.rawValue)
  public static let subscriptImplDecl = NodeKind(
    18 << 16 | decl.rawValue | lexicalScope.rawValue)
  public static let traitDecl = NodeKind(
    19 << 16 | typeDecl.rawValue | genericScope.rawValue)
  public static let typeAliasDecl = NodeKind(
    20 << 16 | typeDecl.rawValue | genericScope.rawValue)
  public static let varDecl = NodeKind(
    21 << 16 | decl.rawValue)

  // MARK: Value expressions

  public static let expr = NodeKind(
    1 << 1)

  public static let assignExpr = NodeKind(
     1 << 16 | expr.rawValue)
  public static let asyncExpr = NodeKind(
     2 << 16 | expr.rawValue)
  public static let awaitExpr = NodeKind(
     3 << 16 | expr.rawValue)
  public static let booleanLiteralExpr = NodeKind(
     4 << 16 | expr.rawValue)
  public static let bufferLiteralExpr = NodeKind(
     5 << 16 | expr.rawValue)
  public static let castExpr = NodeKind(
     6 << 16 | expr.rawValue)
  public static let condExpr = NodeKind(
     7 << 16 | expr.rawValue | lexicalScope.rawValue)
  public static let errorExpr = NodeKind(
     8 << 16 | expr.rawValue)
  public static let floatLiteralExpr = NodeKind(
     9 << 16 | expr.rawValue)
  public static let funCallExpr = NodeKind(
    10 << 16 | expr.rawValue)
  public static let inoutExpr = NodeKind(
    11 << 16 | expr.rawValue)
  public static let integerLiteralExpr = NodeKind(
    12 << 16 | expr.rawValue)
  public static let lambdaExpr = NodeKind(
    13 << 16 | expr.rawValue)
  public static let mapLiteralExpr = NodeKind(
    14 << 16 | expr.rawValue)
  public static let matchExpr = NodeKind(
    15 << 16 | expr.rawValue | lexicalScope.rawValue)
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
  public static let tupleMemberExpr = NodeKind(
    23 << 16 | expr.rawValue)
  public static let unicodeScalarLiteralExpr = NodeKind(
    24 << 16 | expr.rawValue)

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
  public static let condBindingStmt = NodeKind(
     3 << 16 | stmt.rawValue)
  public static let continueStmt = NodeKind(
     4 << 16 | stmt.rawValue)
  public static let declStmt = NodeKind(
     5 << 16 | stmt.rawValue)
  public static let discardStmt = NodeKind(
     6 << 16 | stmt.rawValue)
  public static let doWhileStmt = NodeKind(
     7 << 16 | stmt.rawValue)
  public static let exprStmt = NodeKind(
     8 << 16 | stmt.rawValue)
  public static let forStmt = NodeKind(
     9 << 16 | stmt.rawValue | lexicalScope.rawValue)
  public static let returnStmt = NodeKind(
    10 << 16 | stmt.rawValue)
  public static let whileStmt = NodeKind(
    11 << 16 | stmt.rawValue | lexicalScope.rawValue)
  public static let yieldStmt = NodeKind(
    12 << 16 | stmt.rawValue)

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

  // MARK: Others

  public static let matchCase = NodeKind(
    1 << 16 | lexicalScope.rawValue)
  public static let topLevelDeclSet = NodeKind(
    2 << 16 | lexicalScope.rawValue)

}

extension NodeKind: CustomStringConvertible {

  public var description: String {
    switch self {
    case .decl                      : return "Decl"
    case .typeDecl                  : return "TypeDecl"
    case .associatedTypeDecl        : return "AssociatedTypeDecl"
    case .associatedValueDecl       : return "AssociatedValueDecl"
    case .bindingDecl               : return "BindingDecl"
    case .builtinDecl               : return "BuiltinDecl"
    case .conformanceDecl           : return "ConformanceDecl"
    case .extensionDecl             : return "ExtensionDecl"
    case .funDecl                   : return "FunDecl"
    case .genericTypeParamDecl      : return "GenericTypeParamDecl"
    case .genericValueParamDecl     : return "GenericValueParamDecl"
    case .importDecl                : return "ImportDecl"
    case .methodImplDecl            : return "MethodImplDecl"
    case .moduleDecl                : return "ModuleDecl"
    case .namespaceDecl             : return "NamespaceDecl"
    case .operatorDecl              : return "OperatorDecl"
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
    case .booleanLiteralExpr        : return "BooleanLiteralExpr"
    case .bufferLiteralExpr         : return "BufferLiteralExpr"
    case .condExpr                  : return "CondExpr"
    case .errorExpr                 : return "ErrorExpr"
    case .floatLiteralExpr          : return "FloatLiteralExpr"
    case .funCallExpr               : return "FunCallExpr"
    case .inoutExpr                 : return "InoutExpr"
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
    case .tupleMemberExpr           : return "TupleMemberExpr"
    case .unicodeScalarLiteralExpr  : return "UnicodeScalarLiteralExpr"

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

    case .matchCase                 : return "MatchCase"
    case .topLevelDeclSet           : return "TopLevelDeclSet"

    default                         : return("Unknown")
    }
  }

}
