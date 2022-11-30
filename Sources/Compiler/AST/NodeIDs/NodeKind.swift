import Utils

/// The type of an AST node; a nominal wrapper for `Node.Type` that adds conformances and
/// convenience APIs.
public struct NodeKind: Codable, Equatable, Hashable {

  /// The underlying value.
  let value: Node.Type

  /// Creates an instance with the given underlying value.
  public init(_ value: Node.Type) {
    self.value = value
  }

  /// Serializes `self` into `destination`.
  public func encode(to destination: Encoder) throws {
    try Self.indices[self]!.encode(to: destination)
  }

  /// Deserializes `self` from `source`.
  public init(from source: Decoder) throws {
    let index = try Int(from: source)
    value = Self.allValues[index]
  }

  /// Returns true iff `l` and `r` denote the same node type.
  public static func == (l: Self, r: Self) -> Bool {
    return l.value == r.value
  }

  /// Incorporates the value of `self` into `h`.
  public func hash(into h: inout Hasher) {
    ObjectIdentifier(value).hash(into: &h)
  }

  /// Returns true iff `l` and `r` denote the same node type.
  static func == (l: Self, r: Node.Type) -> Bool {
    return l.value == r
  }

  /// Returns true iff `l` and `r` do not denote the same node type.
  static func != (l: Self, r: Node.Type) -> Bool {
    return l.value == r
  }

  /// Returns true iff `l` and `r` denote the same node type.
  static func == (l: Node.Type, r: Self) -> Bool {
    return l == r.value
  }

  /// Returns true iff `l` and `r` do not denote the same node type.
  static func != (l: Node.Type, r: Self) -> Bool {
    return l != r.value
  }

  /// Returns true iff `me` and `pattern` denote the same node type.
  static func ~=(pattern: Node.Type, me: Self) -> Bool {
    me == pattern
  }

}

/// Extend heterogeneous equality comparison with Node.Type to Optional<NodeKind>.
extension Optional where Wrapped == NodeKind {

  /// Returns true iff `l` and `r` denote the same node type.
  static func == (l: Self, r: Node.Type) -> Bool {
    return l?.value == r
  }

  /// Returns true iff `l` and `r` do not denote the same node type.
  static func != (l: Self, r: Node.Type) -> Bool {
    return l?.value == r
  }

  /// Returns true iff `l` and `r` denote the same node type.
  static func == (l: Node.Type, r: Self) -> Bool {
    return l == r?.value
  }

  /// Returns true iff `l` and `r` do not denote the same node type.
  static func != (l: Node.Type, r: Self) -> Bool {
    return l != r?.value
  }

}

extension NodeKind: CustomStringConvertible {

  /// The name of the underlying Node type.
  public var description: String { String(describing: value) }

}

extension NodeKind {

  static let allValues: [Node.Type] = [
    // MARK: Declarations
    AssociatedTypeDecl.self,
    AssociatedValueDecl.self,
    BindingDecl.self,
    BuiltinDecl.self,
    ConformanceDecl.self,
    ExtensionDecl.self,
    FunctionDecl.self,
    GenericParameterDecl.self,
    ImportDecl.self,
    InitializerDecl.self,
    MethodDecl.self,
    MethodImplDecl.self,
    ModuleDecl.self,
    NamespaceDecl.self,
    OperatorDecl.self,
    ParameterDecl.self,
    ProductTypeDecl.self,
    SubscriptDecl.self,
    SubscriptImplDecl.self,
    TraitDecl.self,
    TypeAliasDecl.self,
    VarDecl.self,

    // MARK: Value expressions
    AssignExpr.self,
    BooleanLiteralExpr.self,
    BufferLiteralExpr.self,
    CastExpr.self,
    CondExpr.self,
    ErrorExpr.self,
    FloatLiteralExpr.self,
    FunCallExpr.self,
    InoutExpr.self,
    IntegerLiteralExpr.self,
    LambdaExpr.self,
    MapLiteralExpr.self,
    MatchExpr.self,
    NameExpr.self,
    NilLiteralExpr.self,
    SequenceExpr.self,
    SpawnExpr.self,
    StringLiteralExpr.self,
    SubscriptCallExpr.self,
    TupleExpr.self,
    TupleMemberExpr.self,
    UnicodeScalarLiteralExpr.self,

    // MARK: Patterns
    BindingPattern.self,
    ExprPattern.self,
    NamePattern.self,
    TuplePattern.self,
    WildcardPattern.self,

    // MARK: Statements
    BraceStmt.self,
    BreakStmt.self,
    CondBindingStmt.self,
    ContinueStmt.self,
    DeclStmt.self,
    DiscardStmt.self,
    DoWhileStmt.self,
    ExprStmt.self,
    ForStmt.self,
    ReturnStmt.self,
    WhileStmt.self,
    YieldStmt.self,

    // MARK: Type expressions
    ConformanceLensTypeExpr.self,
    ExistentialTypeExpr.self,
    LambdaTypeExpr.self,
    ParameterTypeExpr.self,
    RemoteTypeExpr.self,
    TupleTypeExpr.self,
    UnionTypeExpr.self,

    // MARK: Others
    MatchCase.self,
    TopLevelDeclSet.self
  ]

  static let indices = Dictionary(
    uniqueKeysWithValues: allValues.enumerated().map { (n, t) in (NodeKind(t), n) })

}
