import Utils

/// A type that is notified when nodes are entered and left during an AST traversal.
///
/// Use this protocol to implement algorithms that must traverse all or most AST nodes and perform
/// similar operations on each of them. Instances of types implementing this protocol are meant to
/// be passed as argument to `AST.walk(_:notifying:)`.
///
/// For example:
///
///     struct V: ASTWalkObserver {
///       var outermostFunctions: [FunctionDecl.ID] = []
///       mutating func willEnter(_ n: AnyNodeID, in ast: AST) -> Bool {
///         if let d = FunctionDecl.ID(n) {
///           outermostFunctions.append(d)
///           return false
///         } else {
///           return true
///         }
///       }
///     }
///     var v = V()
///     for m in ast.modules { ast.walk(m, notifying: &v) }
///     print(v.outermostFunctions)
///
/// This program prints the IDs of the outermost function declarations in `ast`.
public protocol ASTWalkObserver {

  /// Called when `n`, which is in `ast`, is about to be entered; returns `false` if traversal
  /// should skip `n`.
  ///
  /// Use this method to perform actions before a node is being traversed and/or customize how the
  /// AST is traversed. If the method returns `true`, `willEnter` will be before each child of `n`
  /// is entered and `willExit` will be called when `n` is left. If it returns `false`, neither
  /// `willEnter` nor `willExit` will be called for `n` and its children.
  mutating func willEnter(_ n: AnyNodeID, in ast: AST) -> Bool

  /// Called when `n`, which is in `ast`, is about to be left.
  mutating func willExit(_ n: AnyNodeID, in ast: AST)

}

extension ASTWalkObserver {

  public func willEnter(_ n: AnyNodeID, in ast: AST) -> Bool { true }

  public func willExit(_ n: AnyNodeID, in ast: AST) {}

}

extension AST {

  /// Visits `n` and its children in pre-order, notifying `o` when a node is entered or left.
  public func walk<O: ASTWalkObserver>(_ n: AnyNodeID, notifying o: inout O) {
    guard o.willEnter(n, in: self) else { return }
    switch n.kind {
    case AssociatedTypeDecl.self:
      traverse(self[n] as! AssociatedTypeDecl, notifying: &o)
    case AssociatedValueDecl.self:
      traverse(self[n] as! AssociatedValueDecl, notifying: &o)
    case BindingDecl.self:
      traverse(self[n] as! BindingDecl, notifying: &o)
    case ConformanceDecl.self:
      traverse(self[n] as! ConformanceDecl, notifying: &o)
    case ExtensionDecl.self:
      traverse(self[n] as! ExtensionDecl, notifying: &o)
    case FunctionDecl.self:
      traverse(self[n] as! FunctionDecl, notifying: &o)
    case GenericParameterDecl.self:
      traverse(self[n] as! GenericParameterDecl, notifying: &o)
    case ImportDecl.self:
      traverse(self[n] as! ImportDecl, notifying: &o)
    case InitializerDecl.self:
      traverse(self[n] as! InitializerDecl, notifying: &o)
    case MethodDecl.self:
      traverse(self[n] as! MethodDecl, notifying: &o)
    case MethodImpl.self:
      traverse(self[n] as! MethodImpl, notifying: &o)
    case ModuleDecl.self:
      traverse(self[n] as! ModuleDecl, notifying: &o)
    case NamespaceDecl.self:
      traverse(self[n] as! NamespaceDecl, notifying: &o)
    case OperatorDecl.self:
      traverse(self[n] as! OperatorDecl, notifying: &o)
    case ParameterDecl.self:
      traverse(self[n] as! ParameterDecl, notifying: &o)
    case ProductTypeDecl.self:
      traverse(self[n] as! ProductTypeDecl, notifying: &o)
    case SubscriptDecl.self:
      traverse(self[n] as! SubscriptDecl, notifying: &o)
    case SubscriptImpl.self:
      traverse(self[n] as! SubscriptImpl, notifying: &o)
    case TraitDecl.self:
      traverse(self[n] as! TraitDecl, notifying: &o)
    case TypeAliasDecl.self:
      traverse(self[n] as! TypeAliasDecl, notifying: &o)
    case VarDecl.self:
      traverse(self[n] as! VarDecl, notifying: &o)

    case ArrowTypeExpr.self:
      traverse(self[n] as! ArrowTypeExpr, notifying: &o)
    case BooleanLiteralExpr.self:
      traverse(self[n] as! BooleanLiteralExpr, notifying: &o)
    case BufferLiteralExpr.self:
      traverse(self[n] as! BufferLiteralExpr, notifying: &o)
    case CaptureExpr.self:
      traverse(self[n] as! CaptureExpr, notifying: &o)
    case CastExpr.self:
      traverse(self[n] as! CastExpr, notifying: &o)
    case ConditionalExpr.self:
      traverse(self[n] as! ConditionalExpr, notifying: &o)
    case ConformanceLensExpr.self:
      traverse(self[n] as! ConformanceLensExpr, notifying: &o)
    case ExistentialTypeExpr.self:
      traverse(self[n] as! ExistentialTypeExpr, notifying: &o)
    case FloatLiteralExpr.self:
      traverse(self[n] as! FloatLiteralExpr, notifying: &o)
    case FunctionCallExpr.self:
      traverse(self[n] as! FunctionCallExpr, notifying: &o)
    case InoutExpr.self:
      traverse(self[n] as! InoutExpr, notifying: &o)
    case IntegerLiteralExpr.self:
      traverse(self[n] as! IntegerLiteralExpr, notifying: &o)
    case LambdaExpr.self:
      traverse(self[n] as! LambdaExpr, notifying: &o)
    case MapLiteralExpr.self:
      traverse(self[n] as! MapLiteralExpr, notifying: &o)
    case MatchExpr.self:
      traverse(self[n] as! MatchExpr, notifying: &o)
    case NameExpr.self:
      traverse(self[n] as! NameExpr, notifying: &o)
    case ParameterTypeExpr.self:
      traverse(self[n] as! ParameterTypeExpr, notifying: &o)
    case PragmaLiteralExpr.self:
      traverse(self[n] as! PragmaLiteralExpr, notifying: &o)
    case RemoteTypeExpr.self:
      traverse(self[n] as! RemoteTypeExpr, notifying: &o)
    case SequenceExpr.self:
      traverse(self[n] as! SequenceExpr, notifying: &o)
    case SpawnExpr.self:
      traverse(self[n] as! SpawnExpr, notifying: &o)
    case StringLiteralExpr.self:
      traverse(self[n] as! StringLiteralExpr, notifying: &o)
    case SubscriptCallExpr.self:
      traverse(self[n] as! SubscriptCallExpr, notifying: &o)
    case TupleExpr.self:
      traverse(self[n] as! TupleExpr, notifying: &o)
    case TupleMemberExpr.self:
      traverse(self[n] as! TupleMemberExpr, notifying: &o)
    case TupleTypeExpr.self:
      traverse(self[n] as! TupleTypeExpr, notifying: &o)
    case UnicodeScalarLiteralExpr.self:
      traverse(self[n] as! UnicodeScalarLiteralExpr, notifying: &o)
    case WildcardExpr.self:
      traverse(self[n] as! WildcardExpr, notifying: &o)

    case BindingPattern.self:
      traverse(self[n] as! BindingPattern, notifying: &o)
    case ExprPattern.self:
      traverse(self[n] as! ExprPattern, notifying: &o)
    case NamePattern.self:
      traverse(self[n] as! NamePattern, notifying: &o)
    case OptionPattern.self:
      traverse(self[n] as! OptionPattern, notifying: &o)
    case TuplePattern.self:
      traverse(self[n] as! TuplePattern, notifying: &o)
    case WildcardPattern.self:
      traverse(self[n] as! WildcardPattern, notifying: &o)

    case AssignStmt.self:
      traverse(self[n] as! AssignStmt, notifying: &o)
    case BraceStmt.self:
      traverse(self[n] as! BraceStmt, notifying: &o)
    case BreakStmt.self:
      traverse(self[n] as! BreakStmt, notifying: &o)
    case ConditionalBindingStmt.self:
      traverse(self[n] as! ConditionalBindingStmt, notifying: &o)
    case ConditionalCompilationStmt.self:
      traverse(self[n] as! ConditionalCompilationStmt, notifying: &o)
    case ConditionalStmt.self:
      traverse(self[n] as! ConditionalStmt, notifying: &o)
    case ContinueStmt.self:
      traverse(self[n] as! ContinueStmt, notifying: &o)
    case DeclStmt.self:
      traverse(self[n] as! DeclStmt, notifying: &o)
    case DiscardStmt.self:
      traverse(self[n] as! DiscardStmt, notifying: &o)
    case DoWhileStmt.self:
      traverse(self[n] as! DoWhileStmt, notifying: &o)
    case ExprStmt.self:
      traverse(self[n] as! ExprStmt, notifying: &o)
    case ForStmt.self:
      traverse(self[n] as! ForStmt, notifying: &o)
    case ReturnStmt.self:
      traverse(self[n] as! ReturnStmt, notifying: &o)
    case WhileStmt.self:
      traverse(self[n] as! WhileStmt, notifying: &o)
    case YieldStmt.self:
      traverse(self[n] as! YieldStmt, notifying: &o)

    case MatchCase.self:
      traverse(self[n] as! MatchCase, notifying: &o)
    case TranslationUnit.self:
      traverse(self[n] as! TranslationUnit, notifying: &o)

    default:
      unreachable()
    }
    o.willExit(n, in: self)
  }

  /// Visits `n` and its children in pre-order, notifying `o` when a node is entered or left.
  public func walk<T: NodeIDProtocol, O: ASTWalkObserver>(_ n: T, notifying o: inout O) {
    walk(AnyNodeID(n), notifying: &o)
  }

  /// Visits `n` and its children in pre-order, notifying `o` when a node is entered or left.
  public func walk<T: NodeIDProtocol, O: ASTWalkObserver>(_ n: T?, notifying o: inout O) {
    n.map({ walk(AnyNodeID($0), notifying: &o) })
  }

  /// Visits all nodes in `roots` and their children in pre-order, notifying `o` when a node is
  /// entered or left.
  public func walk<T: NodeIDProtocol, O: ASTWalkObserver>(roots: [T], notifying o: inout O) {
    for r in roots { walk(AnyNodeID(r), notifying: &o) }
  }

  /// Visits all condition items in `i` and their children in pre-order, notifying `o` when a node
  /// is entered or left.
  public func walk<O: ASTWalkObserver>(conditionItems i: [ConditionItem], notifying o: inout O) {
    for x in i {
      switch x {
      case let .expr(e):
        walk(e, notifying: &o)
      case let .decl(d):
        walk(d, notifying: &o)
      }
    }
  }

  // MARK: Declarations

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: AssociatedTypeDecl, notifying o: inout O
  ) {
    walk(roots: n.conformances, notifying: &o)
    walk(n.defaultValue, notifying: &o)
    n.whereClause.map({ traverse(whereClause: $0.value, notifying: &o) })
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: AssociatedValueDecl, notifying o: inout O
  ) {
    walk(n.defaultValue, notifying: &o)
    n.whereClause.map({ traverse(whereClause: $0.value, notifying: &o) })
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: BindingDecl, notifying o: inout O
  ) {
    walk(n.pattern, notifying: &o)
    walk(n.initializer, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ConformanceDecl, notifying o: inout O
  ) {
    walk(n.subject, notifying: &o)
    walk(roots: n.conformances, notifying: &o)
    n.whereClause.map({ traverse(whereClause: $0.value, notifying: &o) })
    walk(roots: n.members, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ExtensionDecl, notifying o: inout O
  ) {
    walk(n.subject, notifying: &o)
    n.whereClause.map({ traverse(whereClause: $0.value, notifying: &o) })
    walk(roots: n.members, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: FunctionDecl, notifying o: inout O
  ) {
    n.genericClause.map({ traverse(genericClause: $0.value, notifying: &o) })
    walk(roots: n.explicitCaptures, notifying: &o)
    walk(roots: n.parameters, notifying: &o)
    walk(n.receiver, notifying: &o)
    walk(n.output, notifying: &o)
    n.body.map({ (b) in walk(b.base, notifying: &o) })
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: GenericParameterDecl, notifying o: inout O
  ) {
    walk(roots: n.conformances, notifying: &o)
    walk(n.defaultValue, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ImportDecl, notifying o: inout O
  ) {}

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: InitializerDecl, notifying o: inout O
  ) {
    n.genericClause.map({ traverse(genericClause: $0.value, notifying: &o) })
    walk(roots: n.parameters, notifying: &o)
    walk(n.receiver, notifying: &o)
    walk(n.body, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: MethodDecl, notifying o: inout O
  ) {
    n.genericClause.map({ traverse(genericClause: $0.value, notifying: &o) })
    walk(roots: n.parameters, notifying: &o)
    walk(n.output, notifying: &o)
    walk(roots: n.impls, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: MethodImpl, notifying o: inout O
  ) {
    walk(n.receiver, notifying: &o)
    n.body.map({ (b) in walk(b.base, notifying: &o) })
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ModuleDecl, notifying o: inout O
  ) {
    walk(roots: n.sources, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: NamespaceDecl, notifying o: inout O
  ) {
    walk(roots: n.members, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: OperatorDecl, notifying o: inout O
  ) {}

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ParameterDecl, notifying o: inout O
  ) {
    walk(n.annotation, notifying: &o)
    walk(n.defaultValue, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ProductTypeDecl, notifying o: inout O
  ) {
    n.genericClause.map({ traverse(genericClause: $0.value, notifying: &o) })
    walk(roots: n.conformances, notifying: &o)
    walk(roots: n.members, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: SubscriptDecl, notifying o: inout O
  ) {
    n.genericClause.map({ traverse(genericClause: $0.value, notifying: &o) })
    walk(roots: n.explicitCaptures, notifying: &o)
    walk(roots: n.parameters, notifying: &o)
    walk(n.output, notifying: &o)
    walk(roots: n.impls, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: SubscriptImpl, notifying o: inout O
  ) {
    walk(n.receiver, notifying: &o)
    n.body.map({ (b) in walk(b.base, notifying: &o) })
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: TraitDecl, notifying o: inout O
  ) {
    walk(roots: n.bounds, notifying: &o)
    walk(roots: n.members, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: TypeAliasDecl, notifying o: inout O
  ) {
    n.genericClause.map({ traverse(genericClause: $0.value, notifying: &o) })
    walk(n.aliasedType, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: VarDecl, notifying o: inout O
  ) {}

  // MARK: Expressions

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: BooleanLiteralExpr, notifying o: inout O
  ) {}

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: BufferLiteralExpr, notifying o: inout O
  ) {
    walk(roots: n.elements, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: CaptureExpr, notifying o: inout O
  ) {
    walk(n.source, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: CastExpr, notifying o: inout O
  ) {
    walk(n.left, notifying: &o)
    walk(n.right, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ConditionalExpr, notifying o: inout O
  ) {
    walk(conditionItems: n.condition, notifying: &o)
    walk(n.success, notifying: &o)
    walk(n.failure.value, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ConformanceLensExpr, notifying o: inout O
  ) {
    walk(n.subject, notifying: &o)
    walk(n.lens, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ExistentialTypeExpr, notifying o: inout O
  ) {
    walk(roots: n.traits, notifying: &o)
    n.whereClause.map({ traverse(whereClause: $0.value, notifying: &o) })
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: FloatLiteralExpr, notifying o: inout O
  ) {}

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: FunctionCallExpr, notifying o: inout O
  ) {
    walk(n.callee, notifying: &o)
    walk(roots: n.arguments.map(\.value), notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: InoutExpr, notifying o: inout O
  ) {
    walk(n.subject, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: IntegerLiteralExpr, notifying o: inout O
  ) {}

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: LambdaExpr, notifying o: inout O
  ) {
    walk(n.decl, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ArrowTypeExpr, notifying o: inout O
  ) {
    walk(n.environment, notifying: &o)
    walk(roots: n.parameters.map(\.type), notifying: &o)
    walk(n.output, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: MapLiteralExpr, notifying o: inout O
  ) {
    for e in n.elements {
      walk(e.key, notifying: &o)
      walk(e.value, notifying: &o)
    }
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: MatchExpr, notifying o: inout O
  ) {
    walk(n.subject, notifying: &o)
    walk(roots: n.cases, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: NameExpr, notifying o: inout O
  ) {
    if case .explicit(let e) = n.domain { walk(e, notifying: &o) }
    walk(roots: n.arguments.map(\.value), notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ParameterTypeExpr, notifying o: inout O
  ) {
    walk(n.bareType, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: PragmaLiteralExpr, notifying o: inout O
  ) {}

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: RemoteTypeExpr, notifying o: inout O
  ) {
    walk(n.operand, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: SequenceExpr, notifying o: inout O
  ) {
    walk(n.head, notifying: &o)
    for t in n.tail {
      walk(t.operator, notifying: &o)
      walk(t.operand, notifying: &o)
    }
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: SpawnExpr, notifying o: inout O
  ) {
    walk(n.decl, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: StringLiteralExpr, notifying o: inout O
  ) {}

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: SubscriptCallExpr, notifying o: inout O
  ) {
    walk(n.callee, notifying: &o)
    walk(roots: n.arguments.map(\.value), notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: TupleExpr, notifying o: inout O
  ) {
    walk(roots: n.elements.map(\.value), notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: TupleMemberExpr, notifying o: inout O
  ) {
    walk(n.tuple, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: TupleTypeExpr, notifying o: inout O
  ) {
    walk(roots: n.elements.map(\.type), notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: UnicodeScalarLiteralExpr, notifying o: inout O
  ) {}

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: WildcardExpr, notifying o: inout O
  ) {}

  // MARK: Patterns

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: BindingPattern, notifying o: inout O
  ) {
    walk(n.subpattern, notifying: &o)
    walk(n.annotation, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ExprPattern, notifying o: inout O
  ) {
    walk(n.expr, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: NamePattern, notifying o: inout O
  ) {
    walk(n.decl, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: OptionPattern, notifying o: inout O
  ) {
    walk(n.name, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: TuplePattern, notifying o: inout O
  ) {
    walk(roots: n.elements.map(\.pattern), notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: WildcardPattern, notifying o: inout O
  ) {}

  // MARK: Statements

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: AssignStmt, notifying o: inout O
  ) {
    walk(n.left, notifying: &o)
    walk(n.right, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: BraceStmt, notifying o: inout O
  ) {
    walk(roots: n.stmts, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: BreakStmt, notifying o: inout O
  ) {}

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ConditionalBindingStmt, notifying o: inout O
  ) {
    walk(n.binding, notifying: &o)
    walk(n.fallback, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ConditionalCompilationStmt, notifying o: inout O
  ) {
    walk(roots: n.expansion(for: compilationConditions), notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ConditionalStmt, notifying o: inout O
  ) {
    walk(conditionItems: n.condition, notifying: &o)
    walk(n.success, notifying: &o)
    walk(n.failure?.value, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ContinueStmt, notifying o: inout O
  ) {}

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: DeclStmt, notifying o: inout O
  ) {
    walk(n.decl, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: DiscardStmt, notifying o: inout O
  ) {
    walk(n.expr, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: DoWhileStmt, notifying o: inout O
  ) {
    walk(n.body, notifying: &o)
    walk(n.condition.value, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ExprStmt, notifying o: inout O
  ) {
    walk(n.expr, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ForStmt, notifying o: inout O
  ) {
    walk(n.binding, notifying: &o)
    walk(n.domain.value, notifying: &o)
    walk(n.filter?.value, notifying: &o)
    walk(n.body, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: ReturnStmt, notifying o: inout O
  ) {
    walk(n.value, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: WhileStmt, notifying o: inout O
  ) {
    walk(conditionItems: n.condition, notifying: &o)
    walk(n.body, notifying: &o)
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: YieldStmt, notifying o: inout O
  ) {
    walk(n.value, notifying: &o)
  }

  // MARK: Others

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: MatchCase, notifying o: inout O
  ) {
    walk(n.pattern, notifying: &o)
    walk(n.condition, notifying: &o)
    switch n.body {
    case .expr(let e):
      walk(e, notifying: &o)
    case .block(let s):
      walk(s, notifying: &o)
    }
  }

  /// Visits the children of `n` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    _ n: TranslationUnit, notifying o: inout O
  ) {
    walk(roots: n.decls, notifying: &o)
  }

  /// Visits the children of `c` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(genericClause c: GenericClause, notifying o: inout O) {
    walk(roots: c.parameters, notifying: &o)
    c.whereClause.map({ traverse(whereClause: $0.value, notifying: &o) })
  }

  /// Visits the children of `c` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(whereClause c: WhereClause, notifying o: inout O) {
    for k in c.constraints { traverse(constraintExpr: k.value, notifying: &o) }
  }

  /// Visits the children of `c` in pre-order, notifying `o` when a node is entered or left.
  public func traverse<O: ASTWalkObserver>(
    constraintExpr c: WhereClause.ConstraintExpr, notifying o: inout O
  ) {
    switch c {
    case .bound(let lhs, let traits):
      walk(lhs, notifying: &o)
      for t in traits { walk(t, notifying: &o) }
    case .equality(let lhs, let rhs):
      walk(lhs, notifying: &o)
      walk(rhs, notifying: &o)
    case .value(let e):
      walk(e, notifying: &o)
    }
  }

}
