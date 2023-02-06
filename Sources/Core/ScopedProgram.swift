import Utils

/// A data structure representing a scoped Val program ready to be type checked.
public struct ScopedProgram: Program {

  public let ast: AST

  public private(set) var scopeToParent = ASTProperty<AnyScopeID>()

  public private(set) var scopeToDecls = ASTProperty<[AnyDeclID]>()

  public private(set) var declToScope = DeclProperty<AnyScopeID>()

  public private(set) var varToBinding: [NodeID<VarDecl>: NodeID<BindingDecl>] = [:]

  /// Creates a scoped program from an AST.
  public init(_ ast: AST) {
    self.ast = ast

    // Establish the scope relationships.
    for module in ast.modules {
      var state = VisitorState(module: module)
      visit(moduleDecl: module, withState: &state)
    }
  }

}

// MARK: Construction of scope relationships

extension ScopedProgram {

  /// A data structure representing the state of the visitor building scope relationships.
  private struct VisitorState {

    /// The ID of the innermost lexical scope currently visited.
    var innermost: AnyScopeID

    /// The ID of the binding declaration currently visited, if any.
    var bindingDeclBeingVisited: NodeID<BindingDecl>?

    init(module: NodeID<ModuleDecl>) {
      self.innermost = AnyScopeID(module)
      self.bindingDeclBeingVisited = nil
    }

  }

  /// Inserts `decl` into `scope`.
  private mutating func insert<T: DeclID>(decl: T, into scope: AnyScopeID) {
    let child = AnyDeclID(decl)

    if let parent = declToScope[child] {
      if parent == scope {
        // The relation is already established, we're done.
        return
      } else {
        // Remove the existing edge scope container to containee.
        scopeToDecls[scope]?.removeAll(where: { $0 == child })
      }
    }

    // Create the edges.
    declToScope[child] = scope
    scopeToDecls[scope, default: []].append(child)
  }

  /// Sets `scope` as the parent of the current innermost lexical scope and calls `action` with a
  /// a mutable projection of `self` and `state` where `scope` is the innermost lexical scope.
  private mutating func nesting<T: ScopeID>(
    in scope: T,
    withState state: inout VisitorState,
    _ action: (inout ScopedProgram, inout VisitorState) -> Void
  ) {
    let currentInnermost = state.innermost
    let newInnermost = AnyScopeID(scope)
    scopeToParent[newInnermost] = currentInnermost
    state.innermost = newInnermost

    action(&self, &state)

    state.innermost = currentInnermost
  }

  // MARK: Declarations

  private mutating func visit(decl: AnyDeclID, withState state: inout VisitorState) {
    switch decl.kind {
    case AssociatedTypeDecl.self:
      visit(associatedTypeDecl: NodeID(decl)!, withState: &state)
    case AssociatedValueDecl.self:
      visit(associatedValueDecl: NodeID(decl)!, withState: &state)
    case BindingDecl.self:
      visit(bindingDecl: NodeID(decl)!, withState: &state)
    case ConformanceDecl.self:
      visit(conformanceDecl: NodeID(decl)!, withState: &state)
    case ExtensionDecl.self:
      visit(extensionDecl: NodeID(decl)!, withState: &state)
    case FunctionDecl.self:
      visit(functionDecl: NodeID(decl)!, withState: &state)
    case GenericParameterDecl.self:
      visit(genericParameterDecl: NodeID(decl)!, withState: &state)
    case ImportDecl.self:
      visit(importDecl: NodeID(decl)!, withState: &state)
    case InitializerDecl.self:
      visit(initializerDecl: NodeID(decl)!, withState: &state)
    case MethodDecl.self:
      visit(methodDecl: NodeID(decl)!, withState: &state)
    case MethodImpl.self:
      visit(methodImpl: NodeID(decl)!, withState: &state)
    case ModuleDecl.self:
      visit(moduleDecl: NodeID(decl)!, withState: &state)
    case NamespaceDecl.self:
      visit(namespaceDecl: NodeID(decl)!, withState: &state)
    case OperatorDecl.self:
      visit(operatorDecl: NodeID(decl)!, withState: &state)
    case ParameterDecl.self:
      visit(parameterDecl: NodeID(decl)!, withState: &state)
    case ProductTypeDecl.self:
      visit(productTypeDecl: NodeID(decl)!, withState: &state)
    case SubscriptDecl.self:
      visit(subscriptDecl: NodeID(decl)!, withState: &state)
    case SubscriptImpl.self:
      visit(subscriptImpl: NodeID(decl)!, withState: &state)
    case TraitDecl.self:
      visit(traitDecl: NodeID(decl)!, withState: &state)
    case TypeAliasDecl.self:
      visit(typeAliasDecl: NodeID(decl)!, withState: &state)
    case VarDecl.self:
      visit(varDecl: NodeID(decl)!, withState: &state)
    default:
      unexpected("declaration", found: decl, of: ast)
    }
  }

  private mutating func visit(
    associatedTypeDecl decl: NodeID<AssociatedTypeDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    for conformance in ast[decl].conformances {
      visit(nameExpr: conformance, withState: &state)
    }
    if let defaultValue = ast[decl].defaultValue {
      visit(expr: defaultValue, withState: &state)
    }
    if let clause = ast[decl].whereClause?.value {
      visit(whereClause: clause, withState: &state)
    }
  }

  private mutating func visit(
    associatedValueDecl decl: NodeID<AssociatedValueDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    if let defaultValue = ast[decl].defaultValue {
      visit(expr: defaultValue, withState: &state)
    }
    if let clause = ast[decl].whereClause?.value {
      visit(whereClause: clause, withState: &state)
    }
  }

  private mutating func visit(
    bindingDecl decl: NodeID<BindingDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    let currentBindingDeclBeingVisited = state.bindingDeclBeingVisited
    state.bindingDeclBeingVisited = decl

    visit(bindingPattern: ast[decl].pattern, withState: &state)
    if let initializer = ast[decl].initializer {
      visit(expr: initializer, withState: &state)
    }

    state.bindingDeclBeingVisited = currentBindingDeclBeingVisited
  }

  private mutating func visit(
    conformanceDecl decl: NodeID<ConformanceDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    nesting(
      in: decl, withState: &state,
      { (this, state) in
        this.visit(expr: this.ast[decl].subject, withState: &state)
        if let clause = this.ast[decl].whereClause?.value {
          this.visit(whereClause: clause, withState: &state)
        }
        for member in this.ast[decl].members {
          this.visit(decl: member, withState: &state)
        }
      })
  }

  private mutating func visit(
    extensionDecl decl: NodeID<ExtensionDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    nesting(
      in: decl, withState: &state,
      { (this, state) in
        this.visit(expr: this.ast[decl].subject, withState: &state)
        if let clause = this.ast[decl].whereClause?.value {
          this.visit(whereClause: clause, withState: &state)
        }
        for member in this.ast[decl].members {
          this.visit(decl: member, withState: &state)
        }
      })
  }

  private mutating func visit(
    functionDecl decl: NodeID<FunctionDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    nesting(
      in: decl, withState: &state,
      { (this, state) in
        if let clause = this.ast[decl].genericClause?.value {
          this.visit(genericClause: clause, withState: &state)
        }
        for capture in this.ast[decl].explicitCaptures {
          this.visit(bindingDecl: capture, withState: &state)
        }
        for parameter in this.ast[decl].parameters {
          this.visit(parameterDecl: parameter, withState: &state)
        }
        if let receiver = this.ast[decl].receiver {
          this.visit(parameterDecl: receiver, withState: &state)
        }
        if let output = this.ast[decl].output {
          this.visit(expr: output, withState: &state)
        }

        switch this.ast[decl].body {
        case let .expr(expr):
          this.visit(expr: expr, withState: &state)

        case let .block(stmt):
          this.visit(braceStmt: stmt, withState: &state)

        case nil:
          break
        }
      })
  }

  private mutating func visit(
    genericParameterDecl decl: NodeID<GenericParameterDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    for conformance in ast[decl].conformances {
      visit(nameExpr: conformance, withState: &state)
    }
    if let defaultValue = ast[decl].defaultValue {
      visit(expr: defaultValue, withState: &state)
    }
  }

  private mutating func visit(
    importDecl decl: NodeID<ImportDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)
  }

  private mutating func visit(
    initializerDecl decl: NodeID<InitializerDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    nesting(
      in: decl, withState: &state,
      { (this, state) in
        if let clause = this.ast[decl].genericClause?.value {
          this.visit(genericClause: clause, withState: &state)
        }
        for parameter in this.ast[decl].parameters {
          this.visit(parameterDecl: parameter, withState: &state)
        }
        this.visit(parameterDecl: this.ast[decl].receiver, withState: &state)
        if let body = this.ast[decl].body {
          this.visit(braceStmt: body, withState: &state)
        }
      })
  }

  private mutating func visit(
    methodDecl decl: NodeID<MethodDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    nesting(
      in: decl, withState: &state,
      { (this, state) in
        if let clause = this.ast[decl].genericClause?.value {
          this.visit(genericClause: clause, withState: &state)
        }
        for parameter in this.ast[decl].parameters {
          this.visit(parameterDecl: parameter, withState: &state)
        }
        if let output = this.ast[decl].output {
          this.visit(expr: output, withState: &state)
        }
        for impl in this.ast[decl].impls {
          this.visit(methodImpl: impl, withState: &state)
        }
      })
  }

  private mutating func visit(
    methodImpl decl: NodeID<MethodImpl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    nesting(
      in: decl, withState: &state,
      { (this, state) in
        this.visit(parameterDecl: this.ast[decl].receiver, withState: &state)

        switch this.ast[decl].body {
        case let .expr(expr):
          this.visit(expr: expr, withState: &state)

        case let .block(stmt):
          this.visit(braceStmt: stmt, withState: &state)

        case nil:
          break
        }
      })
  }

  private mutating func visit(
    moduleDecl decl: NodeID<ModuleDecl>,
    withState state: inout VisitorState
  ) {
    precondition(state.innermost == decl)
    for source in ast[decl].sources {
      visit(topLevelDeclSet: source, withState: &state)
    }
  }

  private mutating func visit(
    namespaceDecl decl: NodeID<NamespaceDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    nesting(
      in: decl, withState: &state,
      { (this, state) in
        for member in this.ast[decl].members {
          this.visit(decl: member, withState: &state)
        }
      })
  }

  private mutating func visit(
    operatorDecl decl: NodeID<OperatorDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)
  }

  private mutating func visit(
    parameterDecl decl: NodeID<ParameterDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    if let annotation = ast[decl].annotation {
      visit(parameterTypeExpr: annotation, withState: &state)
    }
    if let defaultValue = ast[decl].defaultValue {
      visit(expr: defaultValue, withState: &state)
    }
  }

  private mutating func visit(
    productTypeDecl decl: NodeID<ProductTypeDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    nesting(
      in: decl, withState: &state,
      { (this, state) in
        if let clause = this.ast[decl].genericClause?.value {
          this.visit(genericClause: clause, withState: &state)
        }
        for conformance in this.ast[decl].conformances {
          this.visit(nameExpr: conformance, withState: &state)
        }
        for member in this.ast[decl].members {
          this.visit(decl: member, withState: &state)
        }
      })
  }

  private mutating func visit(
    subscriptDecl decl: NodeID<SubscriptDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    nesting(
      in: decl, withState: &state,
      { (this, state) in
        if let clause = this.ast[decl].genericClause?.value {
          this.visit(genericClause: clause, withState: &state)
        }
        for capture in this.ast[decl].explicitCaptures {
          this.visit(bindingDecl: capture, withState: &state)
        }
        for parameter in this.ast[decl].parameters ?? [] {
          this.visit(parameterDecl: parameter, withState: &state)
        }
        this.visit(expr: this.ast[decl].output, withState: &state)
        for impl in this.ast[decl].impls {
          this.visit(subscriptImpl: impl, withState: &state)
        }
      })
  }

  private mutating func visit(
    subscriptImpl decl: NodeID<SubscriptImpl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    nesting(
      in: decl, withState: &state,
      { (this, state) in
        if let receiver = this.ast[decl].receiver {
          this.visit(parameterDecl: receiver, withState: &state)
        }

        switch this.ast[decl].body {
        case let .expr(expr):
          this.visit(expr: expr, withState: &state)

        case let .block(stmt):
          this.visit(braceStmt: stmt, withState: &state)

        case nil:
          break
        }
      })
  }

  private mutating func visit(
    traitDecl decl: NodeID<TraitDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    nesting(
      in: decl, withState: &state,
      { (this, state) in
        for refinement in this.ast[decl].refinements {
          this.visit(nameExpr: refinement, withState: &state)
        }
        for member in this.ast[decl].members {
          this.visit(decl: member, withState: &state)
        }
      })
  }

  private mutating func visit(
    typeAliasDecl decl: NodeID<TypeAliasDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)

    nesting(
      in: decl, withState: &state,
      { (this, state) in
        if let clause = this.ast[decl].genericClause?.value {
          this.visit(genericClause: clause, withState: &state)
        }
        switch this.ast[decl].body {
        case .typeExpr(let expr):
          this.visit(expr: expr, withState: &state)

        case .union(let union):
          for element in union {
            this.visit(productTypeDecl: element, withState: &state)
          }
        }
      })
  }

  private mutating func visit(
    varDecl decl: NodeID<VarDecl>,
    withState state: inout VisitorState
  ) {
    insert(decl: decl, into: state.innermost)
    varToBinding[decl] = state.bindingDeclBeingVisited
  }

  private mutating func visit(
    topLevelDeclSet: NodeID<TranslationUnit>,
    withState state: inout VisitorState
  ) {
    nesting(
      in: topLevelDeclSet, withState: &state,
      { (this, state) in
        for member in this.ast[topLevelDeclSet].decls {
          this.visit(decl: member, withState: &state)
        }
      })
  }

  // MARK: Expressions

  private mutating func visit(expr: AnyExprID, withState state: inout VisitorState) {
    switch expr.kind {
    case BooleanLiteralExpr.self:
      break
    case BufferLiteralExpr.self:
      visit(bufferLiteralExpr: NodeID(expr)!, withState: &state)
    case CastExpr.self:
      visit(castExpr: NodeID(expr)!, withState: &state)
    case CondExpr.self:
      visit(condExpr: NodeID(expr)!, withState: &state)
    case ConformanceLensTypeExpr.self:
      visit(conformanceLensTypeExpr: NodeID(expr)!, withState: &state)
    case ErrorExpr.self:
      break
    case ExistentialTypeExpr.self:
      visit(existentialTypeExpr: NodeID(expr)!, withState: &state)
    case FloatLiteralExpr.self:
      break
    case FunctionCallExpr.self:
      visit(functionCallExpr: NodeID(expr)!, withState: &state)
    case InoutExpr.self:
      visit(inoutExpr: NodeID(expr)!, withState: &state)
    case IntegerLiteralExpr.self:
      break
    case LambdaExpr.self:
      visit(lambdaExpr: NodeID(expr)!, withState: &state)
    case LambdaTypeExpr.self:
      visit(lambdaTypeExpr: NodeID(expr)!, withState: &state)
    case MapLiteralExpr.self:
      visit(mapLiteralExpr: NodeID(expr)!, withState: &state)
    case MatchExpr.self:
      visit(matchExpr: NodeID(expr)!, withState: &state)
    case NameExpr.self:
      visit(nameExpr: NodeID(expr)!, withState: &state)
    case NilLiteralExpr.self:
      break
    case ParameterTypeExpr.self:
      visit(parameterTypeExpr: NodeID(expr)!, withState: &state)
    case RemoteTypeExpr.self:
      visit(storedProjectionTypeExpr: NodeID(expr)!, withState: &state)
    case SequenceExpr.self:
      visit(sequenceExpr: NodeID(expr)!, withState: &state)
    case SpawnExpr.self:
      visit(spawnExpr: NodeID(expr)!, withState: &state)
    case StringLiteralExpr.self:
      break
    case SubscriptCallExpr.self:
      visit(subscriptCallExpr: NodeID(expr)!, withState: &state)
    case TupleExpr.self:
      visit(tupleExpr: NodeID(expr)!, withState: &state)
    case TupleMemberExpr.self:
      visit(tupleMemberExpr: NodeID(expr)!, withState: &state)
    case TupleTypeExpr.self:
      visit(tupleTypeExpr: NodeID(expr)!, withState: &state)
    case UnicodeScalarLiteralExpr.self:
      break
    case UnionTypeExpr.self:
      visit(unionTypeExpr: NodeID(expr)!, withState: &state)
    case WildcardExpr.self:
      break
    default:
      unexpected("expression", found: expr, of: ast)
    }
  }

  private mutating func visit(
    bufferLiteralExpr expr: NodeID<BufferLiteralExpr>,
    withState state: inout VisitorState
  ) {
    for element in ast[expr].elements {
      visit(expr: element, withState: &state)
    }
  }

  private mutating func visit(
    castExpr expr: NodeID<CastExpr>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[expr].left, withState: &state)
    visit(expr: ast[expr].right, withState: &state)
  }

  private mutating func visit(
    condExpr expr: NodeID<CondExpr>,
    withState state: inout VisitorState
  ) {
    nesting(
      in: expr, withState: &state,
      { (this, state) in
        for item in this.ast[expr].condition {
          switch item {
          case let .expr(i):
            this.visit(expr: i, withState: &state)
          case let .decl(i):
            this.visit(bindingDecl: i, withState: &state)
          }
        }

        switch this.ast[expr].success {
        case let .expr(i):
          this.visit(expr: i, withState: &state)
        case let .block(i):
          this.visit(braceStmt: i, withState: &state)
        }

        switch this.ast[expr].failure {
        case let .expr(i):
          this.visit(expr: i, withState: &state)
        case let .block(i):
          this.visit(braceStmt: i, withState: &state)
        case nil:
          break
        }
      })
  }

  private mutating func visit(
    conformanceLensTypeExpr expr: NodeID<ConformanceLensTypeExpr>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[expr].subject, withState: &state)
    visit(expr: ast[expr].lens, withState: &state)
  }

  private mutating func visit(
    existentialTypeExpr expr: NodeID<ExistentialTypeExpr>,
    withState state: inout VisitorState
  ) {
    for trait in ast[expr].traits {
      visit(nameExpr: trait, withState: &state)
    }
    if let clause = ast[expr].whereClause?.value {
      visit(whereClause: clause, withState: &state)
    }
  }

  private mutating func visit(
    functionCallExpr expr: NodeID<FunctionCallExpr>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[expr].callee, withState: &state)
    for argument in ast[expr].arguments {
      visit(expr: argument.value, withState: &state)
    }
  }

  private mutating func visit(
    inoutExpr expr: NodeID<InoutExpr>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[expr].subject, withState: &state)
  }

  private mutating func visit(
    lambdaExpr expr: NodeID<LambdaExpr>,
    withState state: inout VisitorState
  ) {
    visit(functionDecl: ast[expr].decl, withState: &state)
  }

  private mutating func visit(
    lambdaTypeExpr expr: NodeID<LambdaTypeExpr>,
    withState state: inout VisitorState
  ) {
    if let environment = ast[expr].environment {
      visit(expr: environment, withState: &state)
    }
    for parameter in ast[expr].parameters {
      visit(parameterTypeExpr: parameter.type, withState: &state)
    }
    visit(expr: ast[expr].output, withState: &state)
  }

  private mutating func visit(
    mapLiteralExpr expr: NodeID<MapLiteralExpr>,
    withState state: inout VisitorState
  ) {
    for element in ast[expr].elements {
      visit(expr: element.key, withState: &state)
      visit(expr: element.value, withState: &state)
    }
  }

  private mutating func visit(
    matchExpr expr: NodeID<MatchExpr>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[expr].subject, withState: &state)
    for case_ in ast[expr].cases {
      nesting(
        in: case_, withState: &state,
        { (this, state) in
          this.visit(pattern: this.ast[case_].pattern, withState: &state)
          if let condition = this.ast[case_].condition {
            this.visit(expr: condition, withState: &state)
          }

          switch this.ast[case_].body {
          case let .expr(i):
            this.visit(expr: i, withState: &state)
          case let .block(i):
            this.visit(braceStmt: i, withState: &state)
          }
        })
    }
  }

  private mutating func visit(
    nameExpr expr: NodeID<NameExpr>,
    withState state: inout VisitorState
  ) {
    if case let .expr(domain) = ast[expr].domain {
      visit(expr: domain, withState: &state)
    }
    for argument in ast[expr].arguments {
      visit(expr: argument.value, withState: &state)
    }
  }

  private mutating func visit(
    parameterTypeExpr expr: NodeID<ParameterTypeExpr>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[expr].bareType, withState: &state)
  }

  private mutating func visit(
    storedProjectionTypeExpr expr: NodeID<RemoteTypeExpr>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[expr].operand, withState: &state)
  }

  private mutating func visit(
    sequenceExpr expr: NodeID<SequenceExpr>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[expr].head, withState: &state)
    for element in ast[expr].tail {
      visit(nameExpr: element.operator, withState: &state)
      visit(expr: element.operand, withState: &state)
    }
  }

  private mutating func visit(
    spawnExpr expr: NodeID<SpawnExpr>,
    withState state: inout VisitorState
  ) {
    visit(functionDecl: ast[expr].decl, withState: &state)
  }

  private mutating func visit(
    subscriptCallExpr expr: NodeID<SubscriptCallExpr>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[expr].callee, withState: &state)
    for argument in ast[expr].arguments {
      visit(expr: argument.value, withState: &state)
    }
  }

  private mutating func visit(
    tupleExpr expr: NodeID<TupleExpr>,
    withState state: inout VisitorState
  ) {
    for element in ast[expr].elements {
      visit(expr: element.value, withState: &state)
    }
  }

  private mutating func visit(
    tupleMemberExpr expr: NodeID<TupleMemberExpr>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[expr].tuple, withState: &state)
  }

  private mutating func visit(
    tupleTypeExpr expr: NodeID<TupleTypeExpr>,
    withState state: inout VisitorState
  ) {
    for element in ast[expr].elements {
      visit(expr: element.type, withState: &state)
    }
  }

  private mutating func visit(
    unionTypeExpr expr: NodeID<UnionTypeExpr>,
    withState state: inout VisitorState
  ) {
    for element in ast[expr].elements {
      visit(expr: element, withState: &state)
    }
  }

  // MARK: Patterns

  private mutating func visit(pattern: AnyPatternID, withState state: inout VisitorState) {
    switch pattern.kind {
    case BindingPattern.self:
      visit(bindingPattern: NodeID(pattern)!, withState: &state)
    case ExprPattern.self:
      visit(exprPattern: NodeID(pattern)!, withState: &state)
    case NamePattern.self:
      visit(namePattern: NodeID(pattern)!, withState: &state)
    case TuplePattern.self:
      visit(tuplePattern: NodeID(pattern)!, withState: &state)
    case WildcardPattern.self:
      break
    default:
      unexpected("pattern", found: pattern, of: ast)
    }
  }

  private mutating func visit(
    bindingPattern pattern: NodeID<BindingPattern>,
    withState state: inout VisitorState
  ) {
    visit(pattern: ast[pattern].subpattern, withState: &state)
    if let annotation = ast[pattern].annotation {
      visit(expr: annotation, withState: &state)
    }
  }

  private mutating func visit(
    exprPattern pattern: NodeID<ExprPattern>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[pattern].expr, withState: &state)
  }

  private mutating func visit(
    namePattern pattern: NodeID<NamePattern>,
    withState state: inout VisitorState
  ) {
    visit(varDecl: ast[pattern].decl, withState: &state)
  }

  private mutating func visit(
    tuplePattern pattern: NodeID<TuplePattern>,
    withState state: inout VisitorState
  ) {
    for element in ast[pattern].elements {
      visit(pattern: element.pattern, withState: &state)
    }
  }

  // MARK: Statements

  private mutating func visit(stmt: AnyStmtID, withState state: inout VisitorState) {
    switch stmt.kind {
    case AssignStmt.self:
      visit(assignStmt: NodeID(stmt)!, withState: &state)
    case BraceStmt.self:
      visit(braceStmt: NodeID(stmt)!, withState: &state)
    case BraceStmt.self:
      break
    case BreakStmt.self:
      break
    case CondBindingStmt.self:
      visit(condBindingStmt: NodeID(stmt)!, withState: &state)
    case ContinueStmt.self:
      break
    case DeclStmt.self:
      visit(declStmt: NodeID(stmt)!, withState: &state)
    case DiscardStmt.self:
      visit(discardStmt: NodeID(stmt)!, withState: &state)
    case DoWhileStmt.self:
      visit(doWhileStmt: NodeID(stmt)!, withState: &state)
    case ExprStmt.self:
      visit(exprStmt: NodeID(stmt)!, withState: &state)
    case ForStmt.self:
      visit(forStmt: NodeID(stmt)!, withState: &state)
    case ReturnStmt.self:
      visit(returnStmt: NodeID(stmt)!, withState: &state)
    case WhileStmt.self:
      visit(whileStmt: NodeID(stmt)!, withState: &state)
    case YieldStmt.self:
      visit(yieldStmt: NodeID(stmt)!, withState: &state)
    default:
      unexpected("statement", found: stmt, of: ast)
    }
  }

  private mutating func visit(
    assignStmt stmt: NodeID<AssignStmt>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[stmt].left, withState: &state)
    visit(expr: ast[stmt].right, withState: &state)
  }

  private mutating func visit(
    braceStmt stmt: NodeID<BraceStmt>,
    withState state: inout VisitorState
  ) {
    nesting(
      in: stmt, withState: &state,
      { (this, state) in
        for i in this.ast[stmt].stmts {
          this.visit(stmt: i, withState: &state)
        }
      })
  }

  private mutating func visit(
    condBindingStmt stmt: NodeID<CondBindingStmt>,
    withState state: inout VisitorState
  ) {
    visit(bindingDecl: ast[stmt].binding, withState: &state)
    switch ast[stmt].fallback {
    case .expr(let i):
      visit(expr: i, withState: &state)
    case .exit(let i):
      visit(stmt: i, withState: &state)
    }
  }

  private mutating func visit(
    declStmt stmt: NodeID<DeclStmt>,
    withState state: inout VisitorState
  ) {
    visit(decl: ast[stmt].decl, withState: &state)
  }

  private mutating func visit(
    discardStmt stmt: NodeID<DiscardStmt>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[stmt].expr, withState: &state)
  }

  private mutating func visit(
    doWhileStmt stmt: NodeID<DoWhileStmt>,
    withState state: inout VisitorState
  ) {
    visit(braceStmt: ast[stmt].body, withState: &state)

    // Visit the condition of the loop in the same lexical scope as the body.
    let currentInnermost = state.innermost
    state.innermost = AnyScopeID(ast[stmt].body)
    visit(expr: ast[stmt].condition, withState: &state)
    state.innermost = currentInnermost
  }

  private mutating func visit(
    exprStmt stmt: NodeID<ExprStmt>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[stmt].expr, withState: &state)
  }

  private mutating func visit(
    forStmt stmt: NodeID<ForStmt>,
    withState state: inout VisitorState
  ) {
    nesting(
      in: stmt, withState: &state,
      { (this, state) in
        this.visit(bindingDecl: this.ast[stmt].binding, withState: &state)
        if let filter = this.ast[stmt].filter {
          this.visit(expr: filter, withState: &state)
        }
        this.visit(braceStmt: this.ast[stmt].body, withState: &state)
      })
  }

  private mutating func visit(
    returnStmt stmt: NodeID<ReturnStmt>,
    withState state: inout VisitorState
  ) {
    if let value = ast[stmt].value {
      visit(expr: value, withState: &state)
    }
  }

  private mutating func visit(
    whileStmt stmt: NodeID<WhileStmt>,
    withState state: inout VisitorState
  ) {
    nesting(
      in: stmt, withState: &state,
      { (this, state) in
        for item in this.ast[stmt].condition {
          switch item {
          case let .expr(i):
            this.visit(expr: i, withState: &state)
          case let .decl(i):
            this.visit(bindingDecl: i, withState: &state)
          }
        }

        this.visit(braceStmt: this.ast[stmt].body, withState: &state)
      })
  }

  private mutating func visit(
    yieldStmt stmt: NodeID<YieldStmt>,
    withState state: inout VisitorState
  ) {
    visit(expr: ast[stmt].value, withState: &state)
  }

  // MARK: Others

  private mutating func visit(
    genericClause clause: GenericClause,
    withState state: inout VisitorState
  ) {
    for parameter in clause.parameters {
      visit(genericParameterDecl: parameter, withState: &state)
    }
    if let whereClause = clause.whereClause?.value {
      visit(whereClause: whereClause, withState: &state)
    }
  }

  private mutating func visit(
    whereClause clause: WhereClause,
    withState state: inout VisitorState
  ) {
    for constraint in clause.constraints {
      switch constraint.value {
      case .conformance(let lhs, let traits):
        visit(nameExpr: lhs, withState: &state)
        for trait in traits {
          visit(nameExpr: trait, withState: &state)
        }

      case .equality(let lhs, let rhs):
        visit(nameExpr: lhs, withState: &state)
        visit(expr: rhs, withState: &state)

      case .value(let expr):
        visit(expr: expr, withState: &state)
      }
    }
  }

}
