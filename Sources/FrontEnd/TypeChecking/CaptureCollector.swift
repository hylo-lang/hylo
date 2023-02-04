import Core
import Utils

/// An AST visitor that collects the names that occur free in a function.
struct CaptureCollector {

  /// A dictionary mapping free names to their occurrences and the kind of use.
  typealias FreeSet = [Name: (occurences: [NodeID<NameExpr>], capability: AccessEffect)]

  /// The AST containing the nodes being visited.
  let ast: AST

  /// A flag that indicates whether yield statements are supposed to project mutable objects.
  private let isYieldMutating = false

  /// The set of names that are bound in the lexical scopes containing the node being visited.
  private var boundNames: [Set<Name>]

  init(ast: AST, ignoring names: Set<Name> = []) {
    self.ast = ast
    self.boundNames = [names]
  }

  /// Returns the names occurring free in the specified function or subscript declaration, together
  /// with the list of their occurrences and a flag indicating whether the they are used mutably.
  ///
  /// - Requires: `id` must denote a function or subscript declaration.
  mutating func freeNames<T: Decl>(in id: NodeID<T>) -> FreeSet {
    var captures: FreeSet = [:]
    switch id.kind {
    case FunctionDecl.self:
      let d = NodeID<FunctionDecl>(id)!
      collectCaptures(ofFunction: d, includingExplicitCaptures: true, into: &captures)

    case SubscriptDecl.self:
      fatalError("not implemented")

    default:
      unexpected("declaration", found: id, of: ast)
    }

    return captures
  }

  /// Records `occurence` as a use of `name` with the specified capability, unless `name` belongs
  /// the the set of bound identifiers.
  ///
  /// - Parameters:
  ///   - capability: The capability of the use; must be either `let` or `inout`.
  private mutating func record(
    occurrence: NodeID<NameExpr>,
    withCapability capability: AccessEffect,
    ifFree name: Name,
    into captures: inout FreeSet
  ) {
    precondition(capability == .let || capability == .inout)
    if !boundNames.contains(where: { $0.contains(name) }) {
      modifying(
        &captures[name, default: ([], .let)],
        { entry in
          entry.occurences.append(occurrence)
          if capability == .inout {
            entry.capability = capability
          }
        })
    }
  }

  /// Collects the names occurring free in the specified declaration.
  private mutating func collectCaptures<T: DeclID>(
    ofDecl id: T,
    into captures: inout FreeSet
  ) {
    switch id.kind {
    case BindingDecl.self:
      collectCaptures(ofBinding: NodeID(id)!, into: &captures)
    case FunctionDecl.self:
      collectCaptures(
        ofFunction: NodeID(id)!,
        includingExplicitCaptures: false,
        into: &captures)
    default:
      unexpected("declaration", found: id, of: ast)
    }
  }

  private mutating func collectCaptures(
    ofBinding id: NodeID<BindingDecl>,
    into captures: inout FreeSet
  ) {
    // Note: the names introduced by that declaration are not yet available in the pattern and
    // initializer of the declaration.
    collectCaptures(ofBindingPattern: ast[id].pattern, into: &captures)
    if let initializer = ast[id].initializer {
      collectCaptures(
        ofExpr: initializer,
        into: &captures,
        inMutatingContext: ast[ast[id].pattern].introducer.value == .inout)
    }

    // Add the names introduced by the declaration to the set of bound names.
    for (_, pattern) in ast.names(in: ast[id].pattern) {
      boundNames[boundNames.count - 1].insert(Name(stem: ast[ast[pattern].decl].baseName))
    }
  }

  private mutating func collectCaptures(
    ofFunction id: NodeID<FunctionDecl>,
    includingExplicitCaptures areExplicitCapturesIncluded: Bool,
    into captures: inout FreeSet
  ) {
    var newNames: Set<Name> = []

    // Visit the signatures and default values of the parameters and collect their names.
    for parameter in ast[id].parameters {
      if let annotation = ast[parameter].annotation {
        collectCaptures(ofParameter: annotation, into: &captures)
      }
      if let defaultValue = ast[parameter].defaultValue {
        collectCaptures(ofExpr: defaultValue, into: &captures, inMutatingContext: false)
      }
      newNames.insert(Name(stem: ast[parameter].baseName))
    }

    // Visit the type and value expressions in the generic clause and collect the names of the
    // generic parameters.
    if let genericClause = ast[id].genericClause?.value {
      if let whereClause = genericClause.whereClause?.value {
        for constraint in whereClause.constraints {
          if case .equality(let l, let r) = constraint.value {
            collectCaptures(ofTypeExpr: l, into: &captures)
            collectCaptures(ofTypeExpr: r, into: &captures)
          }
        }
      }

      for parameter in genericClause.parameters {
        if let value = ast[parameter].defaultValue {
          collectCaptures(ofExpr: value, into: &captures, inMutatingContext: false)
        }
        newNames.insert(Name(stem: ast[parameter].baseName))
      }
    }

    // Visit the values of the captures and collect their names.
    for capture in ast[id].explicitCaptures {
      if let initializer = ast[capture].initializer {
        collectCaptures(ofExpr: initializer, into: &captures, inMutatingContext: false)
      }
      if !areExplicitCapturesIncluded {
        for (_, pattern) in ast.names(in: ast[capture].pattern) {
          newNames.insert(Name(stem: ast[ast[pattern].decl].baseName))
        }
      }
    }

    // Visit the function's body.
    boundNames.append(newNames)
    switch ast[id].body {
    case .expr(let expr):
      collectCaptures(ofExpr: expr, into: &captures, inMutatingContext: false)
    case .block(let stmt):
      collectCaptures(ofBrace: stmt, into: &captures)
    default:
      break
    }
    boundNames.removeLast()
  }

  /// Collects the names occurring free in the specified expression.
  private mutating func collectCaptures<T: ExprID>(
    ofExpr id: T,
    into captures: inout FreeSet,
    inMutatingContext isContextMutating: Bool
  ) {
    switch id.kind {
    case CastExpr.self:
      collectCaptures(
        ofCast: NodeID(id)!,
        into: &captures,
        inMutatingContext: isContextMutating)

    case CondExpr.self:
      collectCaptures(
        ofCond: NodeID(id)!,
        into: &captures,
        inMutatingContext: isContextMutating)

    case FunctionCallExpr.self:
      collectCaptures(
        ofFunctionCall: NodeID(id)!,
        into: &captures,
        inMutatingContext: isContextMutating)

    case InoutExpr.self:
      collectCaptures(
        ofInout: NodeID(id)!,
        into: &captures,
        inMutatingContext: isContextMutating)

    case NameExpr.self:
      collectCaptures(
        ofName: NodeID(id)!,
        into: &captures,
        inMutatingContext: isContextMutating)

    case SequenceExpr.self:
      collectCaptures(
        ofSequence: NodeID(id)!,
        into: &captures,
        inMutatingContext: isContextMutating)

    case TupleExpr.self:
      collectCaptures(
        ofTuple: NodeID(id)!,
        into: &captures,
        inMutatingContext: isContextMutating)

    case TupleMemberExpr.self:
      collectCaptures(
        ofTupleMember: NodeID(id)!,
        into: &captures,
        inMutatingContext: isContextMutating)

    case BooleanLiteralExpr.self,
      UnicodeScalarLiteralExpr.self,
      ErrorExpr.self,
      FloatLiteralExpr.self,
      IntegerLiteralExpr.self,
      NilLiteralExpr.self,
      StringLiteralExpr.self:
      break

    default:
      unexpected("expression", found: id, of: ast)
    }
  }

  private mutating func collectCaptures(
    ofCond id: NodeID<CondExpr>,
    into captures: inout FreeSet,
    inMutatingContext isContextMutating: Bool
  ) {
    boundNames.append([])

    // Visit the condition.
    for item in ast[id].condition {
      switch item {
      case .expr(let expr):
        collectCaptures(ofExpr: expr, into: &captures, inMutatingContext: false)
      case .decl(let decl):
        collectCaptures(ofBinding: decl, into: &captures)
      }
    }

    // Visit the then branch.
    switch ast[id].success {
    case .block(let stmt):
      collectCaptures(ofBrace: stmt, into: &captures)
    case .expr(let expr):
      collectCaptures(ofExpr: expr, into: &captures, inMutatingContext: isContextMutating)
    }

    // Bindings declared in the condition are not in scope for the else branch.
    boundNames.removeLast()

    // Visit the else branch, if any.
    switch ast[id].failure {
    case .block(let stmt):
      collectCaptures(ofBrace: stmt, into: &captures)
    case .expr(let expr):
      collectCaptures(ofExpr: expr, into: &captures, inMutatingContext: isContextMutating)
    case nil:
      break
    }
  }

  private mutating func collectCaptures(
    ofCast id: NodeID<CastExpr>,
    into captures: inout FreeSet,
    inMutatingContext isContextMutating: Bool
  ) {
    collectCaptures(ofExpr: ast[id].left, into: &captures, inMutatingContext: isContextMutating)
    collectCaptures(ofTypeExpr: ast[id].right, into: &captures)
  }

  private mutating func collectCaptures(
    ofFunctionCall id: NodeID<FunctionCallExpr>,
    into captures: inout FreeSet,
    inMutatingContext isContextMutating: Bool
  ) {
    if ast[id].callee.kind == NameExpr.self {
      // If the callee is a bare name expression, use the label arguments.
      let callee = NodeID<NameExpr>(ast[id].callee)!
      if ast[callee].domain == .none {
        let baseName: Name
        if (ast[callee].name.value.notation == nil) && ast[callee].name.value.labels.isEmpty {
          baseName = Name(
            stem: ast[callee].name.value.stem,
            labels: ast[id].arguments.map({ $0.label?.value }))
        } else {
          baseName = ast[callee].name.value
        }
        record(occurrence: callee, withCapability: .let, ifFree: baseName, into: &captures)
      }

      // Visit other parts of the callee's expression in which free names may occur.
      collectCaptures(
        ofName: callee, into: &captures, inMutatingContext: false, ignoringBaseName: true)
    } else {
      // Visit as any expression.
      collectCaptures(ofExpr: ast[id].callee, into: &captures, inMutatingContext: false)
    }

    // Visit the arguments.
    for argument in ast[id].arguments {
      collectCaptures(ofExpr: argument.value, into: &captures, inMutatingContext: false)
    }
  }

  private mutating func collectCaptures(
    ofInout id: NodeID<InoutExpr>,
    into captures: inout FreeSet,
    inMutatingContext isContextMutating: Bool
  ) {
    collectCaptures(ofExpr: ast[id].subject, into: &captures, inMutatingContext: true)
  }

  /// Collects the names occurring free in the specified expression.
  ///
  /// - Parameter ignoreBaseName: Assign this parameter to `true` to prevent the method from
  ///   inserting the base name of the expression even when it isn't bound.
  private mutating func collectCaptures(
    ofName id: NodeID<NameExpr>,
    into captures: inout FreeSet,
    inMutatingContext isContextMutating: Bool,
    ignoringBaseName ignoreBaseName: Bool = false
  ) {
    switch ast[id].domain {
    case .none where !ignoreBaseName:
      record(
        occurrence: id,
        withCapability: isContextMutating ? .inout : .let,
        ifFree: ast[id].name.value,
        into: &captures)

    case .expr(let domain):
      collectCaptures(ofExpr: domain, into: &captures, inMutatingContext: isContextMutating)

    default:
      break
    }

    for argument in ast[id].arguments {
      collectCaptures(ofExpr: argument.value, into: &captures, inMutatingContext: false)
    }
  }

  private mutating func collectCaptures(
    ofSequence id: NodeID<SequenceExpr>,
    into captures: inout FreeSet,
    inMutatingContext isContextMutating: Bool
  ) {
    collectCaptures(ofExpr: ast[id].head, into: &captures, inMutatingContext: false)
    for element in ast[id].tail {
      collectCaptures(ofExpr: element.operator, into: &captures, inMutatingContext: false)
      collectCaptures(ofExpr: element.operand, into: &captures, inMutatingContext: false)
    }
  }

  private mutating func collectCaptures(
    ofTuple id: NodeID<TupleExpr>,
    into captures: inout FreeSet,
    inMutatingContext isContextMutating: Bool
  ) {
    for element in ast[id].elements {
      collectCaptures(ofExpr: element.value, into: &captures, inMutatingContext: false)
    }
  }

  private mutating func collectCaptures(
    ofTupleMember id: NodeID<TupleMemberExpr>,
    into captures: inout FreeSet,
    inMutatingContext isContextMutating: Bool
  ) {
    collectCaptures(ofExpr: ast[id].tuple, into: &captures, inMutatingContext: false)
  }

  private mutating func collectCaptures<T: PatternID>(
    ofPattern id: T,
    into captures: inout FreeSet
  ) {
    switch id.kind {
    case BindingPattern.self:
      collectCaptures(ofBindingPattern: NodeID(id)!, into: &captures)
    case NamePattern.self:
      collectCaptures(ofNamePattern: NodeID(id)!, into: &captures)
    case TuplePattern.self:
      collectCaptures(ofTuplePattern: NodeID(id)!, into: &captures)
    case ExprPattern.self:
      let expr = ast[NodeID<ExprPattern>(id)!].expr
      collectCaptures(ofExpr: expr, into: &captures, inMutatingContext: false)
    case WildcardPattern.self:
      break
    default:
      unexpected("pattern", found: id, of: ast)
    }
  }

  private mutating func collectCaptures(
    ofBindingPattern id: NodeID<BindingPattern>,
    into captures: inout FreeSet
  ) {
    collectCaptures(ofPattern: ast[id].subpattern, into: &captures)
    if let annotation = ast[id].annotation {
      collectCaptures(ofTypeExpr: annotation, into: &captures)
    }
  }

  private mutating func collectCaptures(
    ofNamePattern id: NodeID<NamePattern>,
    into captures: inout FreeSet
  ) {
    let name = Name(stem: ast[ast[id].decl].baseName)
    boundNames[boundNames.count - 1].insert(name)
  }

  private mutating func collectCaptures(
    ofTuplePattern id: NodeID<TuplePattern>,
    into captures: inout FreeSet
  ) {
    for element in ast[id].elements {
      collectCaptures(ofPattern: element.pattern, into: &captures)
    }
  }

  /// Collects the names occurring free in the specified statement.
  private mutating func collectCaptures<T: StmtID>(
    ofStmt id: T,
    into captures: inout FreeSet
  ) {
    switch id.kind {
    case AssignStmt.self:
      collectCaptures(ofAssign: NodeID(id)!, into: &captures)
    case BraceStmt.self:
      collectCaptures(ofBrace: NodeID(id)!, into: &captures)
    case DoWhileStmt.self:
      collectCaptures(ofDoWhile: NodeID(id)!, into: &captures)
    case ReturnStmt.self:
      collectCaptures(ofReturn: NodeID(id)!, into: &captures)
    case WhileStmt.self:
      collectCaptures(ofWhile: NodeID(id)!, into: &captures)
    case YieldStmt.self:
      collectCaptures(ofYield: NodeID(id)!, into: &captures)
    case DeclStmt.self:
      let decl = ast[NodeID<DeclStmt>(id)!].decl
      collectCaptures(ofDecl: decl, into: &captures)
    case ExprStmt.self:
      let expr = ast[NodeID<ExprStmt>(id)!].expr
      collectCaptures(ofExpr: expr, into: &captures, inMutatingContext: false)
    case BreakStmt.self, ContinueStmt.self:
      break
    default:
      unexpected("statement", found: id, of: ast)
    }
  }

  private mutating func collectCaptures(
    ofAssign id: NodeID<AssignStmt>,
    into captures: inout FreeSet
  ) {
    collectCaptures(ofExpr: ast[id].left, into: &captures, inMutatingContext: true)
    collectCaptures(ofExpr: ast[id].right, into: &captures, inMutatingContext: false)
  }

  private mutating func collectCaptures(
    ofBrace id: NodeID<BraceStmt>,
    into captures: inout FreeSet
  ) {
    boundNames.append([])
    for stmt in ast[id].stmts {
      collectCaptures(ofStmt: stmt, into: &captures)
    }
    boundNames.removeLast()
  }

  private mutating func collectCaptures(
    ofDoWhile id: NodeID<DoWhileStmt>,
    into captures: inout FreeSet
  ) {
    boundNames.append([])
    for stmt in ast[ast[id].body].stmts {
      collectCaptures(ofStmt: stmt, into: &captures)
    }
    collectCaptures(ofExpr: ast[id].condition, into: &captures, inMutatingContext: false)
    boundNames.removeLast()
  }

  private mutating func collectCaptures(
    ofReturn id: NodeID<ReturnStmt>,
    into captures: inout FreeSet
  ) {
    if let value = ast[id].value {
      collectCaptures(ofExpr: value, into: &captures, inMutatingContext: false)
    }
  }

  private mutating func collectCaptures(
    ofWhile id: NodeID<WhileStmt>,
    into captures: inout FreeSet
  ) {
    boundNames.append([])

    // Visit the condition.
    for item in ast[id].condition {
      switch item {
      case .expr(let expr):
        collectCaptures(ofExpr: expr, into: &captures, inMutatingContext: false)
      case .decl(let decl):
        collectCaptures(ofBinding: decl, into: &captures)
      }
    }

    // Visit the body.
    collectCaptures(ofBrace: ast[id].body, into: &captures)

    boundNames.removeLast()
  }

  private mutating func collectCaptures(
    ofYield id: NodeID<YieldStmt>,
    into captures: inout FreeSet
  ) {
    collectCaptures(ofExpr: ast[id].value, into: &captures, inMutatingContext: isYieldMutating)
  }

  private mutating func collectCaptures<T: TypeExprID>(
    ofTypeExpr id: T,
    into captures: inout FreeSet
  ) {
    switch id.kind {
    case NameExpr.self:
      collectCaptures(ofNameType: NodeID(id)!, into: &captures)
    case ParameterTypeExpr.self:
      collectCaptures(ofParameter: NodeID(id)!, into: &captures)
    case TupleTypeExpr.self:
      collectCaptures(ofTupleType: NodeID(id)!, into: &captures)
    default:
      unexpected("type expression", found: id, of: ast)
    }
  }

  private mutating func collectCaptures(
    ofNameType id: NodeID<NameExpr>,
    into captures: inout FreeSet
  ) {
    switch ast[id].domain {
    case .none, .implicit:
      break
    case .expr(let domain):
      collectCaptures(ofExpr: domain, into: &captures, inMutatingContext: false)
    }

    for argument in ast[id].arguments {
      collectCaptures(ofExpr: argument.value, into: &captures, inMutatingContext: false)
    }
  }

  private mutating func collectCaptures(
    ofParameter id: NodeID<ParameterTypeExpr>,
    into captures: inout FreeSet
  ) {
    collectCaptures(ofTypeExpr: ast[id].bareType, into: &captures)
  }

  private mutating func collectCaptures(
    ofTupleType id: NodeID<TupleTypeExpr>,
    into captures: inout FreeSet
  ) {
    for element in ast[id].elements {
      collectCaptures(ofTypeExpr: element.type, into: &captures)
    }
  }

}
