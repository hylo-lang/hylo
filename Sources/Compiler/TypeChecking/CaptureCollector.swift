import Utils

/// An AST visitor that collects the names that occur free in a function.
struct CaptureCollector {

  /// A dictionary mapping free names to their occurrences and the kind of use.
  typealias FreeSet = [
    Name: (occurences: [NodeID<NameExpr>], capability: ProjectionType.Capability)
  ]

  /// The AST containing the nodes being visited.
  let ast: AST

  /// A flag that indicates whether yield statements are supposed to project mutable objects.
  var isYieldMutating = false

  /// The set of names that are bound in the lexical scopes containing the node being visited.
  private var boundNames: [Set<Name>] = []

  init(ast: AST) {
    self.ast = ast
  }

  /// Returns the names occurring free in the specified function declaration, together with the
  /// list of their occurrences and a flag indicating whether the they are used mutably.
  ///
  /// - Requires: `id` must denote a local function.
  mutating func freeNames(in id: NodeID<FunDecl>) -> FreeSet {
    var captures: FreeSet = [:]
    collectCaptures(ofFun: id, includingExplicitCaptures: true, into: &captures)
    return captures
  }

  /// Records `occurence` as a use of `name` with the specified capability, unless `name` belongs
  /// the the set of bound identifiers.
  ///
  /// - Parameters:
  ///   - capability: The capability of the use; must be either `let` or `inout`.
  private mutating func record(
    occurrence: NodeID<NameExpr>,
    withCapability capability: ProjectionType.Capability,
    ifFree name: Name,
    into captures: inout FreeSet
  ) {
    precondition(capability == .let || capability == .inout)
    if !boundNames.contains(where: { $0.contains(name) }) {
      modifying(&captures[name, default: ([], .let)], { entry in
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
    case .bindingDecl:
      collectCaptures(ofBinding: NodeID(unsafeRawValue: id.rawValue), into: &captures)
    case .funDecl:
      collectCaptures(
        ofFun: NodeID(unsafeRawValue: id.rawValue),
        includingExplicitCaptures: false,
        into: &captures)
    default:
      unreachable("unexpected declaration")
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
    for pattern in ast[id].pattern.names(ast: ast) {
      boundNames[boundNames.count - 1].insert(Name(stem: ast[ast[pattern].decl].name))
    }
  }

  private mutating func collectCaptures(
    ofFun id: NodeID<FunDecl>,
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
      newNames.insert(Name(stem: ast[parameter].name))
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
        switch parameter {
        case .type(let parameter):
          if let value = ast[parameter].defaultValue {
            collectCaptures(ofTypeExpr: value, into: &captures)
          }
          newNames.insert(Name(stem: ast[parameter].name))

        case .size(let parameter):
          if let value = ast[parameter].defaultValue {
            collectCaptures(ofExpr: value, into: &captures, inMutatingContext: false)
          }
          newNames.insert(Name(stem: ast[parameter].name))
        }
      }
    }

    // Visit the values of the captures and collect their names.
    for capture in ast[id].explicitCaptures {
      if let initializer = ast[capture].initializer {
        collectCaptures(ofExpr: initializer, into: &captures, inMutatingContext: false)
      }
      if !areExplicitCapturesIncluded {
        for pattern in ast[capture].pattern.names(ast: ast) {
          newNames.insert(Name(stem: ast[ast[pattern].decl].name))
        }
      }
    }

    // Visit the function's body.
    boundNames.append(newNames)
    switch ast[id].body?.value {
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
    case .assignExpr:
      collectCaptures(
        ofAssign: NodeID(unsafeRawValue: id.rawValue),
        into: &captures,
        inMutatingContext: isContextMutating)

    case .castExpr:
      collectCaptures(
        ofCast: NodeID(unsafeRawValue: id.rawValue),
        into: &captures,
        inMutatingContext: isContextMutating)

    case .funCallExpr:
      collectCaptures(
        ofFunCall: NodeID(unsafeRawValue: id.rawValue),
        into: &captures,
        inMutatingContext: isContextMutating)

    case .nameExpr:
      collectCaptures(
        ofName: NodeID(unsafeRawValue: id.rawValue),
        into: &captures,
        inMutatingContext: isContextMutating)

    case .sequenceExpr:
      collectCaptures(
        ofSequence: NodeID(unsafeRawValue: id.rawValue),
        into: &captures,
        inMutatingContext: isContextMutating)

    case .tupleExpr:
      collectCaptures(
        ofTuple: NodeID(unsafeRawValue: id.rawValue),
        into: &captures,
        inMutatingContext: isContextMutating)

    case .boolLiteralExpr,
         .charLiteralExpr,
         .floatLiteralExpr,
         .integerLiteralExpr,
         .nilExpr,
         .stringLiteralExpr:
      break

    default:
      unreachable("unexpected expression")
    }
  }

  private mutating func collectCaptures(
    ofAssign id: NodeID<AssignExpr>,
    into captures: inout FreeSet,
    inMutatingContext isContextMutating: Bool
  ) {
    collectCaptures(ofExpr: ast[id].left, into: &captures, inMutatingContext: true)
    collectCaptures(ofExpr: ast[id].right, into: &captures, inMutatingContext: false)
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
    ofFunCall id: NodeID<FunCallExpr>,
    into captures: inout FreeSet,
    inMutatingContext isContextMutating: Bool
  ) {
    if ast[id].callee.kind == .nameExpr {
      // If the callee is a bare name expression, use the label arguments.
      let callee = NodeID<NameExpr>(unsafeRawValue: ast[id].callee.rawValue)
      if ast[callee].domain == .none {
        let baseName: Name
        if (ast[callee].notation == nil) && ast[callee].labels.isEmpty {
          baseName = Name(
            stem: ast[callee].stem.value,
            labels: ast[id].arguments.map({ $0.value.label?.value }))
        } else {
          baseName = ast[callee].baseName
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
      collectCaptures(ofExpr: argument.value.value, into: &captures, inMutatingContext: false)
    }
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
        ifFree: ast[id].baseName,
        into: &captures)

    case .explicit(let domain):
      collectCaptures(ofExpr: domain, into: &captures, inMutatingContext: isContextMutating)

    default:
      break
    }

    for argument in ast[id].arguments {
      switch argument {
      case .type(let expr):
        collectCaptures(ofTypeExpr: expr, into: &captures)
      case .size(let expr):
        collectCaptures(ofExpr: expr, into: &captures, inMutatingContext: false)
      }
    }
  }

  private mutating func collectCaptures(
    ofTuple id: NodeID<TupleExpr>,
    into captures: inout FreeSet,
    inMutatingContext isContextMutating: Bool
  ) {
    for element in ast[id].elements {
      collectCaptures(ofExpr: element.value.value, into: &captures, inMutatingContext: false)
    }
  }

  private mutating func collectCaptures(
    ofSequence id: NodeID<SequenceExpr>,
    into captures: inout FreeSet,
    inMutatingContext isContextMutating: Bool
  ) {
    switch ast[id] {
    case .unfolded(let exprs):
      // Note: operators are not captured as they denote methods of their left operands.
      for i in 0 ..< exprs.count where i % 2 == 0 {
        collectCaptures(ofExpr: exprs[i], into: &captures, inMutatingContext: false)
      }

    case .root(let expr):
      collectCaptures(ofExpr: expr, into: &captures, inMutatingContext: false)
    }
  }

  private mutating func collectCaptures<T: PatternID>(
    ofPattern id: T,
    into captures: inout FreeSet
  ) {
    switch id.kind {
    case .bindingPattern:
      collectCaptures(ofBindingPattern: NodeID(unsafeRawValue: id.rawValue), into: &captures)
    case .namePattern:
      collectCaptures(ofNamePattern: NodeID(unsafeRawValue: id.rawValue), into: &captures)
    case .tuplePattern:
      collectCaptures(ofTuplePattern: NodeID(unsafeRawValue: id.rawValue), into: &captures)
    case .exprPattern:
      let expr = ast[NodeID<ExprPattern>(unsafeRawValue: id.rawValue)].expr
      collectCaptures(ofExpr: expr, into: &captures, inMutatingContext: false)
    case .wildcardPattern:
      break
    default:
      unreachable("unexpected pattern")
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
    let name = Name(stem: ast[ast[id].decl].name)
    boundNames[boundNames.count - 1].insert(name)
  }

  private mutating func collectCaptures(
    ofTuplePattern id: NodeID<TuplePattern>,
    into captures: inout FreeSet
  ) {
    for element in ast[id].elements {
      collectCaptures(ofPattern: element.value.pattern, into: &captures)
    }
  }

  /// Collects the names occurring free in the specified statement.
  private mutating func collectCaptures<T: StmtID>(
    ofStmt id: T,
    into captures: inout FreeSet
  ) {
    switch id.kind {
    case .braceStmt:
      collectCaptures(ofBrace: NodeID(unsafeRawValue: id.rawValue), into: &captures)
    case .doWhileStmt:
      collectCaptures(ofDoWhile: NodeID(unsafeRawValue: id.rawValue), into: &captures)
    case .returnStmt:
      collectCaptures(ofReturn: NodeID(unsafeRawValue: id.rawValue), into: &captures)
    case .whileStmt:
      collectCaptures(ofWhile: NodeID(unsafeRawValue: id.rawValue), into: &captures)
    case .yieldStmt:
      collectCaptures(ofYield: NodeID(unsafeRawValue: id.rawValue), into: &captures)
    case .declStmt:
      let decl = ast[NodeID<DeclStmt>(unsafeRawValue: id.rawValue)].decl
      collectCaptures(ofDecl: decl, into: &captures)
    case .exprStmt:
      let expr = ast[NodeID<ExprStmt>(unsafeRawValue: id.rawValue)].expr
      collectCaptures(ofExpr: expr, into: &captures, inMutatingContext: false)
    case .breakStmt, .continueStmt:
      break
    default:
      unreachable("unexpected statement")
    }
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
      switch item.value {
      case .expr(let expr):
        collectCaptures(ofExpr: expr, into: &captures, inMutatingContext: false)
      case .decl(let decl):
        collectCaptures(ofBinding: decl, into: &captures)
      }
    }

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
    case .nameTypeExpr:
      collectCaptures(ofNameType: NodeID(unsafeRawValue: id.rawValue), into: &captures)
    case .parameterTypeExpr:
      collectCaptures(ofParameter: NodeID(unsafeRawValue: id.rawValue), into: &captures)
    case .tupleTypeExpr:
      collectCaptures(ofTupleType: NodeID(unsafeRawValue: id.rawValue), into: &captures)
    default:
      unreachable("unexpected type expression")
    }
  }

  private mutating func collectCaptures(
    ofNameType id: NodeID<NameTypeExpr>,
    into captures: inout FreeSet
  ) {
    if let domain = ast[id].domain {
      collectCaptures(ofTypeExpr: domain, into: &captures)
    }

    for argument in ast[id].arguments {
      switch argument {
      case .type(let expr):
        collectCaptures(ofTypeExpr: expr, into: &captures)
      case .size(let expr):
        collectCaptures(ofExpr: expr, into: &captures, inMutatingContext: false)
      }
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
      collectCaptures(ofTypeExpr: element.value.type, into: &captures)
    }
  }

}
