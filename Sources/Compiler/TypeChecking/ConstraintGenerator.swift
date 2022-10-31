import Utils

/// A visitor that generates constraints based on the structure of the AST.
struct ConstraintGenerator: ExprVisitor {

  typealias Result = Void

  /// Creates an instance for generating constraints in `scope`.
  init(checker: TypeChecker, scope: AnyScopeID) {
    self.checker = checker
    self.scope = scope
  }

  /// A borrowed projection of the type checker that uses this constraint generator.
  var checker: TypeChecker

  /// The scope in which the AST is visited.
  private var scope: AnyScopeID

  /// A map from expression to its expected type.
  var expectedTypes = ExprMap<Type>()

  /// A map from visited expression to its inferred type.
  var inferredTypes = ExprMap<Type>()

  /// The set of type constraints being generated.
  private(set) var constraints: [LocatableConstraint] = []

  /// The diagnostics of the errors the generator encountered.
  var diagnostics: [Diagnostic] = []

  mutating func visit(assign id: NodeID<AssignExpr>) {
    // Infer the type on the left.
    let lhs = checker.ast[id].left
    lhs.accept(&self)

    // Constrain the right to be subtype of the left.
    let rhs = checker.ast[id].right
    inferredTypes[rhs] = .variable(TypeVariable(node: rhs.base))
    constraints.append(LocatableConstraint(
      .equalityOrSubtyping(l: inferredTypes[rhs]!, r: inferredTypes[lhs]!),
      node: AnyNodeID(id),
      cause: .assignment))

    // Infer the type on the right.
    expectedTypes[rhs] = inferredTypes[lhs]
    rhs.accept(&self)

    // Assignments have the unit type.
    assume(typeOf: id, equals: .unit)
  }

  mutating func visit(async id: NodeID<AsyncExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(await id: NodeID<AwaitExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(booleanLiteral id: NodeID<BooleanLiteralExpr>) {
    let boolType = ProductType(standardLibraryTypeNamed: "Bool", ast: checker.ast)!
    assume(typeOf: id, equals: .product(boolType))
  }

  mutating func visit(bufferLiteral id : NodeID<BufferLiteralExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(cast id: NodeID<CastExpr>) {
    // Realize the type to which the left operand should be converted.
    guard var target = checker.realize(checker.ast[id].right, inScope: scope) else {
      assignToError(id)
      return
    }

    let (ty, cs) = checker.contextualize(type: target, inScope: scope)
    target = ty
    constraints.append(contentsOf: cs.map({ LocatableConstraint($0, node: AnyNodeID(id)) }))

    let lhs = checker.ast[id].left
    switch checker.ast[id].kind {
    case .down:
      // Note: constraining the type of the left operand to be above the right operand wouldn't
      // contribute any useful information to the constraint system.
      break

    case .up:
      // The type of the left operand must be statically known to subtype of the right operand.
      inferredTypes[lhs] = .variable(TypeVariable(node: lhs.base))
      constraints.append(LocatableConstraint(
        .equalityOrSubtyping(l: inferredTypes[lhs]!, r: target),
        node: AnyNodeID(id),
        cause: .assignment))

    case .builtinPointerConversion:
      // The type of the left operand must be `Builtin.Pointer`.
      inferredTypes[lhs] = .builtin(.pointer)
    }

    // Visit the left operand.
    lhs.accept(&self)

    // In any case, the expression is assumed to have the type denoted by the right operand.
    assume(typeOf: id, equals: target)
  }

  mutating func visit(cond id: NodeID<CondExpr>) {
    defer { assert(inferredTypes[id] != nil) }

    // Visit the condition(s).
    let boolType = ProductType(standardLibraryTypeNamed: "Bool", ast: checker.ast)!
    for item in checker.ast[id].condition {
      switch item {
      case .expr(let expr):
        // Condition must be Boolean.
        inferredTypes[expr] = .product(boolType)
        expr.accept(&self)

      case .decl(let binding):
        _ = checker.check(binding: binding)
      }
    }

    // Assume the node represents an expression if both branches are single expressions.
    let inferredType: Type?

    // Visit the success branch.
    switch checker.ast[id].success {
    case .expr(let thenExpr):
      expectedTypes[thenExpr] = expectedTypes[id]
      thenExpr.accept(&self)
      inferredType = inferredTypes[thenExpr]

    case .block(let thenBlock):
      _ = checker.check(brace: thenBlock)
      inferredType = nil
    }

    // Visit the failure branch.
    switch checker.ast[id].failure {
    case .expr(let elseExpr):
      assume(typeOf: id, equals: inferredType!)
      expectedTypes[elseExpr] = inferredType
      elseExpr.accept(&self)

    case .block(let thenBlock):
      assume(typeOf: id, equals: .unit)
      _ = checker.check(brace: thenBlock)

    case nil:
      assume(typeOf: id, equals: .unit)
    }
  }

  mutating func visit(error id: NodeID<ErrorExpr>) {
    // Nothing to do here.
  }

  mutating func visit(floatLiteral id: NodeID<FloatLiteralExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(funCall id: NodeID<FunCallExpr>) {
    defer { assert(inferredTypes[id] != nil) }

    let callee = checker.ast[id].callee

    func propagateDown(calleeType: LambdaType, calleeConstraints: [Constraint] = []) {
      // Collect the call labels.
      let labels = checker.ast[id].arguments.map({ $0.label?.value })

      // Check that the labels inferred from the callee are consistent with that of the call.
      let calleeLabels = calleeType.inputs.map({ $0.label })
      if calleeLabels != labels {
        diagnostics.append(.incompatibleLabels(
          found: labels, expected: calleeLabels, at: checker.ast.ranges[callee]))
        assignToError(id)
        return
      }

      // Gather the callee's constraints.
      constraints.append(contentsOf: calleeConstraints.map({ c in
        LocatableConstraint(c, node: callee.base)
      }))

      // Propagate type information to the arguments.
      for i in 0 ..< checker.ast[id].arguments.count {
        let argument = checker.ast[id].arguments[i].value
        let argumentType = Type.variable(TypeVariable(node: argument.base))
        let parameterType = calleeType.inputs[i].type

        inferredTypes[argument] = argumentType
        constraints.append(LocatableConstraint(
          .parameter(l: argumentType, r: parameterType),
          node: argument.base,
          cause: .callArgument))

        if case .parameter(let type) = parameterType {
          expectedTypes[argument] = type.bareType
        }
        argument.accept(&self)
      }

      // Constrain the type of the call.
      assume(typeOf: id, equals: calleeType.output)
    }

    // Infer the type of the callee.
    callee.accept(&self)

    // There are four cases to consider:
    // 1. We failed to infer the type of the callee. In that case there's nothing more we can do.
    // 2. We determined that the callee refers to a nominal type declaration. In that case, we
    //    desugar a constructor call.
    // 3. We determined the exact type of the callee, and its a callable. In that case, we may
    //    propagate that information top-down to refine the inference of the arguments' types.
    // 4. We couldn't infer the exact type of the callee and must rely on bottom-up inference to
    //    further refine type inference.

    // 1st case
    if case .error = inferredTypes[callee] {
      assignToError(id)
      return
    }

    // 2nd case
    if let c = NodeID<NameExpr>(converting: callee),
       let d = checker.referredDecls[c]?.decl,
       d.kind <= .typeDecl
    {
      switch d.kind {
      case .productTypeDecl:
        let initializers = resolve(
          stem: "init",
          labels: [],
          notation: nil,
          introducer: nil,
          inDeclSpaceOf: AnyScopeID(converting: d)!)

        // Select suitable candidates based on argument labels.
        let labels = checker.ast[id].arguments.map({ $0.label?.value })
        var candidates: [Constraint.OverloadCandidate] = []
        for initializer in initializers {
          // Remove the receiver from the parameter list.
          let ctor = LambdaType(converting: initializer.type)!.ctor()!

          if labels.elementsEqual(ctor.labels) {
            let (ty, cs) = checker.open(type: .lambda(ctor))
            candidates.append(Constraint.OverloadCandidate(
              reference: .direct(initializer.decl),
              type: ty,
              constraints: cs,
              penalties: 0))
          }
        }

        switch candidates.count {
        case 0:
          let name = Name(stem: "init", labels: labels)
          diagnostics.append(.undefined(name: "\(name)", at: checker.ast[c].name.range))
          assignToError(id)
          return

        case 1:
          // Reassign the referred declaration and type of the name expression.
          checker.referredDecls[c] = candidates[0].reference
          inferredTypes[c] = candidates[0].type

          // Propagate the type of the constructor down.
          let calleeType = LambdaType(converting: candidates[0].type)!
          propagateDown(calleeType: calleeType, calleeConstraints: candidates[0].constraints)

        default:
          // TODO: Handle specializations
          fatalError("not implemented")
        }

      case .traitDecl:
        let trait = TraitType(decl: NodeID(converting: d)!, ast: checker.ast)
        diagnostics.append(.cannotConstruct(trait: trait, at: checker.ast.ranges[callee]))
        assignToError(id)

      case .typeAliasDecl:
        fatalError("not implemented")

      default:
        unreachable("unexpected declaration")
      }

      return
    }

    // 3rd case
    if case .lambda(let calleeType) = inferredTypes[callee] {
      propagateDown(calleeType: calleeType)
      return
    }

    // 4th case
    let output = expectedTypes[id] ?? .variable(TypeVariable(node: AnyNodeID(id)))
    assume(typeOf: id, equals: output)

    var inputs: [CallableTypeParameter] = []
    for i in 0 ..< checker.ast[id].arguments.count {
      // Infer the type of the argument bottom-up.
      checker.ast[id].arguments[i].value.accept(&self)

      let argument = checker.ast[id].arguments[i].value
      let parameterType = Type.variable(TypeVariable())
      constraints.append(LocatableConstraint(
        .parameter(l: inferredTypes[argument]!, r: parameterType),
        node: argument.base,
        cause: .callArgument))

      let argumentLabel = checker.ast[id].arguments[i].label?.value
      inputs.append(CallableTypeParameter(label: argumentLabel, type: parameterType))
    }

    // Constrain the type of the callee.
    let calleeType = Type.lambda(LambdaType(
      environment: .variable(TypeVariable()), inputs: inputs, output: output))
    constraints.append(LocatableConstraint(
      .equality(l: inferredTypes[callee]!, r: calleeType), node: callee.base))
  }

  mutating func visit(`inout` id: NodeID<InoutExpr>) {
    let subexpr = checker.ast[id].subexpr
    expectedTypes[subexpr] = expectedTypes[id]
    subexpr.accept(&self)
    assume(typeOf: id, equals: inferredTypes[subexpr]!)
  }

  mutating func visit(integerLiteral id: NodeID<IntegerLiteralExpr>) {
    defer { assert(inferredTypes[id] != nil) }

    let trait = TraitType(named: "ExpressibleByIntegerLiteral", ast: checker.ast)!

    switch expectedTypes[id] {
    case .some(.variable(let tau)):
      // The type of the expression is a variable, possibly constrained elsewhere; constrain it to
      // either be `Int` or conform to `ExpressibleByIntegerLiteral`.
      let intType = ProductType(standardLibraryTypeNamed: "Int", ast: checker.ast)!
      constraints.append(LocatableConstraint(
        .disjunction([
          Constraint.Minterm(
            constraints: [.equality(l: .variable(tau), r: .product(intType))],
            penalties: 0),
          Constraint.Minterm(
            constraints: [.conformance(l: .variable(tau), traits: [trait])],
            penalties: 1),
        ])))
      assume(typeOf: id, equals: .variable(tau))

    case .some(let expectedType):
      // The type of has been fixed; constrain it to conform to `ExpressibleByIntegerLiteral`.
      constraints.append(LocatableConstraint(
        .conformance(l: expectedType, traits: [trait]), node: AnyNodeID(id)))
      assume(typeOf: id, equals: expectedType)

    case nil:
      // Without contextual information, infer the type of the literal as `Val.Int`.
      let intType = ProductType(standardLibraryTypeNamed: "Int", ast: checker.ast)!
      assume(typeOf: id, equals: .product(intType))
    }
  }

  mutating func visit(lambda id: NodeID<LambdaExpr>) {
    defer { assert(inferredTypes[id] != nil) }

    // Realize the type of the underlying declaration.
    guard case .lambda(let declType) = checker.realize(funDecl: checker.ast[id].decl) else {
      assignToError(id)
      return
    }

    // Schedule the underlying declaration to be type-checked.
    checker.pendingLambdas.append(id)

    if case .lambda(let expectedType) = expectedTypes[id] {
      // Check that the declaration defines the expected number of parameters.
      if declType.inputs.count != expectedType.inputs.count {
        diagnostics.append(.invalidClosureParameterCount(
          expected: expectedType.inputs.count,
          found: declType.inputs.count,
          at: checker.ast.ranges[id]))
        assignToError(id)
        return
      }

      // Check that the declaration defines the expected argument labels.
      if !declType.labels.elementsEqual(expectedType.labels) {
        diagnostics.append(.incompatibleLabels(
          found: Array(declType.labels),
          expected: Array(expectedType.labels),
          at: checker.ast.ranges[id]))
        assignToError(id)
        return
      }
    } else if case .variable = declType.output {
      if case .expr(let body) = checker.ast[checker.ast[id].decl].body {
        // Infer the return type of the lambda from its body.
        inferredTypes[body] = declType.output
        expectedTypes[body] = declType.output

        let currentScope = scope
        scope = AnyScopeID(checker.ast[id].decl)
        body.accept(&self)
        scope = currentScope
      } else {
        // The system is underspecified.
        diagnostics.append(.cannotInferComplexReturnType(
          at: checker.ast[checker.ast[id].decl].introducer.range))
        assignToError(id)
        return
      }
    }

    assume(typeOf: id, equals: .lambda(declType))
  }

  mutating func visit(mapLiteral i: NodeID<MapLiteralExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(match i: NodeID<MatchExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(name id: NodeID<NameExpr>) {
    // Resolves the name.
    switch checker.ast[id].domain {
    case .none:
      let expr = checker.ast[id]
      if checker.isBuiltinModuleVisible && (expr.name.value.stem == "Builtin") {
        assume(typeOf: id, equals: .builtin(.module))
        checker.referredDecls[id] = .direct(AnyDeclID(checker.ast.builtinDecl))
        return
      }

      let candidates = resolve(
        stem: expr.name.value.stem,
        labels: expr.name.value.labels,
        notation: expr.name.value.notation,
        introducer: expr.name.value.introducer)

      if candidates.isEmpty {
        diagnostics.append(.undefined(name: "\(expr.name)", at: expr.name.range))
        assignToError(id)
        return
      }

      let inferredType: Type
      if candidates.count == 1 {
        // Contextualize the match.
        let context = checker.scopeHierarchy.container[candidates[0].decl]!
        let (ty, cs) = checker.contextualize(type: candidates[0].type, inScope: context)
        inferredType = ty

        // Register associated constraints.
        constraints.append(contentsOf: cs.map({ LocatableConstraint($0, node: AnyNodeID(id)) }))

        // Bind the name expression to the referred declaration.
        if checker.scopeHierarchy.isNonStaticMember(decl: candidates[0].decl, ast: checker.ast) {
          checker.referredDecls[id] = .member(candidates[0].decl)
        } else {
          checker.referredDecls[id] = .direct(candidates[0].decl)
        }
      } else {
        // TODO: Create an overload constraint
        fatalError("not implemented")
      }

      assume(typeOf: id, equals: inferredType)

    case .expr(let domain):
      // Infer the type of the domain.
      domain.accept(&self)
      let domainType = inferredTypes[domain]!

      // If we failed to infer the type of the domain, there's nothing more we can do.
      if case .error = domainType {
        assignToError(id)
        return
      }

      // Handle references to built-in symbols.
      if domainType == .builtin(.module) {
        let symbolName = checker.ast[id].name.value.stem

        if let type = BuiltinSymbols[symbolName] {
          assume(typeOf: id, equals: .lambda(type))
          checker.referredDecls[id] = .direct(AnyDeclID(checker.ast.builtinDecl))
        } else if let type = BuiltinType(symbolName) {
          assume(typeOf: id, equals: .builtin(type))
          checker.referredDecls[id] = .direct(AnyDeclID(checker.ast.builtinDecl))
        } else {
          diagnostics.append(.undefined(name: symbolName, at: checker.ast[id].name.range))
          assignToError(id)
        }

        return
      }

      // Map the expression to a fresh variable unless we have top-down type information.
      let inferredType = expectedTypes[id] ?? .variable(TypeVariable(node: AnyNodeID(id)))
      assume(typeOf: id, equals: inferredType)

      // If we determined that the domain refers to a nominal type declaration, create a static
      // member constraint. Otherwise, create a non-static member constraint.
      let constraint: Constraint
      if let base = NodeID<NameExpr>(converting: domain),
         let decl = checker.referredDecls[base]?.decl,
         decl.kind <= .typeDecl
      {
        constraint = .unboundMember(l: domainType, m: checker.ast[id].name.value, r: inferredType)
      } else {
        constraint = .boundMember(l: domainType, m: checker.ast[id].name.value, r: inferredType)
      }
      constraints.append(LocatableConstraint(constraint, node: AnyNodeID(id), cause: .member))

    case .type:
      fatalError("not implemented")

    case .implicit:
      fatalError("not implemented")
    }
  }

  mutating func visit(nil i: NodeID<NilExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(sequence id: NodeID<SequenceExpr>) {
    let root: AnyExprID

    // Fold the sequence if necessary.
    switch checker.ast[id] {
    case .unfolded(let head, let tail):
      let tree = foldSequenceExpr(head: head, tail: tail)
      root = tree.desugars(ast: &checker.ast)
      checker.ast[id] = .root(root)

    case .root(let r):
      root = r
    }

    expectedTypes[root] = expectedTypes[id]
    root.accept(&self)
    assume(typeOf: id, equals: inferredTypes[root]!)
  }

  mutating func visit(storedProjection i: NodeID<StoredProjectionExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(stringLiteral i: NodeID<StringLiteralExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(subscriptCall i: NodeID<SubscriptCallExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(tuple id: NodeID<TupleExpr>) {
    defer { assert(inferredTypes[id] != nil) }

    var expr = checker.ast[id]

    // If the expected type is a tuple compatible with the shape of the expression, propagate that
    // information down the expression tree. Otherwise, infer the type of the expression from the
    // leaves and use type constraints to detect potential mismatch.
    var elements: [TupleType.Element] = []
    if case .tuple(let type) = expectedTypes[id],
       type.elements.elementsEqual(expr.elements, by: { (a, b) in a.label == b.label?.value })
    {
      for i in 0 ..< expr.elements.count {
        modifying(&expr.elements[i], { element in
          expectedTypes[element.value] = type.elements[i].type
          element.value.accept(&self)
          elements.append(TupleType.Element(
            label: element.label?.value,
            type: inferredTypes[element.value]!))
        })
      }
    } else {
      for i in 0 ..< expr.elements.count {
        modifying(&expr.elements[i], { element in
          element.value.accept(&self)
          elements.append(TupleType.Element(
            label: element.label?.value,
            type: inferredTypes[element.value]!))
        })
      }
    }
    assume(typeOf: id, equals: .tuple(TupleType(elements)))
  }

  mutating func visit(tupleMember id: NodeID<TupleMemberExpr>) {
    fatalError("not implemented")
  }

  /// Returns the well-formed declarations to which the specified name may refer, along with their
  /// overarching uncontextualized types. Ill-formed declarations are ignored.
  private mutating func resolve(
    stem: Identifier,
    labels: [String?],
    notation: OperatorNotation?,
    introducer: ImplIntroducer?,
    inDeclSpaceOf lookupContext: AnyScopeID? = nil
  ) -> [(decl: AnyDeclID, type: Type)] {
    // Check preconditions.
    precondition(notation == nil || labels.isEmpty, "invalid name")

    // Search for the referred declaration.
    var matches: TypeChecker.DeclSet
    if let ctx = lookupContext {
      matches = checker.lookup(stem, introducedInDeclSpaceOf: ctx, inScope: scope)
    } else {
      matches = checker.lookup(unqualified: stem, inScope: scope)
      if !matches.isEmpty && matches.isSubset(of: checker.bindingsUnderChecking) {
        matches = checker.lookup(unqualified: stem, inScope: checker.scopeHierarchy.parent[scope]!)
      }
    }

    // Bail out if there are no matches.
    if matches.isEmpty { return [] }

    // TODO: Filter by labels and operator notation

    // If the looked up name has a method introducer, it must refer to a method implementation.
    if let introducer = introducer {
      matches = Set(matches.compactMap({ match in
        guard let method = NodeID<FunDecl>(converting: match),
              let body = checker.ast[method].body,
              case .bundle(let impls) = body
        else { return nil }

        // TODO: Synthesize missing method implementations
        if let impl = impls.first(where: { checker.ast[$0].introducer.value == introducer }) {
          return AnyDeclID(impl)
        } else {
          return nil
        }
      }))
    }

    // Returns the matches along with their contextual type and associated constraints.
    return matches.compactMap({ (match) -> (AnyDeclID, Type)? in
      // Realize the type of the declaration.
      var matchType = checker.realize(decl: match)
      if matchType.isError { return nil }

      // Erase parameter conventions.
      if case .parameter(let t) = matchType {
        matchType = t.bareType
      }

      return (match, matchType)
    })
  }

  private mutating func assume<T: ExprID>(typeOf id: T, equals inferredType: Type) {
    if let ty = inferredTypes[id] {
      if ty != inferredType {
        constraints.append(LocatableConstraint(
          .equality(l: inferredType, r: ty),
          node: AnyNodeID(id)))
      }
    } else {
      inferredTypes[id] = inferredType
    }
  }

  private mutating func assignToError<T: ExprID>(_ id: T) {
    inferredTypes[id] = .error(ErrorType())
  }

}

extension ConstraintGenerator {

  /// A folded sequence of binary operations.
  indirect enum Tree {

    typealias Operator = (name: SourceRepresentable<Identifier>, precedence: PrecedenceGroup?)

    case node(operator: Operator, left: Tree, right: Tree)

    case leaf(AnyExprID)

    // Desugars the tree.
    func desugars(ast: inout AST) -> AnyExprID {
      switch self {
      case .node(let operator_, let left, let right):
        let receiver = left.desugars(ast: &ast)
        let argument = right.desugars(ast: &ast)

        let id = ast.insert(FunCallExpr(
          callee: AnyExprID(ast.insert(NameExpr(
            domain: .expr(receiver),
            name: SourceRepresentable(
              value: Name(stem: operator_.name.value),
              range: operator_.name.range)))),
          arguments: [CallArgument(value: argument)]))

        if let argumentRange = ast.ranges[argument] {
          ast.ranges[id] = (
            ast.ranges[receiver]?.upperBounded(by: argumentRange.upperBound) ?? argumentRange)
        } else {
          ast.ranges[id] = ast.ranges[receiver]
        }

        return AnyExprID(id)

      case .leaf(let id):
        return id
      }
    }

    mutating func append(operator operator_: Operator, rhs: AnyExprID) {
      switch self {
      case .node(let lhsOperator, let lhsLeft, var lhsRight):
        if let l = lhsOperator.precedence {
          if let r = operator_.precedence {
            // Both operators are in groups.
            if (l < r) || (l == r && l.associativity == .left) {
              self = .node(operator: operator_, left: self, right: .leaf(rhs))
              return
            }

            if (l > r) || (l == r && l.associativity == .right) {
              lhsRight.append(operator: operator_, rhs: rhs)
              self = .node(operator: lhsOperator, left: lhsLeft, right: lhsRight)
              return
            }
          } else {
            // Right operator is not in a group. Assume lowest precedence and left associativity.
            self = .node(operator: operator_, left: self, right: .leaf(rhs))
            return
          }
        } else if operator_.precedence != nil {
          // Only right operator is in a group. Assume higher precedence.
          lhsRight.append(operator: operator_, rhs: rhs)
          self = .node(operator: lhsOperator, left: lhsLeft, right: lhsRight)
        } else {
          // Neither operator is in a group. Assume left associativity.
          self = .node(operator: operator_, left: self, right: .leaf(rhs))
        }

      case .leaf:
        self = .node(operator: operator_, left: self, right: .leaf(rhs))
      }
    }

  }

  /// Folds a sequence of binary expressions into a tree.
  mutating func foldSequenceExpr(head: AnyExprID, tail: SequenceExpr.UnfoldedTail) -> Tree {
    return foldSequenceExpr(tail[0...], into: .leaf(head))
  }

  /// Folds the remainder of a sequence of binary expressions into `accumulatedResult`.
  mutating func foldSequenceExpr(
    _ tail: SequenceExpr.UnfoldedTail.SubSequence,
    into initialResult: Tree
  ) -> Tree {
    var accumulator = initialResult

    for i in tail.indices {
      // Search for the operator declaration.
      let candidates = checker.lookup(
        operator: tail[i].operatorName.value, notation: .infix, inScope: scope)

      switch candidates.count {
      case 0:
        checker.diagnostics.insert(.undefinedOperator(
          tail[i].operatorName.value, at: tail[i].operatorName.range))
        accumulator = .leaf(AnyExprID(checker.ast.insert(ErrorExpr())))

      case 1:
        let precedence = checker.ast[candidates[0]].precedenceGroup?.value
        accumulator.append(
          operator: (name: tail[i].operatorName, precedence: precedence), rhs: tail[i].operand)

      default:
        fatalError("not implemented")
      }
    }

    return accumulator
  }

  mutating func visit(unicodeScalarLiteral id: NodeID<UnicodeScalarLiteralExpr>) {
    fatalError("not implemented")
  }

}
