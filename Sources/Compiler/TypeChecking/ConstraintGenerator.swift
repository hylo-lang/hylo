import Utils

/// A visitor that generates constraints based on the structure of the AST.
struct ConstraintGenerator: ExprVisitor {

  typealias Result = Void

  /// A borrowed projection of the type checker that uses this constraint generator.
  var checker: TypeChecker!

  /// The scope in which the AST is visited.
  var scope: AnyScopeID

  /// A table mapping visited expressions to their inferred types.
  var inferredTypes = ExprMap<Type>()

  /// A table mapping name expressions to referred declarations.
  var referredDecls: [NodeID<NameExpr>: AnyDeclID] = [:]

  /// The set of type constraints being generated.
  var constraints: [LocatableConstraint] = []

  /// The diagnostics of the errors the generator encountered.
  var diagnostics: [Diagnostic] = []

  mutating func visit(assign id: NodeID<AssignExpr>) {
    let lhs = checker.ast[id].left
    let rhs = checker.ast[id].right
    lhs.accept(&self)
    rhs.accept(&self)

    constraints.append(LocatableConstraint(
      .equalityOrSubtyping(l: inferredTypes[lhs]!, r: inferredTypes[rhs]!),
      node: AnyNodeID(id),
      cause: .assignment))

    assume(typeOf: id, equals: .unit)
  }

  mutating func visit(async id: NodeID<AsyncExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(await id: NodeID<AwaitExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(boolLiteral id: NodeID<BoolLiteralExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(bufferLiteral id : NodeID<BufferLiteralExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(charLiteral id: NodeID<CharLiteralExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(cast id: NodeID<CastExpr>) {
    // Realize the type to which the left operand should be converted.
    guard let target = checker.realize(checker.ast[id].right, inScope: scope) else {
      assignToError(id)
      return
    }

    switch checker.ast[id].direction {
    case .down:
      // Note: constraining the type of the left operand to be above the right operand wouldn't
      // contribute any useful information to the constraint system.
      break

    case .up:
      // The type of the left operand must be statically known to subtype of the right operand.
      inferredTypes[checker.ast[id].left] = target
    }

    // Visit the left operand.
    checker.ast[id].left.accept(&self)

    // In any case, the expression is assumed to have the type denoted by the right operand.
    assume(typeOf: id, equals: target)
  }

  mutating func visit(cond id: NodeID<CondExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(floatLiteral id: NodeID<FloatLiteralExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(funCall id: NodeID<FunCallExpr>) {
    defer { assert(inferredTypes[id] != nil) }

    let callee = checker.ast[id].callee

    func propagateDown(calleeType: LambdaType, calleeConstraints: [Constraint] = []) {
      // Collect the call labels.
      let labels = checker.ast[id].arguments.map({ $0.value.label?.value })

      // Check that the labels inferred from the callee are consistent with that of the call.
      let calleeLabels = calleeType.inputs.map({ $0.label })
      if calleeLabels != labels {
        diagnostics.append(.incompatibleLabels(
          found: calleeLabels, expected: labels, range: checker.ast.ranges[callee]))
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
        let argumentType = Type.variable(TypeVariable(node: argument.value.base))
        let parameterType = calleeType.inputs[i].type

        inferredTypes[argument.value] = argumentType
        constraints.append(LocatableConstraint(
          .parameter(l: argumentType, r: parameterType),
          node: argument.value.base,
          cause: .callArgument))

        argument.value.accept(&self)
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
       let d = referredDecls[c],
       d.kind <= .typeDecl
    {
      switch d.kind {
      case .productTypeDecl:
        let initializers = resolve(
          stem: "init",
          labels: [],
          notation: nil,
          inDeclSpaceOf: AnyScopeID(converting: d)!)

        // We should get at least one a memberwise initializer.
        assert(!initializers.isEmpty)

        // Select suitable candidates based on argument labels.
        let labels = checker.ast[id].arguments.map({ $0.value.label?.value })
        var candidates: [Constraint.OverloadCandidate] = []
        for initializer in initializers {
          // Remove the receiver from the parameter list.
          guard case .lambda(let f) = initializer.type else { unreachable() }
          let ctor = f.ctor()!

          if labels.elementsEqual(ctor.labels) {
            let (ty, cs) = checker.open(type: .lambda(ctor))
            candidates.append(Constraint.OverloadCandidate(
              decl: initializer.decl, type: ty, constraints: cs))
          }
        }

        switch candidates.count {
        case 0:
          let name = Name(stem: "init", labels: labels)
          diagnostics.append(.undefined(name: "\(name)", range: checker.ast[c].stem.range))
          assignToError(id)
          return

        case 1:
          // Reassign the referred declaration and type of the name expression.
          referredDecls[c] = candidates[0].decl
          inferredTypes[c] = candidates[0].type

          // Propagate the type of the constructor down.
          guard case .lambda(let calleeType) = candidates[0].type else { unreachable() }
          propagateDown(calleeType: calleeType, calleeConstraints: candidates[0].constraints)

        default:
          // TODO: Handle specializations
          fatalError("not implemented")
        }

      case .traitDecl:
        let trait = TraitType(decl: NodeID(converting: d)!, ast: checker.ast)
        diagnostics.append(.cannotConstruct(trait: trait, range: checker.ast.ranges[callee]))
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
    let output: Type
    if let expectedType = inferredTypes[id] {
      output = expectedType
    } else {
      output = .variable(TypeVariable(node: AnyNodeID(id)))
      inferredTypes[id] = output
    }

    var inputs: [CallableTypeParameter] = []
    for i in 0 ..< checker.ast[id].arguments.count {
      checker.ast[id].arguments[i].value.value.accept(&self)
      let argumentValue = checker.ast[id].arguments[i].value.value

      let argumentLabel = checker.ast[id].arguments[i].value.label?.value
      let argumentType = inferredTypes[argumentValue]!
      let parameterType = Type.variable(TypeVariable())

      inputs.append(CallableTypeParameter(label: argumentLabel, type: parameterType))
      constraints.append(LocatableConstraint(
        .parameter(l: argumentType, r: parameterType),
        node: argumentValue.base,
        cause: .callArgument))
    }

    // Constrain the type of the callee.
    let calleeType = Type.lambda(LambdaType(
      environment: .variable(TypeVariable()), inputs: inputs, output: output))
    constraints.append(LocatableConstraint(
      .equality(l: inferredTypes[callee]!, r: calleeType), node: callee.base))
  }

  mutating func visit(integerLiteral id: NodeID<IntegerLiteralExpr>) {
    let trait = TraitType(named: "ExpressibleByIntegerLiteral", ast: checker.ast)
      ?? unreachable()

    switch inferredTypes[id] {
    case .some(.variable(let tau)):
      // The type of the expression is a variable, possibly constrained elsewhere; constrain it to
      // either be `Int` or conform to `ExpressibleByIntegerLiteral`.
      constraints.append(LocatableConstraint(
        .disjunction([
          Constraint.Minterm(
            constraints: [.equality(l: .variable(tau), r: .int(in: checker.ast))],
            penalties: 0),
          Constraint.Minterm(
            constraints: [.conformance(l: .variable(tau), traits: [trait])],
            penalties: 1),
        ])))

    case .some(let expectedType):
      // The type of has been fixed; constrain it to conform to `ExpressibleByIntegerLiteral`.
      constraints.append(LocatableConstraint(
        .conformance(l: expectedType, traits: [trait]), node: AnyNodeID(id)))

    case nil:
      // Without contextual information, infer the type of the literal as `Val.Int`.
      inferredTypes[id] = .int(in: checker.ast)
    }
  }

  mutating func visit(lambda id: NodeID<LambdaExpr>) {
    // Realize the type of the underlying declaration.
    guard case .lambda(let declType) = checker.realize(funDecl: checker.ast[id].decl) else {
      assignToError(id)
      return
    }

    // TODO: Schedule the underlying declaration to be type-checked.

    // Exploit top-down information to refine the realized type or try to infer missing information
    // from the body of the lambda.
    if case .lambda(let expectedType) = inferredTypes[id] {
      // Check that the declaration defines the correct number of parameters.
      if declType.inputs.count != expectedType.inputs.count {
        diagnostics.append(.invalidClosureParameterCount(
          expected: expectedType.inputs.count,
          found: declType.inputs.count,
          range: checker.ast.ranges[id]))
        assignToError(id)
        return
      }

      // Check that the declaration defines the correct argument labels.
      if !declType.labels.elementsEqual(expectedType.labels) {
        diagnostics.append(.incompatibleLabels(
          found: Array(declType.labels),
          expected: Array(expectedType.labels),
          range: checker.ast.ranges[id]))
        assignToError(id)
        return
      }

      constraints.append(LocatableConstraint(
        .equalityOrSubtyping(l: .lambda(declType), r: .lambda(expectedType)),
        node: AnyNodeID(id)))
    } else if case .variable = declType.output {
      if case .expr(let body) = checker.ast[checker.ast[id].decl].body?.value {
        inferredTypes[body] = declType.output
        body.accept(&self)
      } else {
        diagnostics.append(.cannotInferComplexReturnType(
          range: checker.ast[checker.ast[id].decl].body?.range))
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

  mutating func visit(matchCase i: NodeID<MatchCaseExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(name id: NodeID<NameExpr>) {
    let stem = checker.ast[id].stem

    // Resolves the name.
    switch checker.ast[id].domain {
    case .none:
      let candidates = resolve(
        stem: checker.ast[id].stem.value,
        labels: checker.ast[id].labels,
        notation: checker.ast[id].notation)

      if candidates.isEmpty {
        diagnostics.append(.undefined(name: "\(checker.ast[id].baseName)", range: stem.range))
        assignToError(id)
        return
      }

      let inferredType: Type
      if candidates.count == 1 {
        // Contextualize the match.
        let context = checker.scopeHierarchy.container[candidates[0].decl]!
        let (ty, cs) = checker.contextualize(type: candidates[0].type, inScope: context)

        // Register associated constraints.
        inferredType = ty
        constraints.append(contentsOf: cs.map({ c in
          LocatableConstraint(c, node: AnyNodeID(id))
        }))

        // Bind the name expression to the referred declaration.
        referredDecls[id] = candidates[0].decl
      } else {
        // TODO: Create an overload constraint
        fatalError("not implemented")
      }

      assume(typeOf: id, equals: inferredType)

    case .explicit(let domain):
      // Infer the type of the domain.
      domain.accept(&self)

      // If we failed to infer the type of the domain, there's nothing more we can do.
      if case .error = inferredTypes[domain] {
        assignToError(id)
        return
      }

      // Map the expression to a fresh variable unless we have top-down type information.
      let inferredType: Type
      if let expectedType = inferredTypes[id] {
        inferredType = expectedType
      } else {
        inferredType = .variable(TypeVariable(node: AnyNodeID(id)))
        inferredTypes[id] = inferredType
      }

      // If we determined that the domain refers to a nominal type declaration, create a static
      // member constraint. Otherwise, create a non-static member constraint.
      if let base = NodeID<NameExpr>(converting: domain),
         let decl = referredDecls[base],
         decl.kind <= .typeDecl
      {
        fatalError("not implemented")
      } else {
        constraints.append(LocatableConstraint(
          .member(l: inferredTypes[domain]!, m: checker.ast[id].baseName, r: inferredType),
          node: AnyNodeID(id),
          cause: .member))
      }

    case .implicit:
      fatalError("not implemented")
    }
  }

  mutating func visit(nil i: NodeID<NilExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(sequence id: NodeID<SequenceExpr>) {
    let root: AnyExprID
    switch checker.ast[id] {
    case .unfolded(let exprs):
      // Fold the sequence.
      let tree = foldSequenceExpr(exprs)
      root = tree.desugars(ast: &checker.ast)
      checker.ast[id] = .root(root)

    case .root(let r):
      root = r
    }

    inferredTypes[root] = inferredTypes[id]
    root.accept(&self)
    inferredTypes[id] = inferredTypes[root]
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
    var expr = checker.ast[id]

    // If the expected type is a tuple compatible with the shape of the expression, propagate that
    // information down the expression tree. Otherwise, infer the type of the expression from the
    // leaves and use type constraints to detect potential mismatch.
    if case .tuple(let type) = inferredTypes[id],
       type.elements.elementsEqual(expr.elements, by: { (a, b) in a.label == b.value.label })
    {
      for i in 0 ..< expr.elements.count {
        modifying(&expr.elements[i].value, { element in
          inferredTypes[element.value] = type.elements[i].type
          element.value.accept(&self)
        })
      }
    } else {
      // Infer the type of the expression.
      var elements: [TupleType.Element] = []
      for i in 0 ..< expr.elements.count {
        modifying(&expr.elements[i].value, { element in
          inferredTypes[element.value] = nil
          element.value.accept(&self)

          elements.append(TupleType.Element(
            label: element.label,
            type: inferredTypes[element.value]!))
        })
      }
      assume(typeOf: id, equals: .tuple(TupleType(elements)))
    }

    assert(inferredTypes[id] != nil)
    checker.ast[id] = expr
  }

  /// Returns the declarations to which the specified name may refer along with their overarching
  /// type before contextualization. Ill-formed declarations are ignored.
  private mutating func resolve(
    stem: Identifier,
    labels: [String?],
    notation: OperatorNotation?,
    inDeclSpaceOf s: AnyScopeID? = nil
  ) -> [(decl: AnyDeclID, type: Type)] {
    // Search for the referred declaration.
    let matches: TypeChecker.DeclSet
    if let s = s {
      matches = checker.lookup(stem, introducedInDeclSpaceOf: s, inScope: scope)
    } else {
      matches = checker.lookup(unqualified: stem, inScope: scope)
    }

    // TODO: Filter by labels and operator notation

    // Returns the matches along with their contextual type and associated constraints.
    return matches.compactMap({ (match) -> (AnyDeclID, Type)? in
      // Realize the type of the declaration.
      guard var type = checker.realize(decl: match) else { return nil }

      // Erase parameter conventions.
      if case .parameter(let t) = type {
        type = t.bareType
      }

      return (match, type)
    })
  }

  private mutating func assume<T: ExprID>(typeOf id: T, equals inferredType: Type) {
    if let expectedType = inferredTypes[id] {
      // Create a subtying constraint if the expected type might be above the inferred type.
      // Otherwise, create an equality constraint.
      let constraint: Constraint
      switch inferredType {
      case .never,
           .variable,
           _ where !expectedType.isLeaf:
        constraint = .equalityOrSubtyping(l: inferredType, r: expectedType)
      default:
        constraint = .equality(l: inferredType, r: expectedType)
      }
      constraints.append(LocatableConstraint(constraint, node: AnyNodeID(id)))
    } else {
      inferredTypes[id] = inferredType
    }
  }

  private mutating func assignToError<T: ExprID>(_ id: T) {
    if inferredTypes[id] == nil {
      inferredTypes[id] = .error(ErrorType())
    }
  }

}

extension ConstraintGenerator {

  /// A folded sequence of binary operations.
  indirect enum Tree {

    case node(operator: NodeID<NameExpr>, left: Tree, right: Tree)

    case leaf(AnyExprID)

    // Desugars the tree.
    func desugars(ast: inout AST) -> AnyExprID {
      switch self {
      case .node(let op, let left, let right):
        switch ast[op].stem.value {
        case "=":
          // `=` is right-associative and has the lowest precedence.
          let lhs = left.desugars(ast: &ast)
          let rhs = right.desugars(ast: &ast)
          let id = AnyExprID(ast.insert(AssignExpr(left: lhs, right: rhs)))
          ast.ranges[id] = ast.ranges[lhs] ..< ast.ranges[rhs]
          return id

        default:
          fatalError("not implemented")
        }

      case .leaf(let id):
        return id
      }
    }

  }

  /// Folds a sequence of binary expressions into a tree.
  mutating func foldSequenceExpr(_ exprs: [AnyExprID]) -> Tree {
    var tree = Tree.node(
      operator: NodeID(converting: exprs[1])!, left: .leaf(exprs[0]), right: .leaf(exprs[2]))
    foldSequenceExpr(exprs[3...], into: &tree)
    return tree
  }

  /// Folds the remainder of a sequence of binary expressions into `accumulatedResult`.
  mutating func foldSequenceExpr(
    _ subexprs: ArraySlice<AnyExprID>,
    into accumulatedResult: inout Tree
  ) {
    // End of iteration.
    if subexprs.isEmpty { return }

    // Read the operator.
//    let i = subexprs.startIndex
//    let op = NodeID<NameExpr>(converting: subexprs[i]) ?? unreachable("invalid sequence")

    fatalError("not implemented")
  }

}
