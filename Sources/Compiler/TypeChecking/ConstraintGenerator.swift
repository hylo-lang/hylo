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

  mutating func visit(async i: NodeID<AsyncExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(await i: NodeID<AwaitExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(boolLiteral i: NodeID<BoolLiteralExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(bufferLiteral i : NodeID<BufferLiteralExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(charLiteral i: NodeID<CharLiteralExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(cond i: NodeID<CondExpr>) {
    fatalError("not implemented")
  }

  mutating func visit(floatLiteral i: NodeID<FloatLiteralExpr>) {
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
        assume(typeOf: id, is: .error(ErrorType()))
        return
      }

      // Gather the callee's constraints.
      constraints.append(contentsOf: calleeConstraints.map({ c in
        LocatableConstraint(c, node: AnyNodeID(callee))
      }))

      // Propagate type information to the arguments.
      for i in 0 ..< checker.ast[id].arguments.count {
        let argument = checker.ast[id].arguments[i].value
        let argumentType = Type.variable(TypeVariable(node: AnyNodeID(argument.value)))
        let parameterType = calleeType.inputs[i].type

        inferredTypes[argument.value] = argumentType
        constraints.append(LocatableConstraint(
          .parameter(l: argumentType, r: parameterType),
          node: AnyNodeID(argument.value),
          cause: .callArgument))

        argument.value.accept(&self)
      }

      // Constrain the type of the call.
      assume(typeOf: id, is: calleeType.output)
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
      assume(typeOf: id, is: .error(ErrorType()))
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
          assume(typeOf: id, is: .error(ErrorType()))
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
        assume(typeOf: id, is: .error(ErrorType()))

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
        node: AnyNodeID(argumentValue),
        cause: .callArgument))
    }

    // Constrain the type of the callee.
    let calleeType = Type.lambda(LambdaType(
      environment: .variable(TypeVariable()), inputs: inputs, output: output))
    constraints.append(LocatableConstraint(
      .equality(l: inferredTypes[callee]!, r: calleeType), node: AnyNodeID(callee)))
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

  mutating func visit(lambda i: NodeID<LambdaExpr>) {
    fatalError("not implemented")
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
        assume(typeOf: id, is: .error(ErrorType()))
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

      assume(typeOf: id, is: inferredType)

    case .explicit(let domain):
      // Infer the type of the domain.
      domain.accept(&self)

      // If we failed to infer the type of the domain, there's nothing more we can do.
      if case .error = inferredTypes[domain] {
        assume(typeOf: id, is: .error(ErrorType()))
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
      assume(typeOf: id, is: .tuple(TupleType(elements)))
    }

    assert(inferredTypes[id] != nil)
    checker.ast[id] = expr
  }

  mutating func visit(unfolded i: NodeID<UnfoldedExpr>) {
    fatalError("not implemented")
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

  private mutating func assume<T: ExprID>(typeOf id: T, is inferredType: Type) {
    if let expectedType = inferredTypes[id] {
      constraints.append(LocatableConstraint(
        .equality(l: inferredType, r: expectedType), node: AnyNodeID(id)))
    } else {
      inferredTypes[id] = inferredType
    }
  }

}
