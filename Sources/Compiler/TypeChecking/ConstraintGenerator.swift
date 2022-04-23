import Utils

/// A visitor that generates constraints based on the structure of the AST.
struct ConstraintGenerator: ExprVisitor {

  typealias Result = AnyExprID

  /// A borrowed projection of the type checker that uses this constraint generator.
  var checker: TypeChecker!

  /// The scope in which the AST is visited.
  var scope: AnyScopeID

  /// A table mapping visited expressions to their inferred types.
  var inferredTypes = ExprMap<Type>()

  /// The set of type constraints being generated.
  var constraints: [LocatableConstraint] = []

  /// Indicates that the generater encounted one or more errors.
  var hasErrors = false

  mutating func visit(async i: NodeID<AsyncExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(await i: NodeID<AwaitExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(boolLiteral i: NodeID<BoolLiteralExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(bufferLiteral i : NodeID<BufferLiteralExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(charLiteral i: NodeID<CharLiteralExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(cond i: NodeID<CondExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(floatLiteral i: NodeID<FloatLiteralExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(funCall id: NodeID<FunCallExpr>) -> AnyExprID {
    defer { assert(inferredTypes[id] != nil) }

    // Infer the type of the callee.
    let callee = checker.ast[id].callee.accept(&self)
    checker.ast[id].callee = callee

    // There are four cases to consider:
    // 1. We failed to infer the type of the callee. In that case there's nothing more we can do.
    // 2. We deternined that the callee refers to a nominal type declaration. In that case, we
    //    desugar a constructor call.
    // 3. We determined the exact type of the callee, and its a callable. In that case, we may
    //    propagate that information top-down to refine the inference of the arguments' types.
    // 4. We determined that the callee is overloaded. In that case we must rely on argument labels
    //    and bottom-up inference to constrain to select the appropriate candidate.

    // 1st case
    if case .error = inferredTypes[callee] {
      inferredTypes[id] = .error(ErrorType())
      return AnyExprID(id)
    }

    // 2nd case
    // TODO

    // 3rd case
    if let calleeType = inferredTypes[callee]?.base as? CallableType {
      // Collect the call labels.
      let labels = checker.ast[id].arguments.map({ $0.value.label?.value })

      // Check that the labels inferred from the callee are consistent with that of the call.
      let calleeLabels = calleeType.inputs.map({ $0.label })
      if calleeLabels != labels {
        hasErrors = true
        checker.diagnostics.insert(.incompatibleLabels(
          found: calleeLabels, expected: labels, range: checker.ast.ranges[callee]))
        inferredTypes[id] = .error(ErrorType())
        return AnyExprID(id)
      }

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

        checker.ast[id].arguments[i].value.value = argument.value.accept(&self)
      }

      // Constrain the type of the call.
      if let expectedType = inferredTypes[id] {
        constraints.append(LocatableConstraint(
          .equality(l: expectedType, r: calleeType.output), node: AnyNodeID(id)))
      } else {
        inferredTypes[id] = calleeType.output
      }

      return AnyExprID(id)
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
      let argumentValue = checker.ast[id].arguments[i].value.value.accept(&self)
      checker.ast[id].arguments[i].value.value = argumentValue

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

    return AnyExprID(id)
  }

  mutating func visit(integerLiteral id: NodeID<IntegerLiteralExpr>) -> AnyExprID {
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

    return AnyExprID(id)
  }

  mutating func visit(lambda i: NodeID<LambdaExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(mapLiteral i: NodeID<MapLiteralExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(match i: NodeID<MatchExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(matchCase i: NodeID<MatchCaseExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(name id: NodeID<NameExpr>) -> AnyExprID {
    let stem = checker.ast[id].stem

    // Resolves the name.
    switch checker.ast[id].domain {
    case .none:
      let results = resolve(
        stem: checker.ast[id].stem.value,
        labels: checker.ast[id].labels,
        notation: checker.ast[id].notation)

      if results.isEmpty {
        hasErrors = true
        checker.diagnostics.insert(.undefined(name: checker.ast[id].baseName, range: stem.range))
        inferredTypes[id] = .error(ErrorType())
        return AnyExprID(id)
      }

      var inferredType: Type
      if results.count == 1 {
        inferredType = results[0].type
        constraints.append(contentsOf: results[0].constraints.map({ c in
          LocatableConstraint(c, node: AnyNodeID(id))
        }))
      } else {
        // TODO: Create an overload constraint
        fatalError("not implemented")
      }

      if let expectedType = inferredTypes[id] {
        constraints.append(LocatableConstraint(
          .equality(l: expectedType, r: inferredType), node: AnyNodeID(id)))
      } else {
        inferredTypes[id] = inferredType
      }

    case .implicit, .explicit:
      fatalError("not implemented")
    }

    return AnyExprID(id)
  }

  mutating func visit(nil i: NodeID<NilExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(storedProjection i: NodeID<StoredProjectionExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(stringLiteral i: NodeID<StringLiteralExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(subscriptCall i: NodeID<SubscriptCallExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(tuple id: NodeID<TupleExpr>) -> AnyExprID {
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
          element.value = element.value.accept(&self)
        })
      }
    } else {
      // Infer the type of the expression.
      var elements: [TupleType.Element] = []
      for i in 0 ..< expr.elements.count {
        modifying(&expr.elements[i].value, { element in
          inferredTypes[element.value] = nil
          element.value = element.value.accept(&self)

          elements.append(TupleType.Element(
            label: element.label,
            type: inferredTypes[element.value]!))
        })
      }
      let inferredType = Type.tuple(TupleType(elements))

      // If there was an expected type, constrain it to be equal to the inferred type. Otherwise,
      // assign the latter to the expression.
      if let expectedType = inferredTypes[id] {
        constraints.append(LocatableConstraint(
          .equality(l: expectedType, r: inferredType), node: AnyNodeID(id)))
      } else {
        inferredTypes[id] = inferredType
      }
    }

    assert(inferredTypes[id] != nil)
    checker.ast[id] = expr
    return AnyExprID(id)
  }

  mutating func visit(unfolded i: NodeID<UnfoldedExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  /// Resolves the declaration to which the given name may refer.
  private mutating func resolve(
    stem: Identifier,
    labels: [String?],
    notation: OperatorNotation?
  ) -> [(decl: AnyDeclID, type: Type, constraints: [Constraint])] {
    // Search for the referred declaration with an unqualified lookup.
    let matches = checker.lookup(unqualified: stem, inScope: scope)

    // TODO: Filter by labels and operator notation

    // Returns the matches along with their contextual type and associated constraints.
    return matches.compactMap({ (match) -> (AnyDeclID, Type, [Constraint])? in
      // Realize the type of the declaration.
      guard var type = checker.realize(decl: match) else { return nil }

      // Erase parameter conventions.
      if case .parameter(let t) = type {
        type = t.bareType
      }

      // Substitute generic parameters.
      var cs: [Constraint]
      let scope = checker.scopeHierarchy.container[match]!
      (type, cs) = checker.contextualize(type: type, inScope: scope)

      return (match, type, cs)
    })
  }

}
