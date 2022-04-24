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

    // Infer the type of the callee.
    checker.ast[id].callee.accept(&self)
    let callee = checker.ast[id].callee

    // There are four cases to consider:
    // 1. We failed to infer the type of the callee. In that case there's nothing more we can do.
    // 2. We deternined that the callee refers to a nominal type declaration. In that case, we
    //    desugar a constructor call.
    // 3. We determined the exact type of the callee, and its a callable. In that case, we may
    //    propagate that information top-down to refine the inference of the arguments' types.
    // 4. We determined that the callee is overloaded. In that case we must rely on argument labels
    //    and bottom-up inference to constrain to select the appropriate candidate.

    func propagateDown(calleeType: CallableType) {
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
        if let ctor = checker.memberwiseCtorType(of: NodeID(converting: d)!) {
          propagateDown(calleeType: ctor)
        } else {
          assume(typeOf: id, is: .error(ErrorType()))
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
    if let calleeType = inferredTypes[callee]?.base as? CallableType {
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
      let results = resolve(
        stem: checker.ast[id].stem.value,
        labels: checker.ast[id].labels,
        notation: checker.ast[id].notation)

      if results.isEmpty {
        diagnostics.append(.undefined(name: checker.ast[id].baseName, range: stem.range))
        assume(typeOf: id, is: .error(ErrorType()))
        return
      }

      var inferredType: Type
      if results.count == 1 {
        inferredType = results[0].type
        constraints.append(contentsOf: results[0].constraints.map({ c in
          LocatableConstraint(c, node: AnyNodeID(id))
        }))
        referredDecls[id] = results[0].decl
      } else {
        // TODO: Create an overload constraint
        fatalError("not implemented")
      }

      assume(typeOf: id, is: inferredType)

    case .implicit, .explicit:
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

  private mutating func assume<T: ExprID>(typeOf id: T, is inferredType: Type) {
    if let expectedType = inferredTypes[id] {
      constraints.append(LocatableConstraint(
        .equality(l: inferredType, r: expectedType), node: AnyNodeID(id)))
    } else {
      inferredTypes[id] = inferredType
    }
  }

}
