import Utils

/// A visitor that generates constraints based on the structure of the AST.
struct ConstraintGenerator: ExprVisitor {

  typealias Result = AnyExprID

  /// A borrowed projection of the type checker that uses this constraint generator.
  var checker: TypeChecker!

  /// The set of type constraints being generated.
  var constraints: [LocatableConstraint] = []

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

  mutating func visit(funCall i: NodeID<FunCallExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

  mutating func visit(integerLiteral id: NodeID<IntegerLiteralExpr>) -> AnyExprID {
    if let expectedType = checker.exprTypes[AnyExprID(id)] {
      // The type of the expression must conform to `ExpressibleByIntegerLiteral`.
      let trait = TraitType(named: "ExpressibleByIntegerLiteral", ast: checker.ast)
        ?? unreachable()
      constraints.append(LocatableConstraint(
        .conformance(l: expectedType, traits: [trait]), node: AnyNodeID(id)))
    } else {
      // Without contextual information, infer the type of the literal as `Val.Int`.
      checker.exprTypes[AnyExprID(id)] = .int(in: checker.ast)
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

  mutating func visit(name i: NodeID<NameExpr>) -> AnyExprID {
    fatalError("not implemented")
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
    if case .tuple(let type) = checker.exprTypes[AnyExprID(id)],
       type.elements.elementsEqual(expr.elements, by: { (a, b) in a.label == b.value.label })
    {
      for i in 0 ..< expr.elements.count {
        modifying(&expr.elements[i].value, { element in
          checker.exprTypes[element.value] = type.elements[i].type
          element.value = element.value.accept(&self)
        })
      }
    } else {
      // Infer the type of the expression.
      var elements: [TupleType.Element] = []
      for i in 0 ..< expr.elements.count {
        modifying(&expr.elements[i].value, { element in
          checker.exprTypes[element.value] = nil
          element.value = element.value.accept(&self)

          elements.append(TupleType.Element(
            label: element.label,
            type: checker.exprTypes[element.value]!))
        })
      }
      let inferredType = Type.tuple(TupleType(elements))

      // If there was an expected type, constrain it to be equal to the inferred type. Otherwise,
      // assign the latter to the expression.
      if let expectedType = checker.exprTypes[AnyExprID(id)] {
        constraints.append(LocatableConstraint(
          .equality(l: expectedType, r: inferredType), node: AnyNodeID(id)))
      } else {
        checker.exprTypes[AnyExprID(id)] = inferredType
      }
    }

    assert(checker.exprTypes[AnyExprID(id)] != nil)
    checker.ast[id] = expr
    return AnyExprID(id)
  }

  mutating func visit(unfolded i: NodeID<UnfoldedExpr>) -> AnyExprID {
    fatalError("not implemented")
  }

}
