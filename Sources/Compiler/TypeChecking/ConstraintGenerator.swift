import Utils

/// A visitor that generates constraints based on the structure of the AST.
struct ConstraintGenerator: ExprVisitor {

  typealias Result = AnyExprID

  /// A borrowed projection of the type checker that uses this constraint generator.
  var checker: TypeChecker!

  /// The scope in which the AST is visited.
  var scope: AnyScopeID

  /// The set of type constraints being generated.
  var constraints: [LocatableConstraint] = []

  /// A table mapping visited expressions to their inferred types.
  var inferredTypes = ExprMap<Type>()

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
    let trait = TraitType(named: "ExpressibleByIntegerLiteral", ast: checker.ast)
      ?? unreachable()

    switch inferredTypes[id] {
    case .some(.variable(let tau)):
      // The type is a variable, possibly constrained elsewhere; constrain it to either be `Int` or
      // conform to `ExpressibleByIntegerLiteral`.
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

    switch checker.ast[id].domain {
    case .none:
      // Search for the referred declaration with an unqualified lookup.
      let matches = checker.lookup(unqualified: stem.value, inScope: scope)

      if matches.isEmpty {
        checker.diagnostics.insert(.undefined(name: checker.ast[id].baseName, range: stem.range))
        inferredTypes[id] = .error(ErrorType())
        return AnyExprID(id)
      }

      /// Realizes the type of a match.
      func realize(match: AnyDeclID) -> Type {
        let type = checker.realize(decl: matches.first!) ?? .error(ErrorType())

        if case .parameter(let type) = type {
          // Erase parameter conventions.
          return type.bareType
        } else {
          return type
        }
      }

      // Realize the type of the matches.
      var inferredType: Type
      if matches.count == 1 {
        // TODO: Check if notations/labels match
        inferredType = realize(match: matches.first!)
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

}
