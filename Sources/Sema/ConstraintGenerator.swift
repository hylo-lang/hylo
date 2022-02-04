import AST

/// An AST visitor that generates type constraints on expressions.
struct ConstraintGenerator: NodeWalker {

  typealias Result = Bool

  var parent: Node?

  var innermostSpace: DeclSpace?

  /// A Boolean value that indicates whether the walker encountered errors.
  var hasErrors = false

  /// The expected type of the expression, based on the context in which it appears.
  var fixedType: ValType?

  /// A pointer to the system in which new constraints are inserted.
  let system: UnsafeMutablePointer<ConstraintSystem>

  init(system: UnsafeMutablePointer<ConstraintSystem>, useSite: DeclSpace, fixedType: ValType?) {
    self.system = system
    self.innermostSpace = useSite
    self.fixedType = fixedType
  }

  mutating func willVisit(_ expr: Expr) -> (shouldWalk: Bool, nodeBefore: Expr) {
    // Skip the recursive descent into expressions that have errors, based on the assumption that
    // these errors must have beeb reported already.
    return (!expr.type[.hasErrors], expr)
  }

  mutating func willVisit(_ pattern: Pattern) -> (shouldWalk: Bool, nodeBefore: Pattern) {
    // There's nothing to do if the pattern already has a type.
    return (true, pattern)
  }

  mutating func visit(_ node: BoolLiteralExpr) -> Bool {
    let boolType = node.type.context.getTypeDecl(for: .Bool)!.instanceType
    prepare(expr: node, fixedType: fixedType, inferredType: boolType)
    return true
  }

  mutating func visit(_ node: IntLiteralExpr) -> Bool {
    prepare(expr: node, fixedType: fixedType, inferredType: nil)

    // The type of the node must be expressible by `Builtin::IntLiteral`.
    let literalType = node.type.context.getBuiltinType(named: "IntLiteral")!
    insert(RelationalConstraint(
      kind: .conversion, lhs: node.type, rhs: literalType,
      at: ConstraintLocator(node)))

    return true
  }

  func visit(_ node: FloatLiteralExpr) -> Bool {
    fatalError("not implemented")
  }

  func visit(_ node: StringLiteralExpr) -> Bool {
    fatalError("not implemented")
  }

  mutating func visit(_ node: AssignExpr) -> Bool {
    if let fixedType = fixedType, !fixedType.isUnit {
      // Assign expressions always have the unit type, so this constraint will necessarily fail. We
      // add it so that the error will be diagnosed.
      insert(RelationalConstraint(
        kind: .equality, lhs: node.type, rhs: fixedType,
        at: ConstraintLocator(node)))
    }

    // Don't create any constraint if the left operand has an error type.
    fixedType = nil
    var (shouldContinue, _) = walk(expr: node.lvalue)
    guard shouldContinue else { return false }
    guard shouldContinue && !node.lvalue.type.isError else { return false }

    fixedType = node.lvalue.type
    (shouldContinue, _) = walk(expr: node.rvalue)
    guard shouldContinue else { return false }

    return true
  }

  func visit(_ node: BaseCastExpr) -> Bool {
    fatalError("unreachable")
  }

  mutating func visit(_ node: StaticCastExpr) -> Bool {
    // Note: the type of the expression is set to the RHS by the pre-checker.
    insert(RelationalConstraint(
      kind: .subtyping, lhs: node.value.type, rhs: node.type,
      at: ConstraintLocator(node)))

    prepare(expr: node, fixedType: fixedType, inferredType: nil)
    fixedType = nil
    guard traverse(node) else { return false }
    return true
  }

  mutating func visit(_ node: RuntimeCastExpr) -> Bool {
    // Note: the type of the expression is set to the RHS by the pre-checker.
    prepare(expr: node, fixedType: fixedType, inferredType: nil)
    fixedType = nil
    guard traverse(node) else { return false }
    return true
  }

  mutating func visit(_ node: PointerCastExpr) -> Bool {
    // Note: the type of the expression is set to the RHS by the pre-checker.
    prepare(expr: node, fixedType: fixedType, inferredType: nil)

    // The LHS must be a built-in pointer.
    fixedType = node.type.context.getBuiltinType(named: "Pointer")
    guard traverse(node) else { return false }
    return true
  }

  mutating func visit(_ node: TupleExpr) -> Bool {
    let fixedTypeForThisNode = fixedType

    // If the fixed type is a tuple type of the correct size, we can deconstruct it to obtain a
    // fixed type for each element. Otherwise, we should visit each sub-expression without any
    // contextual information.
    if let tupleType = fixedType as? TupleType, tupleType.elems.count == node.elems.count {
      for i in 0 ..< node.elems.count {
        fixedType = tupleType.elems[i].type
        let (shouldContinue, _) = walk(expr: node.elems[i].value)
        guard shouldContinue else { return false }
      }
    } else {
      fixedType = nil
      guard traverse(node) else { return false }
    }

    // Infer type of the tuple from the type of each element.
    let elems = node.elems.map({ elem in
      TupleType.Elem(label: elem.label, type: elem.value.type)
    })
    let inferredType = node.type.context.tupleType(elems)

    prepare(expr: node, fixedType: fixedTypeForThisNode, inferredType: inferredType)
    return true
  }

  mutating func visit(_ node: CallExpr) -> Bool {
    prepare(expr: node, fixedType: fixedType, inferredType: nil)
    fixedType = nil
    guard traverse(node) else { return false }

    // Synthetize the type of a function from the call's arguments.
    var params: [FunType.Param] = []
    for (i, arg) in node.args.enumerated() {
      // If the argument is passed mutating, the parameter must be mutating and its raw type
      // should be the same as the argument's type.
      if arg.value is AddrOfExpr {
        let param = FunType.Param(
          label: arg.label, policy: .inout, rawType: arg.value.type)
        params.append(param)
      }

      // Otherwise, the solver must infer the parameter's raw type and passing policy based on
      // the argument's type, using a subtyping constraint.
      else {
        let paramType = TypeVar(context: node.type.context, node: arg.value)
        insert(RelationalConstraint(
          kind: .subtyping, lhs: arg.value.type, rhs: paramType,
          at: ConstraintLocator(node, .argument(i))))
        params.append(FunType.Param(label: arg.label, type: paramType))
      }
    }

    let funType = node.type.context.funType(params: params, retType: node.type)
    insert(RelationalConstraint(
      kind: .equality, lhs: node.fun.type, rhs: funType,
      at: ConstraintLocator(node.fun)))

    return true
  }

  func visit(_ node: UnresolvedDeclRefExpr) -> Bool {
    return true
  }

  mutating func visit(_ node: UnresolvedMemberExpr) -> Bool {
    prepare(expr: node, fixedType: fixedType, inferredType: nil)
    fixedType = nil
    guard traverse(node) else { return false }

    insert(ValueMemberConstraint(
      node.base.type,
      hasValueMember: node.memberName,
      ofType: node.type,
      useSite: innermostSpace!,
      at: ConstraintLocator(node, .valueMember(node.memberName))))

    return true
  }

  func visit(_ node: UnresolvedQualDeclRefExpr) -> Bool {
    return true
  }

  mutating func visit(_ node: OverloadedDeclRefExpr) -> Bool {
    assert(node.declSet.count >= 1)
    prepare(expr: node, fixedType: fixedType, inferredType: nil)

    insert(OverloadBindingConstraint(
      node.type,
      declSet: node.declSet,
      useSite: innermostSpace!,
      at: ConstraintLocator(node)))

    return true
  }

  mutating func visit(_ node: DeclRefExpr) -> Bool {
    prepare(expr: node, fixedType: fixedType, inferredType: nil)
    return true
  }

  mutating func visit(_ node: TypeDeclRefExpr) -> Bool {
    prepare(expr: node, fixedType: fixedType, inferredType: nil)
    return true
  }

  mutating func visit(_ node: MemberDeclRefExpr) -> Bool {
    prepare(expr: node, fixedType: fixedType, inferredType: nil)
    fixedType = nil
    return traverse(node)
  }

  mutating func visit(_ node: TupleMemberExpr) -> Bool {
    prepare(expr: node, fixedType: fixedType, inferredType: nil)
    fixedType = nil
    guard traverse(node) else { return false }

    insert(TupleMemberConstraint(
      node.base.type, hasMemberAt: node.memberIndex, ofType: node.type,
      at: ConstraintLocator(node, .tupleElem(node.memberIndex))))

    return true
  }

  mutating func visit(_ node: AsyncExpr) -> Bool {
    // First, we must realize the type of the underlying function that represents the body of the
    // expression. We know that this function has a type `() -> T`, assuming the expression has a
    // type `async T`, but we still need to infer `T`.
    let fixedTypeForThisNode = fixedType
    let context = node.type.context

    if node.body.state < .realized {
      let fixedBareType = (fixedType as? AsyncType)?.base

      if let type = fixedBareType, !type[.hasVariables] {
        // We have a concrete fixed type, we can use it to select `T`.
        node.body.type = context.funType(params: [], retType: type)
        node.body.setState(.realized)
      } else if var expr = node.body.singleExprBody {
        // The function is expression-bodied, so we can infer `T` as the type of a sub-expression.
        // FIXME: This is a little hacky.
        fixedType = fixedBareType
        _ = TypeChecker.check(
          expr: &expr,
          fixedType: fixedBareType,
          useSite: node.body.body!,
          freeTypeVarSubstPolicy: .bindToError)
        node.body.type = context.funType(params: [], retType: expr.type)
        node.body.setState(.realized)
      } else {
        // Complain that we can't infer `T` over multiple statements.
        context.report(.complexReturnTypeInference(range: node.range))
        node.body.type = context.errorType
        node.body.setState(.invalid)
        node.type = context.errorType
        hasErrors = true
        return true
      }
    }

    // Now that we've determined `T`, we can type check the body of the expression.
    _ = TypeChecker.check(decl: node.body)
    guard let inferredBareType = (node.body.type as? FunType)?.retType else {
      node.type = context.errorType
      return true
    }
    let inferredType = context.asyncType(of: inferredBareType)

    prepare(expr: node, fixedType: fixedTypeForThisNode, inferredType: inferredType)
    return true
  }

  mutating func visit(_ node: AwaitExpr) -> Bool {
    let fixedTypeForThisNode = fixedType

    // If we have a fixed type for this node, we can create the fixed type of the sub-expression.
    if let bareType = fixedType {
      fixedType = bareType.context.asyncType(of: bareType)
    } else {
      fixedType = nil
    }
    guard traverse(node) else { return false }

    if let awaitedType = node.value.type as? AsyncType {
      prepare(expr: node, fixedType: fixedTypeForThisNode, inferredType: awaitedType.base)
    } else {
      prepare(expr: node, fixedType: fixedTypeForThisNode, inferredType: nil)
      let awaitedType = node.type.context.asyncType(of: node.type)
      system.pointee.insert(RelationalConstraint(
        kind: .oneWayEquality, lhs: awaitedType, rhs: node.value.type,
        at: ConstraintLocator(node.value)))
    }

    return true
  }

  mutating func visit(_ node: AddrOfExpr) -> Bool {
    guard traverse(node) else { return false }
    node.type = node.value.type
    return true
  }

  mutating func visit(_ node: MatchExpr) -> Bool {
    // Note that the subject of the match has aleady been type checked by the pre checker.
    prepare(expr: node, fixedType: fixedType, inferredType: nil)
    let caseType = fixedType
    for i in 0 ..< node.cases.count {
      if node.isSubexpr {
        // If the match expression is used as a sub-expression, then all cases must produce a value
        // with a type coercible to that of the match expression itself. If we have a fixed type,
        // we can use it to type each case as if it had appeared in place of the match. Otherwise,
        // we'll require require all cases to have exactly the same type as the first one.
        guard let expr = node.cases[i].singleExprBody, !(expr is ErrorExpr) else { continue }
        fixedType = caseType
        _ = expr.accept(&self)

        if caseType == nil {
          let kind: RelationalConstraint.Kind = (i == 0) ? .equality : .oneWayEquality
          insert(RelationalConstraint(
            kind: kind, lhs: expr.type, rhs: node.type,
            at: ConstraintLocator(expr)))
        }
      } else {
        // If the match expression is used as a control statement, then we can just walk the body
        // of each case as a regular brace statement.
        walk(stmt: node.cases[i].body)
      }
    }

    return true
  }

  mutating func visit(_ node: WildcardExpr) -> Bool {
    prepare(expr: node, fixedType: fixedType, inferredType: nil)
    return true
  }

  func visit(_ node: ErrorExpr) -> Bool {
    assert(node.type.isError)
    return true
  }

  mutating func visit(_ node: NamedPattern) -> Bool {
    assert((node.decl.state < .typeChecked) || !node.type.isUnresolved)
    prepare(pattern: node, fixedType: fixedType, inferredType: nil)
    return true
  }

  mutating func visit(_ node: TuplePattern) -> Bool {
    let fixedTypeForThisNode = fixedType
    fixedType = nil
    guard traverse(node) else { return false }

    // Infer type of the tuple from the type of each element.
    let elems = node.elems.map({ elem in
      TupleType.Elem(label: elem.label, type: elem.pattern.type)
    })
    let inferredType = node.type.context.tupleType(elems)

    prepare(pattern: node, fixedType: fixedTypeForThisNode, inferredType: inferredType)
    return true
  }

  mutating func visit(_ node: BindingPattern) -> Bool {
    guard traverse(node) else { return false }

    node.type = node.subpattern.type

    if let sign = node.sign {
      // Contextualize the type signature.
      guard let signType = TypeChecker.contextualize(
        sign: sign, from: innermostSpace!, system: &system.pointee)
      else {
        node.type = node.type.context.errorType
        return true
      }

      system.pointee.insert(RelationalConstraint(
        kind: .equality, lhs: node.type, rhs: signType,
        at: ConstraintLocator(node.subpattern)))
    }

    return true
  }

  mutating func visit(_ node: WildcardPattern) -> Bool {
    prepare(pattern: node, fixedType: fixedType, inferredType: nil)
    return true
  }

  private mutating func insert(_ constraint: Constraint) {
    system.pointee.insert(constraint)
  }

  private mutating func prepare(expr: Expr, fixedType: ValType?, inferredType: ValType?) {
    if expr.type.isUnresolved {
      expr.type = inferredType ?? TypeVar(context: expr.type.context, node: expr)
    } else if let inferredType = inferredType {
      insert(RelationalConstraint(
        kind: .equality, lhs: expr.type, rhs: inferredType,
        at: ConstraintLocator(expr)))
    }

    if let fixedType = fixedType {
      insert(RelationalConstraint(
        kind: .subtyping, lhs: expr.type, rhs: fixedType,
        at: ConstraintLocator(expr)))
    }
  }

  private mutating func prepare(pattern: Pattern, fixedType: ValType?, inferredType: ValType?) {
    if pattern.type.isUnresolved {
      pattern.type = inferredType ?? TypeVar(context: pattern.type.context, node: pattern)
    } else if let inferredType = inferredType {
      insert(RelationalConstraint(
        kind: .equality, lhs: pattern.type, rhs: inferredType,
        at: ConstraintLocator(pattern)))
    }

    if let fixedType = fixedType {
      // If the pattern is a wildcard, there's nothing more we can infer from its type. Hence, we
      // can assume it has exactly the expected type.
      let kind: RelationalConstraint.Kind = pattern is WildcardPattern
        ? .equality
        : .subtyping
      insert(RelationalConstraint(
        kind: kind, lhs: pattern.type, rhs: fixedType,
        at: ConstraintLocator(pattern)))
    }
  }

}
