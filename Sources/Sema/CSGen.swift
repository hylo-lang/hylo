import AST

/// A driver for a constraint generator.
final class CSGenDriver: NodeWalker {

  init(system: UnsafeMutablePointer<ConstraintSystem>, useSite: DeclSpace) {
    self.system = system
    super.init(innermostSpace: useSite)
  }

  /// A pointer to the system in which new constraints are inserted.
  let system: UnsafeMutablePointer<ConstraintSystem>

  override func willVisit(_ expr: Expr) -> (shouldWalk: Bool, nodeBefore: Expr) {
    // Skip the recursive descent into match statements, as the heavy-lifting has already been done
    // by the pre-checker. There's nothing more to do unless the match is treated as an expression.
    if let matchExpr = expr as? MatchExpr, matchExpr.isSubexpr {
      matchExpr.accept(ConstraintGenerator(system: system, useSite: innermostSpace!))
      return (false, matchExpr)
    }

    return (true, expr)
  }

  override func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
    if expr.type.hasErrors {
      return (true, expr)
    }

    // Generate constraints.
    expr.accept(ConstraintGenerator(system: system, useSite: innermostSpace!))
    return (true, expr)
  }

  override func didVisit(_ pattern: Pattern) -> (shouldContinue: Bool, nodeAfter: Pattern) {
    // There's nothing to do if the pattern already has a type.
    if !(pattern.type is UnresolvedType) {
      return (true, pattern)
    }

    pattern.accept(ConstraintGenerator(system: system, useSite: innermostSpace!))
    return (true, pattern)
  }

}

/// An AST visitor that generates type constraints on expressions.
private struct ConstraintGenerator: ExprVisitor, PatternVisitor {

  typealias ExprResult = Void
  typealias PatternResult = Void

  /// A pointer to the system in which new constraints are inserted.
  let system: UnsafeMutablePointer<ConstraintSystem>

  /// The declaration space from which nodes are being visited.
  unowned let useSite: DeclSpace

  func visit(_ node: BoolLiteralExpr) -> Void {
    fatalError("not implemented")
  }

  func visit(_ node: IntLiteralExpr) {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    let literalType = node.type.context.getBuiltinType(named: "IntLiteral")!
    system.pointee.insert(
      RelationalConstraint(
        kind: .conversion, lhs: node.type, rhs: literalType,
        at: ConstraintLocator(node)))
  }

  func visit(_ node: FloatLiteralExpr) -> Void {
    fatalError("not implemented")
  }

  func visit(_ node: StringLiteralExpr) -> Void {
    fatalError("not implemented")
  }

  func visit(_ node: AssignExpr) {
    // Don't create any constraint if the left operand has an error type.
    guard !(node.lvalue.type is ErrorType) else { return }

    system.pointee.insert(
      RelationalConstraint(
        kind: .subtyping, lhs: node.rvalue.type, rhs: node.lvalue.type,
        at: ConstraintLocator(node, .assignment)))
  }

  func visit(_ node: BaseCastExpr) {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }
  }

  func visit(_ node: DynCastExpr) {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }
  }

  func visit(_ node: UnsafeCastExpr) {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }
  }

  func visit(_ node: TupleExpr) {
    // Synthetize a type from the tuple's elements.
    let elems = node.elems.map({ elem in
      TupleType.Elem(label: elem.label, type: elem.value.type)
    })
    node.type = node.type.context.tupleType(elems)
  }

  func visit(_ node: CallExpr) {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    // Synthetize the type of a function from the call's arguments.
    var paramTypeElems: [TupleType.Elem] = []
    for (i, arg) in node.args.enumerated() {
      // The subtyping constraint handle cases where the argument is a subtype of the parameter.
      let paramType = TypeVar(context: node.type.context, node: arg.value)
      system.pointee.insert(
        RelationalConstraint(
          kind: .subtyping, lhs: arg.value.type, rhs: paramType,
          at: ConstraintLocator(node, .argument(i))))
      paramTypeElems.append(TupleType.Elem(label: arg.label, type: paramType))
    }

    let paramType = node.type.context.tupleType(paramTypeElems)
    let funType = node.type.context.funType(paramType: paramType, retType: node.type)
    system.pointee.insert(
      RelationalConstraint(
        kind: .equality, lhs: node.fun.type, rhs: funType,
        at: ConstraintLocator(node.fun)))
  }

  func visit(_ node: UnresolvedDeclRefExpr) {
  }

  func visit(_ node: UnresolvedMemberExpr) {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    system.pointee.insert(
      ValueMemberConstraint(
        node.base.type, hasValueMember: node.memberName, ofType: node.type, useSite: useSite,
        at: ConstraintLocator(node, .valueMember(node.memberName))))
  }

  func visit(_ node: UnresolvedQualDeclRefExpr) {
  }

  func visit(_ node: OverloadedDeclRefExpr) {
    assert(node.declSet.count >= 1)

    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    system.pointee.insert(
      OverloadBindingConstraint(
        node.type, declSet: node.declSet, useSite: useSite,
        at: ConstraintLocator(node)))
  }

  func visit(_ node: DeclRefExpr) {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }
  }

  func visit(_ node: TypeDeclRefExpr) {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }
  }

  func visit(_ node: MemberDeclRefExpr) {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }
  }

  func visit(_ node: TupleMemberExpr) {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    system.pointee.insert(
      TupleMemberConstraint(
        node.base.type, hasMemberAt: node.memberIndex, ofType: node.type,
        at: ConstraintLocator(node, .tupleElem(node.memberIndex))))
  }

  func visit(_ node: AsyncExpr) -> Void {
    node.type = node.type.context.asyncType(of: node.value.type)
  }

  func visit(_ node: AwaitExpr) {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    let awaitedType = node.type.context.asyncType(of: node.type)
    system.pointee.insert(
      RelationalConstraint(
        kind: .oneWayEquality, lhs: awaitedType, rhs: node.value.type,
        at: ConstraintLocator(node.value)))
  }

  func visit(_ node: AddrOfExpr) {
    if node.value.type is InoutType {
      node.type = node.value.type
    } else {
      node.type = node.type.context.inoutType(of: node.value.type)
    }
  }

  func visit(_ node: MatchExpr) {
    assert(node.isSubexpr)

    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    // All cases must produce a value with a type coercible to that of the node.
    // We insert them in reverse order so that the type checker visit them from top to bottom.
    for stmt in node.cases.reversed() {
      let expr = stmt.body.stmts[0] as! Expr
      if !(expr.type is ErrorType) {
        system.pointee.insert(
          RelationalConstraint(
            kind: .subtyping, lhs: expr.type, rhs: node.type, at: ConstraintLocator(stmt)))
      }
    }
  }

  func visit(_ node: WildcardExpr) {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }
  }

  func visit(_ node: ErrorExpr) {
    assert(node.type is ErrorType)
  }

  func visit(_ node: NamedPattern) {
    guard node.decl.state < .typeChecked else {
      assert(!(node.type is UnresolvedType))
      return
    }

    node.type = TypeVar(context: node.type.context, node: node)
  }

  func visit(_ node: TuplePattern) {
    // Synthetize a type from the tuple's elements.
    let elems = node.elems.map({ elem in
      TupleType.Elem(label: elem.label, type: elem.pattern.type)
    })
    node.type = node.type.context.tupleType(elems)
  }

  func visit(_ node: BindingPattern) {
    node.type = node.subpattern.type

    if let sign = node.sign {
      // Contextualize the type signature.
      guard let signType = TypeChecker
              .contextualize(sign: sign, from: useSite, system: &system.pointee)
      else {
        node.type = node.type.context.errorType
        return
      }

      system.pointee.insert(
        RelationalConstraint(
          kind: .equality, lhs: node.type, rhs: signType,
          at: ConstraintLocator(node.subpattern)))
    }
  }

  func visit(_ node: WildcardPattern) {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }
  }

}
