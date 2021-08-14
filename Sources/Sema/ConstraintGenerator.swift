import AST

/// An AST visitor that generates type constraints on expressions.
struct ConstraintGenerator: NodeWalker {

  typealias Result = Bool

  var parent: Node?

  var innermostSpace: DeclSpace?

  /// A pointer to the system in which new constraints are inserted.
  let system: UnsafeMutablePointer<ConstraintSystem>

  init(system: UnsafeMutablePointer<ConstraintSystem>, useSite: DeclSpace) {
    self.system = system
    self.innermostSpace = useSite
  }

  mutating func willVisit(_ expr: Expr) -> (shouldWalk: Bool, nodeBefore: Expr) {
    // Skip the recursive descent into expressions that have errors.
    return (!expr.type.hasErrors, expr)
  }

  mutating func willVisit(_ pattern: Pattern) -> (shouldWalk: Bool, nodeAfter: Pattern) {
    // There's nothing to do if the pattern already has a type.
    return (pattern.type is UnresolvedType, pattern)
  }

  func visit(_ node: BoolLiteralExpr) -> Bool {
    fatalError("not implemented")
  }

  func visit(_ node: IntLiteralExpr) -> Bool {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    let literalType = node.type.context.getBuiltinType(named: "IntLiteral")!
    system.pointee.insert(
      RelationalConstraint(
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
    guard traverse(node) else { return false }

    // Don't create any constraint if the left operand has an error type.
    guard !(node.lvalue.type is ErrorType) else { return true }

    system.pointee.insert(
      RelationalConstraint(
        kind: .subtyping, lhs: node.rvalue.type, rhs: node.lvalue.type,
        at: ConstraintLocator(node, .assignment)))

    return true
  }

  mutating func visit(_ node: BaseCastExpr) -> Bool {
    guard traverse(node) else { return false }

    // FIXME: There should be a relationshup with the value's type.
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    return true
  }

  mutating func visit(_ node: DynCastExpr) -> Bool {
    guard traverse(node) else { return false }

    // FIXME: There should be a relationshup with the value's type.
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    return true
  }

  mutating func visit(_ node: UnsafeCastExpr) -> Bool {
    guard traverse(node) else { return false }

    // FIXME: We should implement sanity checks on the cast's validity w.r.t. the value's type.
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    return true
  }

  mutating func visit(_ node: TupleExpr) -> Bool {
    guard traverse(node) else { return false }

    // Synthetize a type from the tuple's elements.
    let elems = node.elems.map({ elem in
      TupleType.Elem(label: elem.label, type: elem.value.type)
    })
    node.type = node.type.context.tupleType(elems)

    return true
  }

  mutating func visit(_ node: CallExpr) -> Bool {
    guard traverse(node) else { return false }

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

    return true
  }

  func visit(_ node: UnresolvedDeclRefExpr) -> Bool {
    return true
  }

  mutating func visit(_ node: UnresolvedMemberExpr) -> Bool {
    guard traverse(node) else { return false }

    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    system.pointee.insert(
      ValueMemberConstraint(
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

  func visit(_ node: OverloadedDeclRefExpr) -> Bool {
    assert(node.declSet.count >= 1)

    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    system.pointee.insert(
      OverloadBindingConstraint(
        node.type,
        declSet: node.declSet,
        useSite: innermostSpace!,
        at: ConstraintLocator(node)))

    return true
  }

  func visit(_ node: DeclRefExpr) -> Bool {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    return true
  }

  func visit(_ node: TypeDeclRefExpr) -> Bool {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    return true
  }

  mutating func visit(_ node: MemberDeclRefExpr) -> Bool {
    guard traverse(node) else { return false }

    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    return true
  }

  mutating func visit(_ node: TupleMemberExpr) -> Bool {
    guard traverse(node) else { return false }

    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    system.pointee.insert(
      TupleMemberConstraint(
        node.base.type, hasMemberAt: node.memberIndex, ofType: node.type,
        at: ConstraintLocator(node, .tupleElem(node.memberIndex))))

    return true
  }

  mutating func visit(_ node: AsyncExpr) -> Bool {
    guard traverse(node) else { return false }
    node.type = node.type.context.asyncType(of: node.value.type)

    return true
  }

  mutating func visit(_ node: AwaitExpr) -> Bool {
    guard traverse(node) else { return false }

    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    let awaitedType = node.type.context.asyncType(of: node.type)
    system.pointee.insert(
      RelationalConstraint(
        kind: .oneWayEquality, lhs: awaitedType, rhs: node.value.type,
        at: ConstraintLocator(node.value)))

    return true
  }

  mutating func visit(_ node: AddrOfExpr) -> Bool {
    guard traverse(node) else { return false }

    if node.value.type is InoutType {
      node.type = node.value.type
    } else {
      node.type = node.type.context.inoutType(of: node.value.type)
    }

    return true
  }

  mutating func visit(_ node: MatchExpr) -> Bool {
    guard traverse(node) else { return false }

    // The the heavy-lifting has already been done by the pre-checker. There's nothing more to do
    // unless the match is treated as an expression.
    guard node.isSubexpr else { return true }

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

    return true
  }

  mutating func visit(_ node: WildcardExpr) -> Bool {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    return true
  }

  func visit(_ node: ErrorExpr) -> Bool {
    assert(node.type is ErrorType)
    return true
  }

  func visit(_ node: NamedPattern) -> Bool {
    guard node.decl.state < .typeChecked else {
      assert(!(node.type is UnresolvedType))
      return true
    }

    node.type = TypeVar(context: node.type.context, node: node)
    return true
  }

  mutating func visit(_ node: TuplePattern) -> Bool {
    guard traverse(node) else { return false }

    // Synthetize a type from the tuple's elements.
    let elems = node.elems.map({ elem in
      TupleType.Elem(label: elem.label, type: elem.pattern.type)
    })
    node.type = node.type.context.tupleType(elems)

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

      system.pointee.insert(
        RelationalConstraint(
          kind: .equality, lhs: node.type, rhs: signType,
          at: ConstraintLocator(node.subpattern)))
    }

    return true
  }

  func visit(_ node: WildcardPattern) -> Bool {
    if node.type is UnresolvedType {
      node.type = TypeVar(context: node.type.context, node: node)
    }

    return true
  }

}
