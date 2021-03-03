import AST

/// A driver for a constraint generator.
final class CSGenDriver: NodeWalker {

  init(system: UnsafeMutablePointer<ConstraintSystem>, useSite: DeclSpace) {
    self.system = system
    super.init(innermostSpace: useSite)
  }

  /// A pointer to the system in which new constraints are inserted.
  let system: UnsafeMutablePointer<ConstraintSystem>

  override func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
    if expr.type.hasErrors {
      return (true, expr)
    }

    // Assign fresh variables to the expressions with unresolved types.
    if (expr.type is UnresolvedType) &&
        !(expr is UnresolvedDeclRefExpr) && !(expr is UnresolvedQualDeclRefExpr)
    {
      expr.type = TypeVar(context: expr.type.context, node: expr)
    }

    // Generate constraints.
    expr.accept(ConstraintGenerator(system: system, useSite: innermostSpace!))
    return (true, expr)
  }

}

/// An AST visitor that generates type constraints for a statement or an expression.
struct ConstraintGenerator: ExprVisitor {

  typealias ExprResult = Void

  /// A pointer to the system in which new constraints are inserted.
  let system: UnsafeMutablePointer<ConstraintSystem>

  /// The declaration space from which nodes are being visited.
  unowned let useSite: DeclSpace

  func visit(_ node: IntLiteralExpr) {
    let literalType = node.type.context.getBuiltinType(named: "IntLiteral")!
    system.pointee.insert(
      RelationalConstraint(
        kind: .conversion, lhs: node.type, rhs: literalType,
        at: ConstraintLocator(node)))
  }

  func visit(_ node: AssignExpr) {
    system.pointee.insert(
      RelationalConstraint(
        kind: .subtyping, lhs: node.rvalue.type, rhs: node.lvalue.type,
        at: ConstraintLocator(node, .assignment)))
  }

  func visit(_ node: UnsafeCastExpr) {
  }

  func visit(_ node: TupleExpr) {
    // Synthetize a type from the tuple's elements.
    let elems = node.elems.map({ elem in
      TupleType.Elem(label: elem.label, type: elem.value.type)
    })

    node.type = node.type.context.tupleType(elems)
  }

  func visit(_ node: CallExpr) {
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
    system.pointee.insert(
      ValueMemberConstraint(
        node.base.type, hasValueMember: node.memberName, ofType: node.type, useSite: useSite,
        at: ConstraintLocator(node, .valueMember(node.memberName))))
  }

  func visit(_ node: UnresolvedQualDeclRefExpr) {
  }

  func visit(_ node: OverloadedDeclRefExpr) {
    assert(node.declSet.count >= 1)

    system.pointee.insert(
      OverloadBindingConstraint(
        node.type, declSet: node.declSet, useSite: useSite,
        at: ConstraintLocator(node)))
  }

  func visit(_ node: DeclRefExpr) {
  }

  func visit(_ node: TypeDeclRefExpr) {
  }

  func visit(_ node: MemberDeclRefExpr) {
  }

  func visit(_ node: TupleMemberExpr) {
    system.pointee.insert(
      TupleMemberConstraint(
        node.base.type, hasMemberAt: node.memberIndex, ofType: node.type,
        at: ConstraintLocator(node, .tupleElem(node.memberIndex))))
  }

  func visit(_ node: AwaitExpr) {
  }

  func visit(_ node: AddrOfExpr) {
  }

  func visit(_ node: WildcardExpr) {
  }

  func visit(_ node: ErrorExpr) {
  }

}
