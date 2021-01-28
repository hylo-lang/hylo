import AST

/// An AST visitor that generates type constraints for a statement or an expression.
struct ConstraintGenerator: StmtVisitor, ExprVisitor {

  typealias StmtResult = Void
  typealias ExprResult = Void

  unowned let checker: TypeChecker

  func visit(_ node: PatternBindingDecl) {
    // If the declaration has a signature, it as the authoritative type information.
    if let sign = node.sign,
       let signType = sign.realize(within: node.parentDeclSpace!)
    {
      checker.system.insert(
        EqualityConstraint(
          node.pattern.type, isEqualTo: checker.instanciate(signType),
          at: ConstraintLocator(node, .annotation)))

      // Since the actual type of the pattern if described by the signature, we can treat the
      // initializer as a mere assignment and only impose a subtyping constraint
      if let initializer = node.initializer {
        checker.system.insert(
          SubtypingConstraint(
            initializer.type, isSubtypeOf: node.pattern.type,
            at: ConstraintLocator(node, .assignment)))
      }
    } else if let initializer = node.initializer {
      // Since we could not infer the type of the pattern from a signature, we must infer it
      // directly from the initializer. We use an equality constraint to type the pattern as
      // closely as tightly as possible to the initializer.
      checker.system.insert(
        EqualityConstraint(
          node.pattern.type, isEqualTo: initializer.type,
          at: ConstraintLocator(node, .assignment)))
    }
  }

  func visit(_ node: BraceStmt) {
  }

  func visit(_ node: RetStmt) {
    guard let funDecl = node.funDecl else { return }

    // Retrieve the expected return type.
    let retType: ValType
    if let sign = funDecl.retSign {
      precondition(sign.state != .parsed, "function signature should have been realized")
      retType = sign.state == .realized
        ? checker.instanciate(sign.type)
        : checker.context.unresolvedType
    } else {
      retType = checker.context.unitType
    }

    let valType = node.value?.type ?? checker.context.unitType
    checker.system.insert(
      SubtypingConstraint(
        valType, isSubtypeOf: retType,
        at: ConstraintLocator(node, .returnValue)))
  }

  func visit(_ node: IntLiteralExpr) {
    precondition(checker.context.stdlib != nil, "standard library is not loaded")

    let viewTypeDecl = checker.context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral)!
    checker.system.insert(
      ConformanceConstraint(
        node.type, conformsTo: viewTypeDecl.instanceType as! ViewType,
        at: ConstraintLocator(node)))
  }

  func visit(_ node: AssignExpr) {
    checker.system.insert(
      SubtypingConstraint(
        node.rvalue.type, isSubtypeOf: node.lvalue.type,
        at: ConstraintLocator(node, .assignment)))
  }

  func visit(_ node: CallExpr) {
    // Synthetize the type of a function from the call's arguments.
    var paramTypeElems: [TupleType.Elem] = []
    for arg in node.args {
      // The subtyping constraint handle cases where the argument is a subtype of the parameter.
      let paramType = TypeVar(context: checker.context, node: arg.value)
      checker.system.insert(
        SubtypingConstraint(
          arg.value.type, isSubtypeOf: paramType,
          at: ConstraintLocator(node, .application)))
      paramTypeElems.append(TupleType.Elem(label: arg.label, type: paramType))
    }

    let paramType = checker.context.tupleType(paramTypeElems)
    let funType = checker.context.funType(paramType: paramType, retType: node.type)
    checker.system.insert(
      EqualityConstraint(
        node.fun.type, isEqualTo: funType,
        at: ConstraintLocator(node.fun)))
  }

  func visit(_ node: UnresolvedDeclRefExpr) {
  }

  func visit(_ node: UnresolvedMemberExpr) {
    checker.system.insert(
      ValueMemberConstraint(
        node.base.type, hasValueMember: node.memberName, ofType: node.type,
        at: ConstraintLocator(node, .valueMember(node.memberName))))
  }

  func visit(_ node: QualDeclRefExpr) {
  }

  func visit(_ node: OverloadedDeclRefExpr) {
    precondition(node.declSet.count >= 1)
    checker.system.insertDisjuncConf(
      disjunctionOfConstraintsWithWeights: node.declSet.map({ decl in
        let constraint = EqualityConstraint(
          node.type, isEqualTo: decl.type,
          at: ConstraintLocator(node))
        return (constraint, 0)
      }))
  }

  func visit(_ node: DeclRefExpr) {
  }

  func visit(_ node: TypeDeclRefExpr) {
  }

  func visit(_ node: MemberRefExpr) {
  }

  func visit(_ node: AddrOfExpr) {
  }

  func visit(_ node: WildcardExpr) {
  }

}
