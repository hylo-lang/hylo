import AST

/// An AST visitor that generates type constraints for a statement or an expression.
struct ConstraintGenerator: StmtVisitor, ExprVisitor {

  typealias StmtResult = Void
  typealias ExprResult = Void

  /// The type checker for which constraints are being generated.
  unowned let checker: TypeChecker

  /// The declaration space from which nodes are being visited.
  unowned let useSite: DeclSpace

  func visit(_ node: PatternBindingDecl) {
    // If the declaration has a signature, it as the authoritative type information.
    if let sign = node.sign {
      var signType = sign.realize(unqualifiedFrom: node.parentDeclSpace!)

      if signType.props.contains(.hasTypeParams) {
        let gds = useSite.innermostGenericSpace!
        gds.prepareGenericEnv()
        signType = gds.genericEnv.instantiate(signType, from: useSite)
      }

      checker.system.insert(
        RelationalConstraint(
          kind: .equality, lhs: node.pattern.type, rhs: signType,
          at: ConstraintLocator(node, .annotation)))

      // Since the actual type of the pattern if described by the signature, we can treat the
      // initializer as a mere assignment and only impose a subtyping constraint
      if let initializer = node.initializer {
        checker.system.insert(
          RelationalConstraint(
            kind: .subtyping, lhs: initializer.type, rhs: node.pattern.type,
            at: ConstraintLocator(node, .assignment)))
      }
    } else if let initializer = node.initializer {
      // Since we could not infer the type of the pattern from a signature, we must infer it
      // directly from the initializer. We use an equality constraint to type the pattern as
      // closely as tightly as possible to the initializer.
      checker.system.insert(
        RelationalConstraint(
          kind: .equality, lhs: node.pattern.type, rhs: initializer.type,
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
      let signType = sign.realize(unqualifiedFrom: funDecl)

      if signType.props.contains(.hasTypeParams) {
        funDecl.prepareGenericEnv()
        retType = funDecl.genericEnv.instantiate(signType, from: useSite)
      } else {
        retType = signType
      }

      guard !(retType is ErrorType) else { return }
    } else {
      retType = checker.context.unitType
    }

    let valType = node.value?.type ?? checker.context.unitType
    checker.system.insert(
      RelationalConstraint(
        kind: .returnBinding, lhs: valType, rhs: retType,
        at: ConstraintLocator(node, .returnValue)))
  }

  func visit(_ node: IntLiteralExpr) {
    let literalType = checker.context.getBuiltinType(named: "IntLiteral")!
    checker.system.insert(
      RelationalConstraint(
        kind: .conversion, lhs: node.type, rhs: literalType,
        at: ConstraintLocator(node)))
  }

  func visit(_ node: AssignExpr) {
    checker.system.insert(
      RelationalConstraint(
        kind: .subtyping, lhs: node.rvalue.type, rhs: node.lvalue.type,
        at: ConstraintLocator(node, .assignment)))
  }

  func visit(_ node: CallExpr) {
    // Synthetize the type of a function from the call's arguments.
    var paramTypeElems: [TupleType.Elem] = []
    for arg in node.args {
      // The subtyping constraint handle cases where the argument is a subtype of the parameter.
      let paramType = TypeVar(context: checker.context, node: arg.value)
      checker.system.insert(
        RelationalConstraint(
          kind: .subtyping, lhs: arg.value.type, rhs: paramType,
          at: ConstraintLocator(node, .application)))
      paramTypeElems.append(TupleType.Elem(label: arg.label, type: paramType))
    }

    let paramType = checker.context.tupleType(paramTypeElems)
    let funType = checker.context.funType(paramType: paramType, retType: node.type)
    checker.system.insert(
      RelationalConstraint(
        kind: .equality, lhs: node.fun.type, rhs: funType,
        at: ConstraintLocator(node.fun)))
  }

  func visit(_ node: UnresolvedDeclRefExpr) {
  }

  func visit(_ node: UnresolvedMemberExpr) {
    checker.system.insert(
      ValueMemberConstraint(
        node.base.type, hasValueMember: node.memberName, ofType: node.type, useSite: useSite,
        at: ConstraintLocator(node, .valueMember(node.memberName))))
  }

  func visit(_ node: UnresolvedQualDeclRefExpr) {
  }

  func visit(_ node: OverloadedDeclRefExpr) {
    assert(node.declSet.count >= 1)

    checker.system.insert(
      OverloadBindingConstraint(
        node.type, declSet: node.declSet, useSite: useSite,
        at: ConstraintLocator(node)))
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

  func visit(_ node: ErrorExpr) {
  }

}
