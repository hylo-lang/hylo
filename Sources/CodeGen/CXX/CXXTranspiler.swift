import BigInt
import Core
import FrontEnd
import Utils

/// A Val to C++ transpiler.
/// This takes Val AST (with type information) and transforms it into C++ AST.
/// It handles one module at a time.
public struct CXXTranspiler {

  /// The Val typed nodes.
  /// Used in a few cases in which we need to obtain the typed nodes from the corresponding Node IDs,
  /// and to make general queries in the Val AST.
  let wholeValProgram: TypedProgram

  /// Creates a C++ transpiler from the whole Val program.
  public init(_ wholeValProgram: TypedProgram) {
    self.wholeValProgram = wholeValProgram
  }

  /// Transpiles Val Module into a C++ module object.
  public func transpile(_ source: ModuleDecl.Typed) -> CXXModule {
    var cxxModule = CXXModule(source)
    for member in source.topLevelDecls {
      let cxxTopLevelDecl = cxx(topLevel: member)
      cxxModule.addTopLevelDecl(cxxTopLevelDecl)
    }
    return cxxModule
  }

  // MARK: Declarations

  /// Transpiles to C++ a top-level declaration.
  func cxx(topLevel src: AnyDeclID.TypedNode) -> CXXTopLevelDecl {
    switch src.kind {
    case FunctionDecl.self:
      return cxx(function: FunctionDecl.Typed(src)!)
    case ProductTypeDecl.self:
      return cxx(type: ProductTypeDecl.Typed(src)!)
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Transpiles to C++ a top-level function declaration.
  func cxx(function src: FunctionDecl.Typed) -> CXXFunctionDecl {
    assert(wholeValProgram.isGlobal(src.id))

    let functionName = src.identifier?.value ?? ""

    return CXXFunctionDecl(
      identifier: CXXIdentifier(functionName),
      output: cxxFunctionReturnType(src, with: functionName),
      parameters: cxxFunctionParameters(src),
      body: src.body != nil ? cxx(funBody: src.body!) : nil)
  }

  /// Transpiles the function return type into a C++ type.
  private func cxxFunctionReturnType(_ src: FunctionDecl.Typed, with name: String) -> CXXTypeExpr {
    if name == "main" {
      // The output type of `main` must be `int`.
      return CXXTypeExpr("int")
    } else {
      switch src.type.base {
      case let valDeclType as LambdaType:
        return CXXTypeExpr(valDeclType.output, ast: wholeValProgram.ast, asReturnType: true)!

      case is MethodType:
        fatalError("not implemented")

      default:
        unreachable()
      }
    }
  }

  /// Transpiles the function parameters to C++.
  private func cxxFunctionParameters(_ src: FunctionDecl.Typed) -> [CXXFunctionDecl.Parameter] {
    let paramTypes = (src.type.base as! LambdaType).inputs
    assert(paramTypes.count == src.parameters.count)

    var cxxParams: [CXXFunctionDecl.Parameter] = []
    for (i, param) in src.parameters.enumerated() {
      let name = CXXIdentifier(param.name)
      let type = CXXTypeExpr(paramTypes[i].type, ast: wholeValProgram.ast)
      cxxParams.append(CXXFunctionDecl.Parameter(name, type!))
    }
    return cxxParams
  }

  /// Transpiles a function body to a C++.
  private func cxx(funBody body: FunctionDecl.Typed.Body) -> CXXScopedBlock {
    switch body {
    case .block(let stmt):
      return cxx(brace: stmt)

    case .expr(let expr):
      let exprBody = CXXReturnStmt(expr: cxxRValue(expr: expr))
      return CXXScopedBlock(stmts: [exprBody])
    }
  }

  /// Transpiles a product type declaration to a C++ class.
  private func cxx(type src: ProductTypeDecl.Typed) -> CXXClassDecl {
    assert(wholeValProgram.isGlobal(src.id))

    let name = CXXIdentifier(src.identifier.value)

    // Transpile the class membmers.
    var cxxMembers: [CXXClassDecl.ClassMember] = []
    for member in src.members {
      switch member.kind {
      case BindingDecl.self:
        let bindingDecl = BindingDecl.Typed(member)!
        // Check if the attribute is static or not.
        var isStatic = false
        if bindingDecl.memberModifier != nil {
          switch bindingDecl.memberModifier!.value {
          case .static:
            isStatic = true
          }
        }
        // TODO: visit initializer (bindingDecl.initializer)
        let cxxInitializer: CXXExpr? = nil
        // TODO: pattern introducer (let, var, sink, inout)
        // Visit the name patterns.
        for (_, name) in bindingDecl.pattern.subpattern.names {
          let varDecl = name.decl
          let cxxAttribute = CXXClassAttribute(
            type: CXXTypeExpr(varDecl.type, ast: wholeValProgram.ast)!,
            name: CXXIdentifier(varDecl.name),
            initializer: cxxInitializer,
            isStatic: isStatic)
          cxxMembers.append(.attribute(cxxAttribute))
        }

      case InitializerDecl.self:
        let initialzerDecl = InitializerDecl.Typed(member)!
        switch initialzerDecl.introducer.value {
        case .`init`:
          // TODO: emit constructor
          cxxMembers.append(.constructor)
          break
        case .memberwiseInit:
          // TODO: emit constructor
          cxxMembers.append(.constructor)
          break
        }

      case MethodDecl.self, FunctionDecl.self:
        cxxMembers.append(.method)

      default:
        unreachable("unexpected class member")
      }
    }

    // Create the C++ class.
    return CXXClassDecl(name: name, members: cxxMembers)
  }

  /// Transpiles a local binding declaration to a C++ statement containing a local variable.
  private func cxx(localBinding decl: BindingDecl.Typed) -> CXXStmt {
    let capability = decl.pattern.introducer.value

    // There's nothing to do if there's no initializer.
    if let initializer = decl.initializer {

      let isLValue =
        (initializer.kind == NameExpr.self) || (initializer.kind == SubscriptCallExpr.self)

      // Visit the initializer.
      let cxxInitialzer = cxx(expr: initializer, asLValue: isLValue)

      // Visit the patterns.
      var stmts: [CXXStmt] = []
      let pattern = decl.pattern
      for (path, name) in pattern.subpattern.names {
        // TODO: emit code for the patterns.
        let decl = name.decl
        let _ = path
        stmts.append(
          CXXLocalVarDecl(
            type: CXXTypeExpr(decl.type, ast: wholeValProgram.ast)!,
            name: CXXIdentifier(decl.identifier.value),
            initializer: cxxInitialzer)
        )
      }
      if stmts.isEmpty {
        // No pattern found; just call the initializer, dropping the result.
        let cxxExpr = CXXVoidCast(baseExpr: cxxInitialzer)
        return CXXExprStmt(expr: cxxExpr)
      } else if stmts.count == 1 {
        return stmts[0]
      } else {
        return CXXScopedBlock(stmts: stmts)
      }
    } else {
      return CXXComment(comment: "EMPTY borrowed local binding (\(capability))")
    }
  }

  // MARK: Statements

  /// Emits the given statement into `module` at the current insertion point.
  private func cxx<ID: StmtID>(stmt: ID.TypedNode) -> CXXStmt {
    switch stmt.kind {
    case BraceStmt.self:
      return cxx(brace: BraceStmt.Typed(stmt)!)
    case DeclStmt.self:
      return cxx(declStmt: DeclStmt.Typed(stmt)!)
    case ExprStmt.self:
      return cxx(exprStmt: ExprStmt.Typed(stmt)!)
    case AssignStmt.self:
      return cxx(assignStmt: AssignStmt.Typed(stmt)!)
    case ReturnStmt.self:
      return cxx(returnStmt: ReturnStmt.Typed(stmt)!)
    case WhileStmt.self:
      return cxx(whileStmt: WhileStmt.Typed(stmt)!)
    case DoWhileStmt.self:
      return cxx(doWhileStmt: DoWhileStmt.Typed(stmt)!)
    case ForStmt.self:
      return cxx(forStmt: ForStmt.Typed(stmt)!)
    case BreakStmt.self:
      return cxx(breakStmt: BreakStmt.Typed(stmt)!)
    case ContinueStmt.self:
      return cxx(continueStmt: ContinueStmt.Typed(stmt)!)
    case YieldStmt.self:
      return cxx(yieldStmt: YieldStmt.Typed(stmt)!)
    default:
      unreachable("unexpected statement")
    }
  }

  private func cxx(brace stmt: BraceStmt.Typed) -> CXXScopedBlock {
    var stmts: [CXXStmt] = []
    for s in stmt.stmts {
      stmts.append(cxx(stmt: s))
    }
    return CXXScopedBlock(stmts: stmts)
  }

  private func cxx(declStmt stmt: DeclStmt.Typed) -> CXXStmt {
    switch stmt.decl.kind {
    case BindingDecl.self:
      return cxx(localBinding: BindingDecl.Typed(stmt.decl)!)
    default:
      unreachable("unexpected declaration")
    }
  }

  private func cxx(exprStmt stmt: ExprStmt.Typed) -> CXXStmt {
    return CXXExprStmt(expr: cxxRValue(expr: stmt.expr))
  }

  private func cxx(assignStmt stmt: AssignStmt.Typed) -> CXXStmt {
    let cxxExpr = CXXInfixExpr(
      oper: .assignment,
      lhs: cxxLValue(expr: stmt.left, withCapability: .set),
      rhs: cxxRValue(expr: stmt.right))
    return CXXExprStmt(expr: cxxExpr)
  }

  private func cxx(returnStmt stmt: ReturnStmt.Typed) -> CXXStmt {
    var expr: CXXExpr?
    if stmt.value != nil {
      expr = cxxRValue(expr: stmt.value!)
    }
    return CXXReturnStmt(expr: expr)
  }

  private func cxx(whileStmt stmt: WhileStmt.Typed) -> CXXStmt {
    // TODO: multiple conditions
    // TODO: bindings in conditions
    let condition: CXXExpr
    if stmt.condition.count == 1 {
      switch stmt.condition[0] {
      case .expr(let condExpr):
        condition = cxxRValue(expr: wholeValProgram[condExpr])
      case .decl(let decl):
        let _ = decl
        condition = CXXComment(comment: "binding condition")
      }
    } else {
      fatalError("not implemented")
    }
    return CXXWhileStmt(
      condition: condition, body: cxx(stmt: stmt.body))
  }
  private func cxx(doWhileStmt stmt: DoWhileStmt.Typed) -> CXXStmt {
    return CXXDoWhileStmt(
      body: cxx(stmt: stmt.body),
      condition: cxxRValue(expr: stmt.condition))
  }
  private func cxx(forStmt stmt: ForStmt.Typed) -> CXXStmt {
    return CXXComment(comment: "ForStmt")
  }
  private func cxx(breakStmt stmt: BreakStmt.Typed) -> CXXStmt {
    return CXXBreakStmt()
  }
  private func cxx(continueStmt stmt: ContinueStmt.Typed) -> CXXStmt {
    return CXXContinueStmt()
  }
  private func cxx(yieldStmt stmt: YieldStmt.Typed) -> CXXStmt {
    return CXXComment(comment: "YieldStmt")
  }

  // MARK: Expressions

  private func cxx(
    expr: AnyExprID.TypedNode,
    asLValue: Bool
  ) -> CXXExpr {
    if asLValue {
      return cxxLValue(expr: expr, withCapability: .let)
    } else {
      return cxxRValue(expr: expr)
    }
  }

  // MARK: r-values

  private func cxxRValue<ID: ExprID>(expr: ID.TypedNode) -> CXXExpr {
    switch expr.kind {
    case BooleanLiteralExpr.self:
      return cxxRValue(booleanLiteral: BooleanLiteralExpr.Typed(expr)!)
    case CondExpr.self:
      return cxxRValue(cond: CondExpr.Typed(expr)!)
    case FunctionCallExpr.self:
      return cxxRValue(functionCall: FunctionCallExpr.Typed(expr)!)
    case IntegerLiteralExpr.self:
      return cxxRValue(integerLiteral: IntegerLiteralExpr.Typed(expr)!)
    case NameExpr.self:
      return cxxRValue(name: NameExpr.Typed(expr)!)
    case SequenceExpr.self:
      return cxxRValue(sequence: SequenceExpr.Typed(expr)!)
    default:
      unreachable("unexpected expression")
    }
  }

  private func cxxRValue(
    booleanLiteral expr: BooleanLiteralExpr.Typed
  ) -> CXXExpr {
    return CXXBooleanLiteralExpr(value: expr.value)
  }

  private func cxxRValue(
    cond expr: CondExpr.Typed
  ) -> CXXExpr {
    // TODO: multiple conditions
    // TODO: bindings in conditions
    let condition: CXXExpr
    if expr.condition.count == 1 {
      switch expr.condition[0] {
      case .expr(let condExpr):
        condition = cxxRValue(expr: wholeValProgram[condExpr])
      case .decl(let decl):
        let _ = decl
        condition = CXXComment(comment: "binding condition")
      }
    } else {
      fatalError("not implemented")
    }
    if expr.type != .void {
      // We result in an expression
      // TODO: do we need to return an l-value?
      let trueExpr: CXXExpr
      let falseExpr: CXXExpr
      switch expr.success {
      case .expr(let altExpr):
        trueExpr = cxxRValue(expr: wholeValProgram[altExpr])
      case .block:
        fatalError("not implemented")
      }
      switch expr.failure {
      case .expr(let altExpr):
        falseExpr = cxxRValue(expr: wholeValProgram[altExpr])
      case .block:
        fatalError("not implemented")
      case .none:
        falseExpr = CXXComment(
          comment: "missing false alternative")
      }
      return CXXConditionalExpr(
        condition: condition, trueExpr: trueExpr, falseExpr: falseExpr)
    } else {
      // We result in a statement
      let trueStmt: CXXStmt
      let falseStmt: CXXStmt?
      switch expr.success {
      case .expr(let altExpr):
        let expr = wholeValProgram[altExpr]
        trueStmt = CXXExprStmt(
          expr: CXXVoidCast(baseExpr: cxxRValue(expr: expr)))
      case .block(let braceStmt):
        trueStmt = cxx(stmt: wholeValProgram[braceStmt])
      }
      switch expr.failure {
      case .expr(let altExpr):
        let expr = wholeValProgram[altExpr]
        falseStmt = CXXExprStmt(
          expr: CXXVoidCast(baseExpr: cxxRValue(expr: expr)))
      case .block(let braceStmt):
        falseStmt = cxx(stmt: wholeValProgram[braceStmt])
      case .none:
        falseStmt = nil
      }
      // TODO: the result is put into an expression statement, which is not right
      return CXXStmtExpr(
        stmt: CXXIfStmt(
          condition: condition,
          trueStmt: trueStmt,
          falseStmt: falseStmt))
    }
  }

  private func cxxRValue(
    functionCall expr: FunctionCallExpr.Typed
  ) -> CXXExpr {
    let calleeType = expr.callee.type.base as! LambdaType

    // Arguments are evaluated first, from left to right.
    var argumentConventions: [AccessEffect] = []
    var arguments: [CXXExpr] = []

    for (parameter, argument) in zip(calleeType.inputs, expr.arguments) {
      let parameterType = parameter.type.base as! ParameterType
      argumentConventions.append(parameterType.convention)
      arguments.append(cxx(argument: wholeValProgram[argument.value], to: parameterType))
    }

    // If the callee is a name expression referring to the declaration of a function capture-less
    // function, it is interpreted as a direct function reference. Otherwise, it is evaluated as a
    // function object the arguments.
    let callee: CXXExpr

    if let calleeNameExpr = NameExpr.Typed(expr.callee) {
      callee = cxxRValue(name: calleeNameExpr, forCalleWithType: calleeType)

      // The name expresssion might fully represent an prefix/postfix operator call;
      // in this case, we don't need to wrap this into a function call
      if callee is CXXPrefixExpr || callee is CXXPostfixExpr {
        return callee
      }

    } else {
      // Evaluate the callee as a function object.
      callee = cxxRValue(expr: expr.callee)
    }

    return CXXFunctionCallExpr(
      callee: callee, arguments: arguments)
  }

  private func cxxRValue(
    integerLiteral expr: IntegerLiteralExpr.Typed
  ) -> CXXExpr {
    return CXXIntegerLiteralExpr(value: expr.value)
  }

  private func cxxRValue(
    name expr: NameExpr.Typed,
    forCalleWithType calleeType: LambdaType? = nil
  ) -> CXXExpr {
    switch expr.decl {
    case .direct(let calleeDecl) where calleeDecl.kind == BuiltinDecl.self:
      // Callee refers to a built-in function.
      assert(calleeType == nil || calleeType!.environment == .void)
      return CXXIdentifier(expr.name.value.stem)

    case .direct(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
      // Callee is a direct reference to a function or initializer declaration.

      // Check for prefix && postfix operator calls
      let functionDecl = FunctionDecl.Typed(calleeDecl)!
      if functionDecl.notation != nil && functionDecl.notation!.value == .prefix {
        let prefixOperators: [String: CXXPrefixExpr.Operator] = [
          "++": .prefixIncrement,
          "--": .prefixDecrement,
          "+": .unaryPlus,
          "-": .unaryMinus,
          "!": .logicalNot,
        ]
        if let cxxPrefixOperator = prefixOperators[nameOfDecl(calleeDecl)] {
          return CXXPrefixExpr(
            oper: cxxPrefixOperator, base: cxxRValue(expr: expr.domainExpr!))
        }
      } else if functionDecl.notation != nil && functionDecl.notation!.value == .postfix {
        let postfixOperators: [String: CXXPostfixExpr.Operator] = [
          "++": .suffixIncrement,
          "--": .suffixDecrement,
        ]
        if let cxxPostfixOperator = postfixOperators[nameOfDecl(calleeDecl)] {
          return CXXPostfixExpr(
            oper: cxxPostfixOperator, base: cxxRValue(expr: expr.domainExpr!))
        }
      }

      // TODO: handle captures
      return CXXIdentifier(nameOfDecl(calleeDecl))

    case .direct(let calleeDecl) where calleeDecl.kind == InitializerDecl.self:
      switch InitializerDecl.Typed(calleeDecl)!.introducer.value {
      case .`init`:
        // TODO: The function is a custom initializer.
        fatalError("not implemented")

      case .memberwiseInit:
        // The function is a memberwise initializer. In that case, the whole call expression is
        // lowered as a `record` instruction.
        // TODO: implement this
        fatalError("not implemented")
      }
    case .direct(let calleeDecl) where calleeDecl.kind == VarDecl.self:
      return CXXIdentifier(nameOfDecl(calleeDecl))

    case .direct(_):
      fatalError("not implemented")

    case .member(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
      // Callee is a member reference to a function or method.
      assert(calleeType != nil)
      let receiverType = calleeType!.captures[0].type

      var receiver: CXXExpr

      // Add the receiver to the arguments.
      if let type = RemoteType(receiverType) {
        // The receiver as a borrowing convention.
        switch expr.domain {
        case .none:
          receiver = CXXReceiverExpr()

        case .expr(let receiverID):
          receiver = cxxLValue(expr: receiverID, withCapability: type.capability)

        case .implicit:
          unreachable()
        }
      } else {
        // The receiver is consumed.
        switch expr.domain {
        case .none:
          receiver = CXXReceiverExpr()

        case .expr(let receiverID):
          receiver = cxxRValue(expr: receiverID)

        case .implicit:
          unreachable()
        }
      }

      // Emit the function reference.
      return CXXInfixExpr(
        oper: .dotAccess,
        lhs: receiver, rhs: CXXIdentifier(nameOfDecl(calleeDecl)))

    case .member(_):
      fatalError("not implemented")
    }
  }

  private func cxxRValue(
    sequence expr: SequenceExpr.Typed
  ) -> CXXExpr {
    return cxx(foldedSequence: expr.foldedSequenceExprs!, withCapability: .sink, for: expr)
  }

  private func cxx(
    foldedSequence seq: FoldedSequenceExpr,
    withCapability capability: AccessEffect,
    for expr: SequenceExpr.Typed
  ) -> CXXExpr {
    switch seq {
    case .infix(let callee, let lhs, let rhs):
      let calleeExpr = wholeValProgram[callee.expr]
      let calleeType = calleeExpr.type.base as! LambdaType

      // Emit the operands, starting with RHS.
      let rhsType = calleeType.inputs[0].type.base as! ParameterType
      let rhsOperand = cxx(foldedSequence: rhs, withCapability: rhsType.convention, for: expr)

      let lhsConvention: AccessEffect
      if let lhsType = RemoteType(calleeType.captures[0].type) {
        lhsConvention = lhsType.capability
      } else {
        lhsConvention = .sink
      }
      let lhsOperand = cxx(foldedSequence: lhs, withCapability: lhsConvention, for: expr)

      // Build the resulting infix expression / function call.
      return cxxRValue(infix: calleeExpr, lhs: lhsOperand, rhs: rhsOperand)

    case .leaf(let expr):
      switch capability {
      case .let:
        return cxxLValue(expr: wholeValProgram[expr], withCapability: .let)
      case .inout:
        return cxxLValue(expr: wholeValProgram[expr], withCapability: .inout)
      case .set:
        return cxxLValue(expr: wholeValProgram[expr], withCapability: .set)
      case .sink:
        return cxxRValue(expr: wholeValProgram[expr])
      case .yielded:
        fatalError("not implemented")
      }
    }
  }

  private func cxxRValue(
    infix expr: NameExpr.Typed,
    lhs: CXXExpr,
    rhs: CXXExpr
  ) -> CXXExpr {

    // Obtained the declaration we are calling.
    let calleeDecl: AnyDeclID.TypedNode
    switch expr.decl {
    case .direct(let decl):
      calleeDecl = decl
    case .member(let decl):
      calleeDecl = decl
    }

    // Emit infix operators.
    let name = nameOfDecl(calleeDecl)
    switch name {
    case "<<": return CXXInfixExpr(oper: .leftShift, lhs: lhs, rhs: rhs)
    case ">>": return CXXInfixExpr(oper: .rightShift, lhs: lhs, rhs: rhs)
    case "*": return CXXInfixExpr(oper: .multiplication, lhs: lhs, rhs: rhs)
    case "/": return CXXInfixExpr(oper: .division, lhs: lhs, rhs: rhs)
    case "%": return CXXInfixExpr(oper: .remainder, lhs: lhs, rhs: rhs)
    case "+": return CXXInfixExpr(oper: .addition, lhs: lhs, rhs: rhs)
    case "-": return CXXInfixExpr(oper: .subtraction, lhs: lhs, rhs: rhs)
    case "==": return CXXInfixExpr(oper: .equality, lhs: lhs, rhs: rhs)
    case "!=": return CXXInfixExpr(oper: .inequality, lhs: lhs, rhs: rhs)
    case "<": return CXXInfixExpr(oper: .lessThan, lhs: lhs, rhs: rhs)
    case "<=": return CXXInfixExpr(oper: .lessEqual, lhs: lhs, rhs: rhs)
    case ">=": return CXXInfixExpr(oper: .greaterEqual, lhs: lhs, rhs: rhs)
    case ">": return CXXInfixExpr(oper: .greaterThan, lhs: lhs, rhs: rhs)
    case "^": return CXXInfixExpr(oper: .bitwiseXor, lhs: lhs, rhs: rhs)
    case "&": return CXXInfixExpr(oper: .bitwiseAnd, lhs: lhs, rhs: rhs)
    case "&&": return CXXInfixExpr(oper: .logicalAnd, lhs: lhs, rhs: rhs)
    case "|": return CXXInfixExpr(oper: .bitwiseOr, lhs: lhs, rhs: rhs)
    case "||": return CXXInfixExpr(oper: .logicalOr, lhs: lhs, rhs: rhs)
    case "<<=": return CXXInfixExpr(oper: .shiftLeftAssignment, lhs: lhs, rhs: rhs)
    case ">>=": return CXXInfixExpr(oper: .shiftRightAssignment, lhs: lhs, rhs: rhs)
    case "*=": return CXXInfixExpr(oper: .mulAssignment, lhs: lhs, rhs: rhs)
    case "/=": return CXXInfixExpr(oper: .divAssignment, lhs: lhs, rhs: rhs)
    case "%=": return CXXInfixExpr(oper: .remAssignment, lhs: lhs, rhs: rhs)
    case "+=": return CXXInfixExpr(oper: .addAssignment, lhs: lhs, rhs: rhs)
    case "-=": return CXXInfixExpr(oper: .divAssignment, lhs: lhs, rhs: rhs)
    case "&=": return CXXInfixExpr(oper: .bitwiseAndAssignment, lhs: lhs, rhs: rhs)

    default:
      // Expand this as a regular function call.
      let callee = cxxLValue(name: expr, withCapability: .let)
      let arguments = [lhs, rhs]
      return CXXFunctionCallExpr(callee: callee, arguments: arguments)
    }
  }

  private func cxx(
    argument expr: AnyExprID.TypedNode,
    to parameterType: ParameterType
  ) -> CXXExpr {
    switch parameterType.convention {
    case .let:
      return cxxLValue(expr: expr, withCapability: .let)
    case .inout:
      return cxxLValue(expr: expr, withCapability: .inout)
    case .set:
      return cxxLValue(expr: expr, withCapability: .set)
    case .sink:
      return cxxRValue(expr: expr)
    case .yielded:
      fatalError("not implemented")
    }
  }

  // MARK: l-values

  private func cxxLValue<ID: ExprID>(
    expr: ID.TypedNode,
    withCapability capability: AccessEffect
  ) -> CXXExpr {
    switch expr.kind {
    case NameExpr.self:
      return cxxLValue(name: NameExpr.Typed(expr)!, withCapability: capability)

    case SubscriptCallExpr.self:
      fatalError("not implemented")

    default:
      // TODO: do we need to add extra instructions to covert this to l-value?
      return cxxRValue(expr: expr)
    }
  }

  private func cxxLValue(
    name expr: NameExpr.Typed,
    withCapability capability: AccessEffect
  ) -> CXXExpr {
    switch expr.decl {
    case .direct(let decl):
      return CXXIdentifier(nameOfDecl(decl))

    case .member(let decl):
      // Emit the receiver.
      let receiver: CXXExpr?
      switch expr.domain {
      case .none:
        // TODO: this doesn't seem right; check the following code
        // receiver = CXXReceiverExpr()
        receiver = nil
      case .implicit:
        fatalError("not implemented")
      case .expr(let recv):
        receiver = cxxLValue(expr: recv, withCapability: capability)
      }

      // Emit the compound expression.
      let idExpr = CXXIdentifier(nameOfDecl(decl))
      if receiver != nil {
        return CXXInfixExpr(oper: .dotAccess, lhs: receiver!, rhs: idExpr)
      } else {
        return idExpr
      }
    }
  }

  // MARK: miscelaneous

  /// Get the name of the given declaration, without manging and labels.
  private func nameOfDecl<T: DeclID>(_ decl: TypedNode<T>) -> String {
    switch decl.kind {
    case ConformanceDecl.self, ExtensionDecl.self:
      fatalError("not implemented")

    case FunctionDecl.self:
      return FunctionDecl.Typed(decl)!.identifier!.value

    case InitializerDecl.self:
      return "init"

    case MethodDecl.self:
      return MethodDecl.Typed(decl)!.identifier.value

    case MethodImplDecl.self:
      let methodImplDecl = MethodImplDecl.Typed(decl)!
      switch methodImplDecl.introducer.value {
      case .let: return "let"
      case .inout: return "inout"
      case .set: return "set"
      case .sink: return "sink"
      }

    case ProductTypeDecl.self:
      return ProductTypeDecl.Typed(decl)!.name

    case ParameterDecl.self:
      return ParameterDecl.Typed(decl)!.identifier.value
    case VarDecl.self:
      return VarDecl.Typed(decl)!.identifier.value

    default:
      fatalError("not implemented")
    }

  }

}
