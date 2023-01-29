import BigInt
import Core
import FrontEnd
import Utils

/// A Val to C++ transpiler.
public struct CXXTranspiler {

  /// The program being transpiled.
  let program: TypedProgram

  /// Creates a C++ transpiler with a well-typed AST.
  public init(program: TypedProgram) {
    self.program = program
  }

  // MARK: API

  /// Emits the C++ module corresponding to the Val module identified by `decl`.
  public mutating func emit(module decl: ModuleDecl.Typed) -> CXXModule {
    var module = CXXModule(decl, for: program)
    for member in decl.topLevelDecls {
      emit(topLevel: member, into: &module)
    }
    return module
  }

  /// Emits the given top-level declaration into `module`.
  mutating func emit(topLevel decl: AnyDeclID.TypedNode, into module: inout CXXModule) {
    switch decl.kind {
    case FunctionDecl.self:
      emit(function: FunctionDecl.Typed(decl)!, into: &module)
    case ProductTypeDecl.self:
      emit(type: ProductTypeDecl.Typed(decl)!, into: &module)
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Emits the given function declaration into `module`.
  mutating func emit(function decl: FunctionDecl.Typed, into module: inout CXXModule) {
    assert(program.isGlobal(decl.id))

    /// The identifier of the function.
    let identifier = CXXIdentifier(decl.identifier?.value ?? "")

    // Determine the output type of the function.
    let output: CXXTypeExpr
    if identifier.description == "main" {
      // The output type of `main` must be `int`.
      output = CXXTypeExpr("int")
    } else {
      switch decl.type.base {
      case let valDeclType as LambdaType:
        output = CXXTypeExpr(valDeclType.output, ast: program.ast, asReturnType: true)!

      case is MethodType:
        fatalError("not implemented")

      default:
        unreachable()
      }
    }

    // Determine the parameter types of the function.
    let paramTypes: [CallableTypeParameter]
    switch decl.type.base {
    case let valDeclType as LambdaType:
      paramTypes = valDeclType.inputs

    case is MethodType:
      fatalError("not implemented")

    default:
      unreachable()
    }

    // Determine the parameters of the function.
    assert(paramTypes.count == decl.parameters.count)
    var cxxParams: [CXXFunctionDecl.Parameter] = []
    for (i, param) in decl.parameters.enumerated() {
      let name = CXXIdentifier(param.name)
      let type = CXXTypeExpr(paramTypes[i].type, ast: program.ast)
      cxxParams.append(CXXFunctionDecl.Parameter(name, type!))
    }

    // The body of the function.
    var cxxBody: CXXStmt? = nil
    if let body = decl.body {
      cxxBody = emit(funBody: body)
    }

    // Create the C++ function object.
    module.addTopLevelDecl(
      CXXFunctionDecl(
        identifier: identifier,
        output: output,
        parameters: cxxParams,
        body: cxxBody,
        original: decl))
  }

  /// Translate the function body into a CXX entity.
  private mutating func emit(funBody body: FunctionDecl.Typed.Body) -> CXXStmt {
    switch body {
    case .block(let stmt):
      return emit(brace: stmt)

    case .expr(let expr):
      let exprBody = CXXReturnStmt(expr: emitR(expr: expr), original: AnyNodeID.TypedNode(expr))
      return CXXScopedBlock(stmts: [exprBody], original: AnyNodeID.TypedNode(expr))
    }
  }

  // MARK: Declarations

  /// Emits the given function declaration into `module`.
  private mutating func emit(type decl: ProductTypeDecl.Typed, into module: inout CXXModule) {
    assert(program.isGlobal(decl.id))

    let name = CXXIdentifier(decl.identifier.value)

    // Transpile the class membmers.
    var cxxMembers: [CXXClassDecl.ClassMember] = []
    for member in decl.members {
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
            type: CXXTypeExpr(varDecl.type, ast: program.ast)!,
            name: CXXIdentifier(varDecl.name),
            initializer: cxxInitializer,
            isStatic: isStatic,
            original: varDecl)
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
    module.addTopLevelDecl(CXXClassDecl(name: name, members: cxxMembers, original: decl))
  }

  private mutating func emit(localBinding decl: BindingDecl.Typed) -> CXXStmt {
    let capability = decl.pattern.introducer.value

    // There's nothing to do if there's no initializer.
    if let initializer = decl.initializer {

      let isLValue =
        (initializer.kind == NameExpr.self) || (initializer.kind == SubscriptCallExpr.self)

      // Visit the initializer.
      let cxxInitialzer = emit(expr: initializer, asLValue: isLValue)

      // Visit the patterns.
      var stmts: [CXXStmt] = []
      let pattern = decl.pattern
      for (path, name) in pattern.subpattern.names {
        // TODO: emit code for the patterns.
        let decl = name.decl
        let _ = path
        stmts.append(
          CXXLocalVarDecl(
            type: CXXTypeExpr(decl.type, ast: program.ast)!,
            name: CXXIdentifier(decl.identifier.value),
            initializer: cxxInitialzer,
            original: decl)
        )
      }
      if stmts.isEmpty {
        // No pattern found; just call the initializer, dropping the result.
        let cxxExpr = CXXVoidCast(baseExpr: cxxInitialzer, original: initializer)
        return CXXExprStmt(expr: cxxExpr, original: AnyNodeID.TypedNode(initializer))
      } else if stmts.count == 1 {
        return stmts[0]
      } else {
        return CXXScopedBlock(stmts: stmts, original: AnyNodeID.TypedNode(initializer))
      }
    } else {
      return CXXComment(
        comment: "EMPTY borrowed local binding (\(capability))", original: AnyNodeID.TypedNode(decl)
      )
    }
  }

  // MARK: Statements

  /// Emits the given statement into `module` at the current insertion point.
  private mutating func emit<ID: StmtID>(stmt: ID.TypedNode) -> CXXStmt {
    switch stmt.kind {
    case BraceStmt.self:
      return emit(brace: BraceStmt.Typed(stmt)!)
    case DeclStmt.self:
      return emit(declStmt: DeclStmt.Typed(stmt)!)
    case ExprStmt.self:
      return emit(exprStmt: ExprStmt.Typed(stmt)!)
    case AssignStmt.self:
      return emit(assignStmt: AssignStmt.Typed(stmt)!)
    case ReturnStmt.self:
      return emit(returnStmt: ReturnStmt.Typed(stmt)!)
    case WhileStmt.self:
      return emit(whileStmt: WhileStmt.Typed(stmt)!)
    case DoWhileStmt.self:
      return emit(doWhileStmt: DoWhileStmt.Typed(stmt)!)
    case ForStmt.self:
      return emit(forStmt: ForStmt.Typed(stmt)!)
    case BreakStmt.self:
      return emit(breakStmt: BreakStmt.Typed(stmt)!)
    case ContinueStmt.self:
      return emit(continueStmt: ContinueStmt.Typed(stmt)!)
    case YieldStmt.self:
      return emit(yieldStmt: YieldStmt.Typed(stmt)!)
    default:
      unreachable("unexpected statement")
    }
  }

  private mutating func emit(brace stmt: BraceStmt.Typed) -> CXXStmt {
    var stmts: [CXXStmt] = []
    for s in stmt.stmts {
      stmts.append(emit(stmt: s))
    }
    return CXXScopedBlock(stmts: stmts, original: AnyNodeID.TypedNode(stmt))
  }

  private mutating func emit(declStmt stmt: DeclStmt.Typed) -> CXXStmt {
    switch stmt.decl.kind {
    case BindingDecl.self:
      return emit(localBinding: BindingDecl.Typed(stmt.decl)!)
    default:
      unreachable("unexpected declaration")
    }
  }

  private mutating func emit(exprStmt stmt: ExprStmt.Typed) -> CXXStmt {
    return CXXExprStmt(expr: emitR(expr: stmt.expr), original: AnyNodeID.TypedNode(stmt))
  }

  private mutating func emit(assignStmt stmt: AssignStmt.Typed) -> CXXStmt {
    let cxxExpr = CXXInfixExpr(
      oper: .assignment,
      lhs: emitL(expr: stmt.left, withCapability: .set),
      rhs: emitR(expr: stmt.right),
      original: AnyNodeID.TypedNode(stmt))
    return CXXExprStmt(expr: cxxExpr, original: AnyNodeID.TypedNode(stmt))
  }

  private mutating func emit(returnStmt stmt: ReturnStmt.Typed) -> CXXStmt {
    var expr: CXXExpr?
    if stmt.value != nil {
      expr = emitR(expr: stmt.value!)
    }
    return CXXReturnStmt(expr: expr, original: AnyNodeID.TypedNode(stmt))
  }

  private mutating func emit(whileStmt stmt: WhileStmt.Typed) -> CXXStmt {
    // TODO: multiple conditions
    // TODO: bindings in conditions
    let condition: CXXExpr
    if stmt.condition.count == 1 {
      switch stmt.condition[0] {
      case .expr(let condExpr):
        condition = emitR(expr: program[condExpr])
      case .decl(let decl):
        condition = CXXComment(
          comment: "binding condition", original: AnyNodeID.TypedNode(program[decl]))
      }
    } else {
      fatalError("not implemented")
    }
    return CXXWhileStmt(
      condition: condition, body: emit(stmt: stmt.body), original: AnyNodeID.TypedNode(stmt))
  }
  private mutating func emit(doWhileStmt stmt: DoWhileStmt.Typed) -> CXXStmt {
    return CXXDoWhileStmt(
      body: emit(stmt: stmt.body),
      condition: emitR(expr: stmt.condition),
      original: AnyNodeID.TypedNode(stmt))
  }
  private mutating func emit(forStmt stmt: ForStmt.Typed) -> CXXStmt {
    return CXXComment(comment: "ForStmt", original: AnyNodeID.TypedNode(stmt))
  }
  private mutating func emit(breakStmt stmt: BreakStmt.Typed) -> CXXStmt {
    return CXXBreakStmt(original: AnyNodeID.TypedNode(stmt))
  }
  private mutating func emit(continueStmt stmt: ContinueStmt.Typed) -> CXXStmt {
    return CXXContinueStmt(original: AnyNodeID.TypedNode(stmt))
  }
  private mutating func emit(yieldStmt stmt: YieldStmt.Typed) -> CXXStmt {
    return CXXComment(comment: "YieldStmt", original: AnyNodeID.TypedNode(stmt))
  }

  // MARK: Expressions

  private mutating func emit(
    expr: AnyExprID.TypedNode,
    asLValue: Bool
  ) -> CXXExpr {
    if asLValue {
      return emitL(expr: expr, withCapability: .let)
    } else {
      return emitR(expr: expr)
    }
  }

  // MARK: r-values

  private mutating func emitR<ID: ExprID>(expr: ID.TypedNode) -> CXXExpr {
    switch expr.kind {
    case BooleanLiteralExpr.self:
      return emitR(booleanLiteral: BooleanLiteralExpr.Typed(expr)!)
    case CondExpr.self:
      return emitR(cond: CondExpr.Typed(expr)!)
    case FunctionCallExpr.self:
      return emitR(functionCall: FunctionCallExpr.Typed(expr)!)
    case IntegerLiteralExpr.self:
      return emitR(integerLiteral: IntegerLiteralExpr.Typed(expr)!)
    case NameExpr.self:
      return emitR(name: NameExpr.Typed(expr)!)
    case SequenceExpr.self:
      return emitR(sequence: SequenceExpr.Typed(expr)!)
    default:
      unreachable("unexpected expression")
    }
  }

  private mutating func emitR(
    booleanLiteral expr: BooleanLiteralExpr.Typed
  ) -> CXXExpr {
    return CXXBooleanLiteralExpr(value: expr.value, original: expr)
  }

  private mutating func emitR(
    cond expr: CondExpr.Typed
  ) -> CXXExpr {
    // TODO: multiple conditions
    // TODO: bindings in conditions
    let condition: CXXExpr
    if expr.condition.count == 1 {
      switch expr.condition[0] {
      case .expr(let condExpr):
        condition = emitR(expr: program[condExpr])
      case .decl(let decl):
        condition = CXXComment(
          comment: "binding condition", original: AnyNodeID.TypedNode(program[decl]))
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
        trueExpr = emitR(expr: program[altExpr])
      case .block:
        fatalError("not implemented")
      }
      switch expr.failure {
      case .expr(let altExpr):
        falseExpr = emitR(expr: program[altExpr])
      case .block:
        fatalError("not implemented")
      case .none:
        falseExpr = CXXComment(
          comment: "missing false alternative", original: AnyNodeID.TypedNode(expr))
      }
      return CXXConditionalExpr(
        condition: condition, trueExpr: trueExpr, falseExpr: falseExpr, original: expr)
    } else {
      // We result in a statement
      let trueStmt: CXXStmt
      let falseStmt: CXXStmt?
      switch expr.success {
      case .expr(let altExpr):
        let expr = program[altExpr]
        trueStmt = CXXExprStmt(
          expr: CXXVoidCast(baseExpr: emitR(expr: expr), original: AnyExprID.TypedNode(expr)),
          original: AnyNodeID.TypedNode(expr))
      case .block(let braceStmt):
        trueStmt = emit(stmt: program[braceStmt])
      }
      switch expr.failure {
      case .expr(let altExpr):
        let expr = program[altExpr]
        falseStmt = CXXExprStmt(
          expr: CXXVoidCast(baseExpr: emitR(expr: expr), original: AnyExprID.TypedNode(expr)),
          original: AnyNodeID.TypedNode(expr))
      case .block(let braceStmt):
        falseStmt = emit(stmt: program[braceStmt])
      case .none:
        falseStmt = nil
      }
      // TODO: the result is put into an expression statement, which is not right
      return CXXStmtExpr(
        stmt: CXXIfStmt(
          condition: condition,
          trueStmt: trueStmt,
          falseStmt: falseStmt,
          original: AnyNodeID.TypedNode(expr)),
        original: AnyNodeID.TypedNode(expr))
    }
  }

  private mutating func emitR(
    functionCall expr: FunctionCallExpr.Typed
  ) -> CXXExpr {
    let calleeType = expr.callee.type.base as! LambdaType

    // Arguments are evaluated first, from left to right.
    var argumentConventions: [AccessEffect] = []
    var arguments: [CXXExpr] = []

    for (parameter, argument) in zip(calleeType.inputs, expr.arguments) {
      let parameterType = parameter.type.base as! ParameterType
      argumentConventions.append(parameterType.convention)
      arguments.append(emit(argument: program[argument.value], to: parameterType))
    }

    // If the callee is a name expression referring to the declaration of a function capture-less
    // function, it is interpreted as a direct function reference. Otherwise, it is evaluated as a
    // function object the arguments.
    let callee: CXXExpr

    if let calleeNameExpr = NameExpr.Typed(expr.callee) {
      callee = emitR(name: calleeNameExpr, forCalleWithType: calleeType)

      // The name expresssion might fully represent an prefix/postfix operator call;
      // in this case, we don't need to wrap this into a function call
      if callee is CXXPrefixExpr || callee is CXXPostfixExpr {
        return callee
      }

    } else {
      // Evaluate the callee as a function object.
      callee = emitR(expr: expr.callee)
    }

    return CXXFunctionCallExpr(
      callee: callee, arguments: arguments, original: AnyExprID.TypedNode(expr))
  }

  private mutating func emitR(
    integerLiteral expr: IntegerLiteralExpr.Typed
  ) -> CXXExpr {
    return CXXIntegerLiteralExpr(value: expr.value, original: expr)
  }

  private mutating func emitR(
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
            oper: cxxPrefixOperator, base: emitR(expr: expr.domainExpr!), original: nil)
        }
      } else if functionDecl.notation != nil && functionDecl.notation!.value == .postfix {
        let postfixOperators: [String: CXXPostfixExpr.Operator] = [
          "++": .suffixIncrement,
          "--": .suffixDecrement,
        ]
        if let cxxPostfixOperator = postfixOperators[nameOfDecl(calleeDecl)] {
          return CXXPostfixExpr(
            oper: cxxPostfixOperator, base: emitR(expr: expr.domainExpr!), original: nil)
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
          receiver = CXXReceiverExpr(original: AnyExprID.TypedNode(expr))

        case .expr(let receiverID):
          receiver = emitL(expr: receiverID, withCapability: type.capability)

        case .implicit:
          unreachable()
        }
      } else {
        // The receiver is consumed.
        switch expr.domain {
        case .none:
          receiver = CXXReceiverExpr(original: AnyExprID.TypedNode(expr))

        case .expr(let receiverID):
          receiver = emitR(expr: receiverID)

        case .implicit:
          unreachable()
        }
      }

      // Emit the function reference.
      return CXXInfixExpr(
        oper: .dotAccess,
        lhs: receiver, rhs: CXXIdentifier(nameOfDecl(calleeDecl)),
        original: AnyNodeID.TypedNode(expr))

    case .member(_):
      fatalError("not implemented")
    }
  }

  private mutating func emitR(
    sequence expr: SequenceExpr.Typed
  ) -> CXXExpr {
    return emit(foldedSequence: expr.foldedSequenceExprs!, withCapability: .sink, for: expr)
  }

  private mutating func emit(
    foldedSequence seq: FoldedSequenceExpr,
    withCapability capability: AccessEffect,
    for expr: SequenceExpr.Typed
  ) -> CXXExpr {
    switch seq {
    case .infix(let callee, let lhs, let rhs):
      let calleeExpr = program[callee.expr]
      let calleeType = calleeExpr.type.base as! LambdaType

      // Emit the operands, starting with RHS.
      let rhsType = calleeType.inputs[0].type.base as! ParameterType
      let rhsOperand = emit(foldedSequence: rhs, withCapability: rhsType.convention, for: expr)

      let lhsConvention: AccessEffect
      if let lhsType = RemoteType(calleeType.captures[0].type) {
        lhsConvention = lhsType.capability
      } else {
        lhsConvention = .sink
      }
      let lhsOperand = emit(foldedSequence: lhs, withCapability: lhsConvention, for: expr)

      // Build the resulting infix expression / function call.
      return emitR(infix: calleeExpr, lhs: lhsOperand, rhs: rhsOperand)

    case .leaf(let expr):
      switch capability {
      case .let:
        return emitL(expr: program[expr], withCapability: .let)
      case .inout:
        return emitL(expr: program[expr], withCapability: .inout)
      case .set:
        return emitL(expr: program[expr], withCapability: .set)
      case .sink:
        return emitR(expr: program[expr])
      case .yielded:
        fatalError("not implemented")
      }
    }
  }

  private mutating func emitR(
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
    let orig = AnyNodeID.TypedNode(expr)
    let name = nameOfDecl(calleeDecl)
    switch name {
    case "<<": return CXXInfixExpr(oper: .leftShift, lhs: lhs, rhs: rhs, original: orig)
    case ">>": return CXXInfixExpr(oper: .rightShift, lhs: lhs, rhs: rhs, original: orig)
    case "*": return CXXInfixExpr(oper: .multiplication, lhs: lhs, rhs: rhs, original: orig)
    case "/": return CXXInfixExpr(oper: .division, lhs: lhs, rhs: rhs, original: orig)
    case "%": return CXXInfixExpr(oper: .remainder, lhs: lhs, rhs: rhs, original: orig)
    case "+": return CXXInfixExpr(oper: .addition, lhs: lhs, rhs: rhs, original: orig)
    case "-": return CXXInfixExpr(oper: .subtraction, lhs: lhs, rhs: rhs, original: orig)
    case "==": return CXXInfixExpr(oper: .equality, lhs: lhs, rhs: rhs, original: orig)
    case "!=": return CXXInfixExpr(oper: .inequality, lhs: lhs, rhs: rhs, original: orig)
    case "<": return CXXInfixExpr(oper: .lessThan, lhs: lhs, rhs: rhs, original: orig)
    case "<=": return CXXInfixExpr(oper: .lessEqual, lhs: lhs, rhs: rhs, original: orig)
    case ">=": return CXXInfixExpr(oper: .greaterEqual, lhs: lhs, rhs: rhs, original: orig)
    case ">": return CXXInfixExpr(oper: .greaterThan, lhs: lhs, rhs: rhs, original: orig)
    case "^": return CXXInfixExpr(oper: .bitwiseXor, lhs: lhs, rhs: rhs, original: orig)
    case "&": return CXXInfixExpr(oper: .bitwiseAnd, lhs: lhs, rhs: rhs, original: orig)
    case "&&": return CXXInfixExpr(oper: .logicalAnd, lhs: lhs, rhs: rhs, original: orig)
    case "|": return CXXInfixExpr(oper: .bitwiseOr, lhs: lhs, rhs: rhs, original: orig)
    case "||": return CXXInfixExpr(oper: .logicalOr, lhs: lhs, rhs: rhs, original: orig)
    case "<<=": return CXXInfixExpr(oper: .shiftLeftAssignment, lhs: lhs, rhs: rhs, original: orig)
    case ">>=": return CXXInfixExpr(oper: .shiftRightAssignment, lhs: lhs, rhs: rhs, original: orig)
    case "*=": return CXXInfixExpr(oper: .mulAssignment, lhs: lhs, rhs: rhs, original: orig)
    case "/=": return CXXInfixExpr(oper: .divAssignment, lhs: lhs, rhs: rhs, original: orig)
    case "%=": return CXXInfixExpr(oper: .remAssignment, lhs: lhs, rhs: rhs, original: orig)
    case "+=": return CXXInfixExpr(oper: .addAssignment, lhs: lhs, rhs: rhs, original: orig)
    case "-=": return CXXInfixExpr(oper: .divAssignment, lhs: lhs, rhs: rhs, original: orig)
    case "&=": return CXXInfixExpr(oper: .bitwiseAndAssignment, lhs: lhs, rhs: rhs, original: orig)

    default:
      // Expand this as a regular function call.
      let callee = emitL(name: expr, withCapability: .let)
      let arguments = [lhs, rhs]
      return CXXFunctionCallExpr(
        callee: callee, arguments: arguments, original: AnyExprID.TypedNode(expr))
    }
  }

  private mutating func emit(
    argument expr: AnyExprID.TypedNode,
    to parameterType: ParameterType
  ) -> CXXExpr {
    switch parameterType.convention {
    case .let:
      return emitL(expr: expr, withCapability: .let)
    case .inout:
      return emitL(expr: expr, withCapability: .inout)
    case .set:
      return emitL(expr: expr, withCapability: .set)
    case .sink:
      return emitR(expr: expr)
    case .yielded:
      fatalError("not implemented")
    }
  }

  // MARK: l-values

  private mutating func emitL<ID: ExprID>(
    expr: ID.TypedNode,
    withCapability capability: AccessEffect
  ) -> CXXExpr {
    switch expr.kind {
    case NameExpr.self:
      return emitL(name: NameExpr.Typed(expr)!, withCapability: capability)

    case SubscriptCallExpr.self:
      fatalError("not implemented")

    default:
      // TODO: do we need to add extra instructions to covert this to l-value?
      return emitR(expr: expr)
    }
  }

  private mutating func emitL(
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
        // receiver = CXXReceiverExpr(original: expr)
        receiver = nil
      case .implicit:
        fatalError("not implemented")
      case .expr(let recv):
        receiver = emitL(expr: recv, withCapability: capability)
      }

      // Emit the compound expression.
      let idExpr = CXXIdentifier(nameOfDecl(decl))
      if receiver != nil {
        return CXXInfixExpr(
          oper: .dotAccess, lhs: receiver!, rhs: idExpr, original: AnyNodeID.TypedNode(expr))
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
