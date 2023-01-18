import BigInt
import Core
import FrontEnd
import Utils

/// A Val to C++ transpiler.
public struct CXXTranspiler {

  /// The program being transpiled.
  public let program: TypedProgram

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
  public mutating func emit(function decl: FunctionDecl.Typed, into module: inout CXXModule) {
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
    var cxxBody: CXXRepresentable? = nil
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
  private mutating func emit(funBody body: FunctionDecl.Typed.Body) -> CXXRepresentable {
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
        let cxxInitializer: CXXRepresentable? = nil
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

      case MethodDecl.self:
        cxxMembers.append(.method)

      default:
        unreachable("unexpected class member")
      }
    }

    // Create the C++ class.
    module.addTopLevelDecl(CXXClassDecl(name: name, members: cxxMembers, original: decl))
  }

  private mutating func emit(localBinding decl: BindingDecl.Typed) -> CXXRepresentable {
    let capability = decl.pattern.introducer.value

    // There's nothing to do if there's no initializer.
    if let initializer: TypedNode<AnyExprID> = decl.initializer {

      let isLValue =
        (initializer.kind == NameExpr.self) || (initializer.kind == SubscriptCallExpr.self)

      // Visit the initializer.
      let cxxInitialzer = emit(expr: initializer, asLValue: isLValue)

      // Visit the patterns.
      var stmts: [CXXRepresentable] = []
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
  private mutating func emit<ID: StmtID>(stmt: ID.TypedNode) -> CXXRepresentable {
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

  private mutating func emit(brace stmt: BraceStmt.Typed) -> CXXRepresentable {
    var stmts: [CXXRepresentable] = []
    for s in stmt.stmts {
      stmts.append(emit(stmt: s))
    }
    return CXXScopedBlock(stmts: stmts, original: AnyNodeID.TypedNode(stmt))
  }

  private mutating func emit(declStmt stmt: DeclStmt.Typed) -> CXXRepresentable {
    switch stmt.decl.kind {
    case BindingDecl.self:
      return emit(localBinding: BindingDecl.Typed(stmt.decl)!)
    default:
      unreachable("unexpected declaration")
    }
  }

  private mutating func emit(exprStmt stmt: ExprStmt.Typed) -> CXXRepresentable {
    return CXXExprStmt(expr: emitR(expr: stmt.expr), original: AnyNodeID.TypedNode(stmt))
  }

  private mutating func emit(assignStmt stmt: AssignStmt.Typed) -> CXXRepresentable {
    let cxxExpr = CXXInfixExpr(
      callee: CXXIdentifier("="),
      lhs: emitL(expr: stmt.left, withCapability: .set),
      rhs: emitR(expr: stmt.right),
      original: AnyNodeID.TypedNode(stmt))
    return CXXExprStmt(expr: cxxExpr, original: AnyNodeID.TypedNode(stmt))
  }

  private mutating func emit(returnStmt stmt: ReturnStmt.Typed) -> CXXRepresentable {
    var expr: CXXRepresentable?
    if stmt.value != nil {
      expr = emitR(expr: stmt.value!)
    }
    return CXXReturnStmt(expr: expr, original: AnyNodeID.TypedNode(stmt))
  }

  private mutating func emit(whileStmt stmt: WhileStmt.Typed) -> CXXRepresentable {
    return CXXComment(comment: "WhileStmt", original: AnyNodeID.TypedNode(stmt))
  }
  private mutating func emit(doWhileStmt stmt: DoWhileStmt.Typed) -> CXXRepresentable {
    return CXXComment(comment: "DoWhileStmt", original: AnyNodeID.TypedNode(stmt))
  }
  private mutating func emit(forStmt stmt: ForStmt.Typed) -> CXXRepresentable {
    return CXXComment(comment: "ForStmt", original: AnyNodeID.TypedNode(stmt))
  }
  private mutating func emit(breakStmt stmt: BreakStmt.Typed) -> CXXRepresentable {
    return CXXComment(comment: "BreakStmt", original: AnyNodeID.TypedNode(stmt))
  }
  private mutating func emit(continueStmt stmt: ContinueStmt.Typed) -> CXXRepresentable {
    return CXXComment(comment: "ContinueStmt", original: AnyNodeID.TypedNode(stmt))
  }
  private mutating func emit(yieldStmt stmt: YieldStmt.Typed) -> CXXRepresentable {
    return CXXComment(comment: "YieldStmt", original: AnyNodeID.TypedNode(stmt))
  }

  // MARK: Expressions

  private mutating func emit(
    expr: AnyExprID.TypedNode,
    asLValue: Bool
  ) -> CXXRepresentable {
    if asLValue {
      return emitL(expr: expr, withCapability: .let)
    } else {
      return emitR(expr: expr)
    }
  }

  // MARK: r-values

  private mutating func emitR<ID: ExprID>(expr: ID.TypedNode) -> CXXRepresentable {
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
  ) -> CXXRepresentable {
    return CXXBooleanLiteralExpr(value: expr.value, original: expr)
  }

  private mutating func emitR(
    cond expr: CondExpr.Typed
  ) -> CXXRepresentable {
    // TODO: multiple conditions
    // TODO: bindings in conditions
    let condition: CXXRepresentable
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
      let trueExpr: CXXRepresentable
      let falseExpr: CXXRepresentable
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
      let trueStmt: CXXRepresentable
      let falseStmt: CXXRepresentable?
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
      return CXXIfStmt(
        condition: condition,
        trueStmt: trueStmt,
        falseStmt: falseStmt,
        original: AnyNodeID.TypedNode(expr))
    }
  }

  private mutating func emitR(
    functionCall expr: FunctionCallExpr.Typed
  ) -> CXXRepresentable {
    let calleeType = expr.callee.type.base as! LambdaType

    // Arguments are evaluated first, from left to right.
    var argumentConventions: [AccessEffect] = []
    var arguments: [CXXRepresentable] = []

    for (parameter, argument) in zip(calleeType.inputs, expr.arguments) {
      let parameterType = parameter.type.base as! ParameterType
      argumentConventions.append(parameterType.convention)
      arguments.append(emit(argument: program[argument.value], to: parameterType))
    }

    // If the callee is a name expression referring to the declaration of a function capture-less
    // function, it is interpreted as a direct function reference. Otherwise, it is evaluated as a
    // function object the arguments.
    let callee: CXXRepresentable

    if let calleeNameExpr = NameExpr.Typed(expr.callee) {
      switch calleeNameExpr.decl {
      case .direct(let calleeDecl) where calleeDecl.kind == BuiltinDecl.self:
        // Callee refers to a built-in function.
        assert(calleeType.environment == .void)
        callee = CXXIdentifier(calleeNameExpr.name.value.stem)

      case .direct(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
        // Callee is a direct reference to a function or initializer declaration.
        // TODO: handle captures
        callee = CXXIdentifier(nameOfDecl(calleeDecl))

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

      case .member(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
        // Callee is a member reference to a function or method.
        let receiverType = calleeType.captures[0].type

        var receiver: CXXRepresentable

        // Add the receiver to the arguments.
        if let type = RemoteType(receiverType) {
          // The receiver as a borrowing convention.
          switch calleeNameExpr.domain {
          case .none:
            receiver = CXXReceiverExpr(original: AnyExprID.TypedNode(expr))

          case .expr(let receiverID):
            receiver = emitL(expr: receiverID, withCapability: type.capability)

          case .implicit:
            unreachable()
          }
        } else {
          // The receiver is consumed.
          switch calleeNameExpr.domain {
          case .none:
            receiver = CXXReceiverExpr(original: AnyExprID.TypedNode(expr))

          case .expr(let receiverID):
            receiver = emitR(expr: receiverID)

          case .implicit:
            unreachable()
          }
        }

        // Emit the function reference.
        callee = CXXCompoundExpr(
          base: receiver, id: CXXIdentifier(nameOfDecl(calleeDecl)),
          original: AnyExprID.TypedNode(expr))

      default:
        // Evaluate the callee as a function object.
        callee = emitR(expr: expr.callee)
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
  ) -> CXXRepresentable {
    return CXXIntegerLiteralExpr(value: expr.value, original: expr)
  }

  private mutating func emitR(
    name expr: NameExpr.Typed
  ) -> CXXRepresentable {
    return CXXComment(comment: "name expression", original: AnyNodeID.TypedNode(expr))
  }

  private mutating func emitR(
    sequence expr: SequenceExpr.Typed
  ) -> CXXRepresentable {
    return emit(foldedSequence: expr.foldedSequenceExprs!, withCapability: .sink, for: expr)
  }

  private mutating func emit(
    foldedSequence seq: FoldedSequenceExpr,
    withCapability capability: AccessEffect,
    for expr: SequenceExpr.Typed
  ) -> CXXRepresentable {
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
    lhs: CXXRepresentable,
    rhs: CXXRepresentable
  ) -> CXXRepresentable {

    // Obtained the declaration we are calling.
    let calleeDecl: AnyDeclID.TypedNode
    switch expr.decl {
    case .direct(let decl):
      calleeDecl = decl
    case .member(let decl):
      calleeDecl = decl
    }

    // Emit infix operators.
    // TODO: Do we need to check the base types?
    let name = nameOfDecl(calleeDecl)
    switch name {
    case "<<", ">>", "*", "*!", "/", "%", "+", "+!", "-", "-!", "..<", "...", "??", "==", "!=", "<",
      "<=", ">=", ">", "^", "&", "&&", "|", "||", "<<=", ">>=", "*=", "/=", "%=", "+=", "-=", "&=",
      "&&=", "**":
      // Expand this as a native infix operator call
      return CXXInfixExpr(
        callee: CXXIdentifier(name), lhs: lhs, rhs: rhs, original: AnyNodeID.TypedNode(expr))

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
  ) -> CXXRepresentable {
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
  ) -> CXXRepresentable {
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
  ) -> CXXRepresentable {
    switch expr.decl {
    case .direct(let decl):
      return CXXIdentifier(nameOfDecl(decl))

    case .member(let decl):
      // Emit the receiver.
      let receiver: CXXRepresentable?
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
        return CXXCompoundExpr(base: receiver!, id: idExpr, original: AnyExprID.TypedNode(expr))
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
