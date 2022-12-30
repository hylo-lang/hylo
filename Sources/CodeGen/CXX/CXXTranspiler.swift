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
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Emits the given function declaration into `module`.
  public mutating func emit(function decl: FunctionDecl.Typed, into module: inout CXXModule) {
    // Declare the function in the module if necessary.
    let id = module.getOrCreateFunction(correspondingTo: decl)

    // If we have a body for our function, emit it.
    if let body = decl.body {
      module.setFunctionBody(emit(funBody: body), forID: id)
    }
  }

  /// Translate the function body into a CXX entity.
  private mutating func emit(funBody body: FunctionDecl.Typed.Body) -> CXXRepresentable {
    switch body {
    case .block(let stmt):
      return emit(brace: stmt)

    case .expr(let expr):
      let exprStmt = emitR(expr: expr)
      return CXXScopedBlock([exprStmt], for: expr)
    }
  }

  // MARK: Declarations

  private mutating func emit(localBinding decl: BindingDecl.Typed) -> CXXRepresentable {
    let pattern = decl.pattern

    switch pattern.introducer.value {
    case .var, .sinklet:
      return emit(storedLocalBinding: decl)
    case .let:
      return emit(borrowedLocalBinding: decl, withCapability: .let)
    case .inout:
      return emit(borrowedLocalBinding: decl, withCapability: .inout)
    }
  }

  private mutating func emit(storedLocalBinding decl: BindingDecl.Typed) -> CXXRepresentable {
    return CXXComment("local binding", for: decl)
  }

  /// Emits borrowed bindings.
  private mutating func emit(
    borrowedLocalBinding decl: BindingDecl.Typed,
    withCapability capability: AccessEffect
  ) -> CXXRepresentable {
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
        stmts.append(
          CXXComment("decl \(name), type: \(decl.type.description); path: \(path)", for: name))
      }
      if stmts.isEmpty {
        // No pattern found; just call the initializer, dropping the result.
        return CXXVoidCast(baseExpr: cxxInitialzer, original: initializer)
      } else {
        return CXXScopedBlock(stmts, for: initializer)
      }
    } else {
      return CXXComment("EMPTY borrowed local binding (\(capability))", for: decl)
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
    case ReturnStmt.self:
      return emit(returnStmt: ReturnStmt.Typed(stmt)!)
    default:
      unreachable("unexpected statement")
    }
  }

  private mutating func emit(brace stmt: BraceStmt.Typed) -> CXXRepresentable {
    var stmts: [CXXRepresentable] = []
    for s in stmt.stmts {
      stmts.append(emit(stmt: s))
    }
    return CXXScopedBlock(stmts, for: stmt)
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
    return CXXComment("expr stmt", for: stmt)
  }

  private mutating func emit(returnStmt stmt: ReturnStmt.Typed) -> CXXRepresentable {
    return CXXComment("return stmt", for: stmt)
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
    return CXXBooleanLiteralExpr(expr.value, original: expr)
  }

  private mutating func emitR(
    cond expr: CondExpr.Typed
  ) -> CXXRepresentable {
    let isExpression = expr.type != .void
    if isExpression {
      // TODO: do we need to return an l-value?
      // TODO: multiple conditions
      // TODO: bindings in conditions
      let condition: CXXRepresentable
      let trueExpr: CXXRepresentable
      let falseExpr: CXXRepresentable
      if expr.condition.count == 1 {
        switch expr.condition[0] {
        case .expr(let condExpr):
          condition = emitR(expr: program[condExpr])
        case .decl(let decl):
          condition = CXXComment("binding condition", for: program[decl])
        }
      } else {
        fatalError("not implemented")
      }
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
        falseExpr = CXXComment("missing false alternative", for: expr)
      }
      return CXXConditionalExpr(
        condition: condition, trueExpr: trueExpr, falseExpr: falseExpr, original: expr)
    } else {
      return CXXComment("if statement", for: expr)
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
            receiver = CXXThisExpr(original: expr)

          case .expr(let receiverID):
            receiver = emitL(expr: receiverID, withCapability: type.capability)

          case .implicit:
            unreachable()
          }
        } else {
          // The receiver is consumed.
          switch calleeNameExpr.domain {
          case .none:
            receiver = CXXThisExpr(original: expr)

          case .expr(let receiverID):
            receiver = emitR(expr: receiverID)

          case .implicit:
            unreachable()
          }
        }

        // Emit the function reference.
        callee = CXXCompoundExpr(
          base: receiver, id: CXXIdentifier(nameOfDecl(calleeDecl)), original: expr)

      default:
        // Evaluate the callee as a function object.
        callee = emitR(expr: expr.callee)
      }
    } else {
      // Evaluate the callee as a function object.
      callee = emitR(expr: expr.callee)
    }

    return CXXFunctionCallExpr(callee: callee, arguments: arguments, original: expr)
  }

  private mutating func emitR(
    integerLiteral expr: IntegerLiteralExpr.Typed
  ) -> CXXRepresentable {
    return CXXIntegerLiteralExpr(expr.value, original: expr)
  }

  private mutating func emitR(
    name expr: NameExpr.Typed
  ) -> CXXRepresentable {
    return CXXComment("name expression", for: expr)
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

      // Build the resulting function call
      var comment: String = "fun call TODO("
      lhsOperand.writeCode(into: &comment)
      comment.write(", ")
      rhsOperand.writeCode(into: &comment)
      comment.write(")")
      return CXXComment(comment, for: expr)

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
      let receiver: CXXRepresentable
      switch expr.domain {
      case .none:
        receiver = CXXThisExpr(original: expr)
      case .implicit:
        fatalError("not implemented")
      case .expr(let recv):
        receiver = emitL(expr: recv, withCapability: capability)
      }

      // Emit the compound expression.
      switch decl.kind {
      case VarDecl.self:
        let varDecl = VarDecl.Typed(decl)!
        return CXXCompoundExpr(base: receiver, id: CXXIdentifier(varDecl.name), original: expr)
      default:
        fatalError("not implemented")
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
