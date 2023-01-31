import BigInt
import Core
import FrontEnd
import Utils

/// The conversion of typed Val module AST into corresponding C++ AST.
public struct CXXTranspiler {

  /// The Val typed nodes.
  ///
  /// This property is used when we need access to the contents of a node from its ID.
  let wholeValProgram: TypedProgram

  /// Creates a C++ transpiler from the whole Val program.
  public init(_ wholeValProgram: TypedProgram) {
    self.wholeValProgram = wholeValProgram
  }

  /// Returns a C++ AST implementing the semantics of `source`.
  public func transpile(_ source: ModuleDecl.Typed) -> CXXModule {
    return CXXModule(source: source, topLevelDecls: source.topLevelDecls.map({ cxx(topLevel: $0) }))
  }

  // MARK: Declarations

  /// Transpiles to C++ a top-level declaration.
  func cxx(topLevel source: AnyDeclID.TypedNode) -> CXXTopLevelDecl {
    switch source.kind {
    case FunctionDecl.self:
      return cxx(function: FunctionDecl.Typed(source)!)
    case ProductTypeDecl.self:
      return cxx(type: ProductTypeDecl.Typed(source)!)
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Transpiles to C++ a top-level function declaration.
  func cxx(function source: FunctionDecl.Typed) -> CXXFunctionDecl {
    assert(wholeValProgram.isGlobal(source.id))

    let functionName = source.identifier?.value ?? ""

    return CXXFunctionDecl(
      identifier: CXXIdentifier(functionName),
      output: cxxFunctionReturnType(source, with: functionName),
      parameters: cxxFunctionParameters(source),
      body: source.body != nil ? cxx(funBody: source.body!) : nil)
  }

  /// Transpiles the function return type into a C++ type.
  private func cxxFunctionReturnType(_ source: FunctionDecl.Typed, with name: String) -> CXXTypeExpr
  {
    if name == "main" {
      // The output type of `main` must be `int`.
      return CXXTypeExpr("int")
    } else {
      switch source.type.base {
      case let valDeclType as LambdaType:
        return cxx(typeExpr: valDeclType.output, asReturnType: true)

      case is MethodType:
        fatalError("not implemented")

      default:
        unreachable()
      }
    }
  }

  /// Transpiles the function parameters to C++.
  private func cxxFunctionParameters(_ source: FunctionDecl.Typed) -> [CXXFunctionDecl.Parameter] {
    let paramTypes = (source.type.base as! LambdaType).inputs
    assert(paramTypes.count == source.parameters.count)
    return zip(source.parameters, paramTypes).map { p, t in
      CXXFunctionDecl.Parameter(CXXIdentifier(p.baseName), cxx(typeExpr: t.type))
    }
  }

  /// Transpiles a function body to a C++.
  private func cxx(funBody body: FunctionDecl.Typed.Body) -> CXXScopedBlock {
    switch body {
    case .block(let stmt):
      return cxx(brace: stmt)

    case .expr(let expr):
      let exprBody = CXXReturnStmt(expr: cxx(expr: expr))
      return CXXScopedBlock(stmts: [exprBody])
    }
  }

  /// Transpiles a product type declaration to a C++ class.
  private func cxx(type source: ProductTypeDecl.Typed) -> CXXClassDecl {
    assert(wholeValProgram.isGlobal(source.id))
    return CXXClassDecl(
      name: CXXIdentifier(source.identifier.value),
      members: source.members.flatMap({ cxx(classMember: $0) }))
  }

  /// Transpiles product type member to C++.
  private func cxx(classMember source: AnyDeclID.TypedNode) -> [CXXClassDecl.ClassMember] {
    switch source.kind {
    case BindingDecl.self:
      // One binding can expand into multiple class attributes
      return cxx(productTypeBinding: BindingDecl.Typed(source)!)

    case InitializerDecl.self:
      return [cxx(productTypeInitialzer: InitializerDecl.Typed(source)!)]

    case MethodDecl.self, FunctionDecl.self:
      return [.method]

    default:
      unreachable("unexpected class member")
    }
  }

  /// Appends one C++ class attribute to `target` for each name bound in `source`.
  private func cxx(productTypeBinding source: BindingDecl.Typed) -> [CXXClassDecl.ClassMember] {
    return source.pattern.subpattern.names.map({
      .attribute(
        CXXClassAttribute(
          type: cxx(typeExpr: $0.pattern.decl.type),
          name: CXXIdentifier($0.pattern.decl.baseName),
          initializer: nil,  // TODO
          isStatic: source.isStatic))
    })
  }
  /// Translate an initializer found in product type to the corresponding C++ constructor.
  private func cxx(productTypeInitialzer source: InitializerDecl.Typed) -> CXXClassDecl.ClassMember
  {
    switch source.introducer.value {
    case .`init`:
      // TODO: emit constructor
      return .constructor
    case .memberwiseInit:
      // TODO: emit constructor
      return .constructor
    }
  }

  /// Transpiles a local binding declaration to a C++ statement containing a local variable.
  ///
  /// The statement can be a `CXXLocalVarDecl` or a `CXXScopedBlock` containing `CXXLocalVarDecl` objects.
  private func cxx(localBinding source: BindingDecl.Typed) -> CXXStmt {
    let cxxInitializer = source.initializer != nil ? cxx(expr: source.initializer!) : nil
    let numNames = source.pattern.subpattern.names.count

    assert(numNames > 0 || cxxInitializer != nil)

    if numNames > 1 {
      let varStmts = source.pattern.subpattern.names.map({
        cxx(localNamePattern: $0.pattern, with: cxxInitializer)
      })
      // TODO: scoped block is not good here, because of lifetime issues
      return CXXScopedBlock(stmts: varStmts)
    } else if numNames == 1 {
      return cxx(localNamePattern: source.pattern.subpattern.names[0].pattern, with: cxxInitializer)
    } else if numNames == 0 {
      // No pattern found; just call the initializer, dropping the result.
      let cxxExpr = CXXVoidCast(baseExpr: cxxInitializer!)
      return CXXExprStmt(expr: cxxExpr)
    } else {
      unreachable()
    }
  }
  /// Transpile to a C++ local variable the Val name pattern, using a C++ expression as intializer.
  private func cxx(localNamePattern source: NamePattern.Typed, with cxxInitializer: CXXExpr?)
    -> CXXLocalVarDecl
  {
    CXXLocalVarDecl(
      type: cxx(typeExpr: source.decl.type),
      name: CXXIdentifier(source.decl.identifier.value),
      initializer: cxxInitializer)
  }

  // MARK: Statements

  /// Transpiles a Val statement into the corresponding C++ statement.
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

  /// Transpiles a Val brace statement into the corresponding C++ scoped block statement.
  private func cxx(brace source: BraceStmt.Typed) -> CXXScopedBlock {
    return CXXScopedBlock(stmts: Array(source.stmts.map({ cxx(stmt: $0) })))
  }

  /// Transpiles a Val declaration statement into the corresponding C++ local decl.
  private func cxx(declStmt source: DeclStmt.Typed) -> CXXStmt {
    switch source.decl.kind {
    case BindingDecl.self:
      return cxx(localBinding: BindingDecl.Typed(source.decl)!)
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Transpiles a Val expression statement into the corresponding C++ expression statement.
  private func cxx(exprStmt source: ExprStmt.Typed) -> CXXStmt {
    return CXXExprStmt(expr: cxx(expr: source.expr))
  }

  /// Transpiles a Val assignment statement into an C++ statement containing an assign expression.
  private func cxx(assignStmt source: AssignStmt.Typed) -> CXXExprStmt {
    let cxxExpr = CXXInfixExpr(
      oper: .assignment,
      lhs: cxx(expr: source.left),
      rhs: cxx(expr: source.right))
    return CXXExprStmt(expr: cxxExpr)
  }

  /// Transpiles a Val return statement into the corresponding C++ return statement.
  private func cxx(returnStmt source: ReturnStmt.Typed) -> CXXReturnStmt {
    let returnValue = source.value != nil ? cxx(expr: source.value!) : nil
    return CXXReturnStmt(expr: returnValue)
  }

  /// Transpiles a Val while statement into the corresponding C++ while statement.
  private func cxx(whileStmt source: WhileStmt.Typed) -> CXXWhileStmt {
    // TODO: multiple conditions
    // TODO: bindings in conditions
    let condition: CXXExpr
    if source.condition.count == 1 {
      switch source.condition[0] {
      case .expr(let condExpr):
        condition = cxx(expr: wholeValProgram[condExpr])
      case .decl(let decl):
        let _ = decl
        condition = CXXComment(comment: "binding condition")
      }
    } else {
      fatalError("not implemented")
    }
    return CXXWhileStmt(
      condition: condition, body: cxx(stmt: source.body))
  }
  /// Transpiles a Val do-while statement into the corresponding C++ do-while statement.
  private func cxx(doWhileStmt source: DoWhileStmt.Typed) -> CXXDoWhileStmt {
    return CXXDoWhileStmt(
      body: cxx(stmt: source.body),
      condition: cxx(expr: source.condition))
  }
  /// Transpiles a Val for statement into the corresponding C++ for statement.
  private func cxx(forStmt source: ForStmt.Typed) -> CXXStmt {
    return CXXComment(comment: "ForStmt")
  }
  /// Transpiles a Val break statement into a C++ break statement.
  private func cxx(breakStmt source: BreakStmt.Typed) -> CXXBreakStmt {
    return CXXBreakStmt()
  }
  /// Transpiles a Val continue statement into a C++ continue statement.
  private func cxx(continueStmt source: ContinueStmt.Typed) -> CXXContinueStmt {
    return CXXContinueStmt()
  }
  /// Transpiles a Val yield statement into a corresponding C++ statement.
  private func cxx(yieldStmt source: YieldStmt.Typed) -> CXXStmt {
    return CXXComment(comment: "YieldStmt")
  }

  // MARK: Expressions

  /// Transpiles a Val expression into a corresponding C++ expression.
  private func cxx(expr: AnyExprID.TypedNode) -> CXXExpr {
    switch expr.kind {
    case BooleanLiteralExpr.self:
      return cxx(booleanLiteral: BooleanLiteralExpr.Typed(expr)!)
    case IntegerLiteralExpr.self:
      return cxx(integerLiteral: IntegerLiteralExpr.Typed(expr)!)
    case NameExpr.self:
      return cxx(name: NameExpr.Typed(expr)!)
    case SequenceExpr.self:
      return cxx(sequence: SequenceExpr.Typed(expr)!)
    case FunctionCallExpr.self:
      return cxx(functionCall: FunctionCallExpr.Typed(expr)!)
    case CondExpr.self:
      return cxx(cond: CondExpr.Typed(expr)!)
    default:
      unreachable("unexpected expression")
    }
  }

  /// Transpiles a Val boolean literal into a corresponding C++ literal.
  private func cxx(booleanLiteral source: BooleanLiteralExpr.Typed) -> CXXBooleanLiteralExpr {
    return CXXBooleanLiteralExpr(value: source.value)
  }

  /// Transpiles a Val integer literal into a corresponding C++ literal.
  private func cxx(integerLiteral source: IntegerLiteralExpr.Typed) -> CXXExpr {
    return CXXIntegerLiteralExpr(value: source.value)
  }

  /// Transpiles a Val sequence expression into a corresponding C++ expression.
  private func cxx(sequence source: SequenceExpr.Typed) -> CXXExpr {
    return cxx(foldedSequence: source.foldedSequenceExprs!)
  }

  /// Transpiles a Val folded sequence expression into a corresponding C++ expression.
  private func cxx(foldedSequence seq: FoldedSequenceExpr) -> CXXExpr {
    switch seq {
    case .infix(let (callee, _), let lhs, let rhs):
      return cxx(
        infix: wholeValProgram[callee],
        lhs: cxx(foldedSequence: lhs),
        rhs: cxx(foldedSequence: rhs))

    case .leaf(let expr):
      return cxx(expr: wholeValProgram[expr])
    }
  }

  /// Transpiles a Val infix operator call into a corresponding C++ infix operator / function call.
  private func cxx(
    infix source: NameExpr.Typed,
    lhs: CXXExpr,
    rhs: CXXExpr
  ) -> CXXExpr {
    let name = nameOfDecl(source.decl.decl)
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
      let callee = cxx(name: source)
      let arguments = [lhs, rhs]
      return CXXFunctionCallExpr(callee: callee, arguments: arguments)
    }
  }

  /// Transpiles a Val function call into a corresponding C++ function call.
  private func cxx(
    functionCall source: FunctionCallExpr.Typed
  ) -> CXXExpr {
    // Transpile the arguments
    // TODO: passing conventions
    let arguments = Array(source.arguments.map({ cxx(expr: wholeValProgram[$0.value]) }))

    // Transpile the calee expression
    let callee: CXXExpr
    if let calleeNameExpr = NameExpr.Typed(source.callee) {
      // If the callee is a name expression, we might need the callee type when expanding the name.
      callee = cxx(name: calleeNameExpr)

      // The name expresssion might fully represent an prefix/postfix operator call;
      // in this case, we don't need to wrap this into a function call
      if callee is CXXPrefixExpr || callee is CXXPostfixExpr {
        assert(arguments.isEmpty)
        return callee
      }
    } else {
      callee = cxx(expr: source.callee)
    }

    return CXXFunctionCallExpr(callee: callee, arguments: arguments)
  }

  /// Transpiles a Val conditional expression into a corresponding C++ expression.
  ///
  /// As much as possible, this will be converted into a ternary operator (CXXConditionalExpr).
  /// There are, however, in wich this needs to be translated into an if statment, then trasformed into an expression.
  private func cxx(cond source: CondExpr.Typed) -> CXXExpr {
    // TODO: multiple conditions
    // TODO: bindings in conditions
    let condition: CXXExpr
    if source.condition.count == 1 {
      switch source.condition[0] {
      case .expr(let condExpr):
        condition = cxx(expr: wholeValProgram[condExpr])
      case .decl(let decl):
        let _ = decl
        condition = CXXComment(comment: "binding condition")
      }
    } else {
      fatalError("not implemented")
    }
    // TODO: better checks if we need an expression or a statement
    if source.type != .void {
      // We result in an expression
      // TODO: do we need to return an l-value?
      let trueExpr = cxx(condBodyExpr: source.success)
      let falseExpr = cxx(condBodyExpr: source.failure)
      return CXXConditionalExpr(condition: condition, trueExpr: trueExpr, falseExpr: falseExpr)
    } else {
      // We result in a statement, and we wrap the statement into an expression
      let trueStmt = cxx(condBodyStmt: source.success)!
      let falseStmt = cxx(condBodyStmt: source.failure)
      return CXXStmtExpr(
        stmt: CXXIfStmt(condition: condition, trueStmt: trueStmt, falseStmt: falseStmt))
    }
  }
  /// Transates a conditional expression body into the corresponding C++ expression
  private func cxx(condBodyExpr source: CondExpr.Body?) -> CXXExpr {
    switch source {
    case .expr(let altExpr):
      return cxx(expr: wholeValProgram[altExpr])
    case .block:
      fatalError("not implemented")
    case .none:
      return CXXComment(comment: "missing alternative")
    }
  }
  /// Transates a conditional expression body into the corresponding C++ statement
  private func cxx(condBodyStmt source: CondExpr.Body?) -> CXXStmt? {
    switch source {
    case .expr(let altExpr):
      return CXXExprStmt(expr: CXXVoidCast(baseExpr: cxx(expr: wholeValProgram[altExpr])))
    case .block(let altStmt):
      return cxx(stmt: wholeValProgram[altStmt])
    case .none:
      return nil
    }
  }

  // MARK: names

  /// Transpiles a Val r-value name expression into a corresponding expression.
  ///
  /// This usually result into a C++ identifier, but it can also result in pre-/post-fix operator calls.
  private func cxx(name source: NameExpr.Typed) -> CXXExpr {
    switch source.decl {
    case .direct(let calleeDecl) where calleeDecl.kind == BuiltinDecl.self:
      return cxx(nameOfBuiltin: source)
    case .direct(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
      return cxx(
        nameOfFunction: FunctionDecl.Typed(calleeDecl)!, withDomainExpr: source.domainExpr)
    case .direct(let calleeDecl) where calleeDecl.kind == InitializerDecl.self:
      return cxx(nameOfInitializer: InitializerDecl.Typed(calleeDecl)!)
    case .direct(let calleeDecl):
      // For variables, and other declarations, just use the name of the declaration
      return CXXIdentifier(nameOfDecl(calleeDecl))

    case .member(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
      return cxx(nameOfMemberFunction: FunctionDecl.Typed(calleeDecl)!, withDomain: source.domain)

    case .member(_):
      fatalError("not implemented")
    }
  }
  /// Translates a name pointing to a builin to a C++ expression.
  private func cxx(nameOfBuiltin source: NameExpr.Typed) -> CXXExpr {
    return CXXIdentifier(source.name.value.stem)
  }
  /// Translates a name pointing to a fumction declaration to a C++ expression.
  ///
  /// Usually this translates to an identifier, but it can translate to pre-/post-fix operator calls.
  private func cxx(
    nameOfFunction source: FunctionDecl.Typed, withDomainExpr domainExpr: AnyExprID.TypedNode?
  ) -> CXXExpr {
    // Check for prefix && postfix operator calls
    if source.notation != nil && source.notation!.value == .prefix {
      let prefixOperators: [String: CXXPrefixExpr.Operator] = [
        "++": .prefixIncrement,
        "--": .prefixDecrement,
        "+": .unaryPlus,
        "-": .unaryMinus,
        "!": .logicalNot,
      ]
      if let cxxPrefixOperator = prefixOperators[nameOfDecl(source)] {
        return CXXPrefixExpr(
          oper: cxxPrefixOperator, base: cxx(expr: domainExpr!))
      }
    } else if source.notation != nil && source.notation!.value == .postfix {
      let postfixOperators: [String: CXXPostfixExpr.Operator] = [
        "++": .suffixIncrement,
        "--": .suffixDecrement,
      ]
      if let cxxPostfixOperator = postfixOperators[nameOfDecl(source)] {
        return CXXPostfixExpr(
          oper: cxxPostfixOperator, base: cxx(expr: domainExpr!))
      }
    }

    // TODO: handle captures
    return CXXIdentifier(nameOfDecl(source))
  }
  /// Translates a name pointing to an initializer to a C++ expression.
  private func cxx(nameOfInitializer source: InitializerDecl.Typed) -> CXXExpr {
    fatalError("not implemented")
  }
  /// Translates a name pointing to a member function to a C++ expression.
  private func cxx(
    nameOfMemberFunction source: FunctionDecl.Typed, withDomain domain: NameExpr.Typed.Domain
  ) -> CXXExpr {

    // TODO: revisit this whole function; simplify it, and check for correctness

    var receiver: CXXExpr
    switch domain {
    case .none:
      receiver = CXXReceiverExpr()
    case .expr(let receiverID):
      receiver = cxx(expr: receiverID)
    case .implicit:
      unreachable()
    }

    // Emit the function reference.
    return CXXInfixExpr(oper: .dotAccess, lhs: receiver, rhs: CXXIdentifier(nameOfDecl(source)))
  }

  // MARK: miscelaneous

  /// Transates a Val type into a C++ type expression.
  private func cxx(typeExpr source: AnyType, asReturnType isReturnType: Bool = false) -> CXXTypeExpr
  {
    switch source.base {
    case AnyType.void:
      return CXXTypeExpr(isReturnType ? "void" : "std::monostate")

    case let type as ProductType:
      // TODO: we should translate this to an "int" struct
      if type == wholeValProgram.ast.coreType(named: "Int") {
        return CXXTypeExpr("int")
      } else {
        return CXXTypeExpr(type.name.value)
      }

    case let type as ParameterType:
      // TODO: convention
      return cxx(typeExpr: type.bareType)

    default:
      fatalError("not implemented")
    }
  }

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

    case MethodImpl.self:
      let methodImpl = MethodImpl.Typed(decl)!
      switch methodImpl.introducer.value {
      case .let: return "let"
      case .inout: return "inout"
      case .set: return "set"
      case .sink: return "sink"
      }

    case ProductTypeDecl.self:
      return ProductTypeDecl.Typed(decl)!.baseName

    case ParameterDecl.self:
      return ParameterDecl.Typed(decl)!.identifier.value
    case VarDecl.self:
      return VarDecl.Typed(decl)!.identifier.value

    default:
      fatalError("not implemented")
    }

  }

}
