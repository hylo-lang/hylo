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

  /// Creates an instance.
  public init(_ wholeValProgram: TypedProgram) {
    self.wholeValProgram = wholeValProgram
  }

  /// Returns a C++ AST implementing the semantics of `source`.
  public func transpile(_ source: ModuleDecl.Typed) -> CXXModule {
    return CXXModule(
      source: source, topLevelDecls: source.topLevelDecls.map({ cxx(topLevel: $0) }),
      entryPointBody: entryPointBody(module: source))
  }

  // MARK: Declarations

  /// Returns a transpilation of `source`.
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

  /// Returns a transpilation of `source`.
  func cxx(function source: FunctionDecl.Typed) -> CXXFunctionDecl {
    assert(wholeValProgram.isGlobal(source.id))

    let functionName = source.identifier?.value ?? ""

    return CXXFunctionDecl(
      identifier: CXXIdentifier(functionName),
      output: cxxFunctionReturnType(source, with: functionName),
      parameters: cxxFunctionParameters(source),
      body: source.body != nil ? cxx(funBody: source.body!) : nil)
  }

  /// Returns a transpilation of the return type `source`.
  private func cxxFunctionReturnType(_ source: FunctionDecl.Typed, with name: String)
    -> CXXTypeExpr  // TODO: this will be refactored by follow-up patches.
  {
    switch source.type.base {
    case let valDeclType as LambdaType:
      return cxx(typeExpr: valDeclType.output, asReturnType: true)

    case is MethodType:
      fatalError("not implemented")

    default:
      unreachable()
    }
  }

  /// Returns a transpilation of the parameters of `source`.
  private func cxxFunctionParameters(_ source: FunctionDecl.Typed) -> [CXXFunctionDecl.Parameter] {
    let paramTypes = (source.type.base as! LambdaType).inputs
    assert(paramTypes.count == source.parameters.count)
    return zip(source.parameters, paramTypes).map { p, t in
      CXXFunctionDecl.Parameter(CXXIdentifier(p.baseName), cxx(typeExpr: t.type))
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(funBody body: FunctionDecl.Typed.Body) -> CXXScopedBlock {
    switch body {
    case .block(let bodyDetails):
      return cxx(brace: bodyDetails)

    case .expr(let bodyDetails):
      return CXXScopedBlock(stmts: [CXXReturnStmt(expr: cxx(expr: bodyDetails))])
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(type source: ProductTypeDecl.Typed) -> CXXClassDecl {
    assert(wholeValProgram.isGlobal(source.id))
    return CXXClassDecl(
      name: CXXIdentifier(source.identifier.value),
      members: source.members.flatMap({ cxx(classMember: $0) }))
  }

  /// Returns a transpilation of `source`.
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

  /// Returns a transpilation of `source`.
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
  /// Returns a transpilation of `source`.
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

  /// Returns a transpilation of `source`.
  ///
  /// Returned statement can be a `CXXLocalVarDecl` or a `CXXScopedBlock` containing `CXXLocalVarDecl` objects.
  private func cxx(localBinding source: BindingDecl.Typed) -> CXXStmt {
    let cxxInitializer = source.initializer != nil ? cxx(expr: source.initializer!) : nil
    let numNames = source.pattern.subpattern.names.count

    assert(numNames > 0 || cxxInitializer != nil)

    if numNames > 1 {
      let variables = source.pattern.subpattern.names.map({
        cxx(localNamePattern: $0.pattern, with: cxxInitializer)
      })
      // TODO: scoped block is not good here, because of lifetime issues
      return CXXScopedBlock(stmts: variables)
    } else if numNames == 1 {
      return cxx(localNamePattern: source.pattern.subpattern.names[0].pattern, with: cxxInitializer)
    } else if numNames == 0 {
      // No pattern found; just call the initializer, dropping the result.
      return CXXExprStmt(expr: CXXVoidCast(baseExpr: cxxInitializer!))
    } else {
      unreachable()
    }
  }
  /// Returns a transpilation of `source`.
  private func cxx(localNamePattern source: NamePattern.Typed, with cxxInitializer: CXXExpr?)
    -> CXXLocalVarDecl
  {
    CXXLocalVarDecl(
      type: cxx(typeExpr: source.decl.type),
      name: CXXIdentifier(source.decl.identifier.value),
      initializer: cxxInitializer)
  }

  // MARK: Statements

  /// Returns a transpilation of `source`.
  private func cxx<ID: StmtID>(stmt source: ID.TypedNode) -> CXXStmt {
    switch source.kind {
    case BraceStmt.self:
      return cxx(brace: BraceStmt.Typed(source)!)
    case DeclStmt.self:
      return cxx(declStmt: DeclStmt.Typed(source)!)
    case ExprStmt.self:
      return cxx(exprStmt: ExprStmt.Typed(source)!)
    case AssignStmt.self:
      return cxx(assignStmt: AssignStmt.Typed(source)!)
    case ReturnStmt.self:
      return cxx(returnStmt: ReturnStmt.Typed(source)!)
    case WhileStmt.self:
      return cxx(whileStmt: WhileStmt.Typed(source)!)
    case DoWhileStmt.self:
      return cxx(doWhileStmt: DoWhileStmt.Typed(source)!)
    case ForStmt.self:
      return cxx(forStmt: ForStmt.Typed(source)!)
    case BreakStmt.self:
      return cxx(breakStmt: BreakStmt.Typed(source)!)
    case ContinueStmt.self:
      return cxx(continueStmt: ContinueStmt.Typed(source)!)
    case YieldStmt.self:
      return cxx(yieldStmt: YieldStmt.Typed(source)!)
    default:
      unreachable("unexpected statement")
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(brace source: BraceStmt.Typed) -> CXXScopedBlock {
    return CXXScopedBlock(stmts: Array(source.stmts.map({ cxx(stmt: $0) })))
  }

  /// Returns a transpilation of `source`.
  private func cxx(declStmt source: DeclStmt.Typed) -> CXXStmt {
    switch source.decl.kind {
    case BindingDecl.self:
      return cxx(localBinding: BindingDecl.Typed(source.decl)!)
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(exprStmt source: ExprStmt.Typed) -> CXXStmt {
    return CXXExprStmt(expr: cxx(expr: source.expr))
  }

  /// Returns a transpilation of `source`.
  private func cxx(assignStmt source: AssignStmt.Typed) -> CXXExprStmt {
    return CXXExprStmt(
      expr: CXXInfixExpr(
        oper: .assignment,
        lhs: cxx(expr: source.left),
        rhs: cxx(expr: source.right)))
  }

  /// Returns a transpilation of `source`.
  private func cxx(returnStmt source: ReturnStmt.Typed) -> CXXReturnStmt {
    return CXXReturnStmt(expr: source.value != nil ? cxx(expr: source.value!) : nil)
  }

  /// Returns a transpilation of `source`.
  private func cxx(whileStmt source: WhileStmt.Typed) -> CXXWhileStmt {
    // TODO: multiple conditions
    // TODO: bindings in conditions
    let condition: CXXExpr
    if source.condition.count == 1 {
      switch source.condition[0] {
      case .expr(let conditionDetails):
        condition = cxx(expr: wholeValProgram[conditionDetails])
      case .decl(_):
        condition = CXXComment(comment: "binding condition")
      }
    } else {
      fatalError("not implemented")
    }
    return CXXWhileStmt(
      condition: condition, body: cxx(stmt: source.body))
  }
  /// Returns a transpilation of `source`.
  private func cxx(doWhileStmt source: DoWhileStmt.Typed) -> CXXDoWhileStmt {
    return CXXDoWhileStmt(
      body: cxx(stmt: source.body),
      condition: cxx(expr: source.condition))
  }
  /// Returns a transpilation of `source`.
  private func cxx(forStmt source: ForStmt.Typed) -> CXXStmt {
    return CXXComment(comment: "ForStmt")
  }
  /// Returns a transpilation of `source`.
  private func cxx(breakStmt source: BreakStmt.Typed) -> CXXBreakStmt {
    return CXXBreakStmt()
  }
  /// Returns a transpilation of `source`.
  private func cxx(continueStmt source: ContinueStmt.Typed) -> CXXContinueStmt {
    return CXXContinueStmt()
  }
  /// Returns a transpilation of `source`.
  private func cxx(yieldStmt source: YieldStmt.Typed) -> CXXStmt {
    return CXXComment(comment: "YieldStmt")
  }

  // MARK: Expressions

  /// Returns a transpilation of `source`.
  private func cxx(expr source: AnyExprID.TypedNode) -> CXXExpr {
    switch source.kind {
    case BooleanLiteralExpr.self:
      return cxx(booleanLiteral: BooleanLiteralExpr.Typed(source)!)
    case IntegerLiteralExpr.self:
      return cxx(integerLiteral: IntegerLiteralExpr.Typed(source)!)
    case NameExpr.self:
      return cxx(name: NameExpr.Typed(source)!)
    case SequenceExpr.self:
      return cxx(sequence: SequenceExpr.Typed(source)!)
    case FunctionCallExpr.self:
      return cxx(functionCall: FunctionCallExpr.Typed(source)!)
    case CondExpr.self:
      return cxx(cond: CondExpr.Typed(source)!)
    default:
      unreachable("unexpected expression")
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(booleanLiteral source: BooleanLiteralExpr.Typed) -> CXXBooleanLiteralExpr {
    return CXXBooleanLiteralExpr(value: source.value)
  }

  /// Returns a transpilation of `source`.
  private func cxx(integerLiteral source: IntegerLiteralExpr.Typed) -> CXXExpr {
    return CXXIntegerLiteralExpr(value: source.value)
  }

  /// Returns a transpilation of `source`.
  private func cxx(sequence source: SequenceExpr.Typed) -> CXXExpr {
    return cxx(foldedSequence: source.foldedSequenceExprs!)
  }

  /// Returns a transpilation of `source`.
  private func cxx(foldedSequence source: FoldedSequenceExpr) -> CXXExpr {
    switch source {
    case .infix(let (callee, _), let lhs, let rhs):
      return cxx(
        infix: wholeValProgram[callee],
        lhs: cxx(foldedSequence: lhs),
        rhs: cxx(foldedSequence: rhs))

    case .leaf(let leafNode):
      return cxx(expr: wholeValProgram[leafNode])
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(
    infix source: NameExpr.Typed,
    lhs: CXXExpr,
    rhs: CXXExpr
  ) -> CXXExpr {
    switch nameOfDecl(source.decl.decl) {
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
      return CXXFunctionCallExpr(callee: cxx(name: source), arguments: [lhs, rhs])
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(functionCall source: FunctionCallExpr.Typed) -> CXXExpr {
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

  /// Returns a transpilation of `source`.
  ///
  /// As much as possible, this will be converted into a ternary operator (CXXConditionalExpr).
  /// There are, however, in wich this needs to be translated into an if statment, then trasformed into an expression.
  private func cxx(cond source: CondExpr.Typed) -> CXXExpr {
    // TODO: multiple conditions
    // TODO: bindings in conditions
    let condition: CXXExpr
    if source.condition.count == 1 {
      switch source.condition[0] {
      case .expr(let conditionDetails):
        condition = cxx(expr: wholeValProgram[conditionDetails])
      case .decl(_):
        condition = CXXComment(comment: "binding condition")
      }
    } else {
      fatalError("not implemented")
    }
    // TODO: better checks if we need an expression or a statement
    if source.type != .void {
      // We result in an expression
      // TODO: do we need to return an l-value?
      return CXXConditionalExpr(
        condition: condition, trueExpr: cxx(condBodyExpr: source.success),
        falseExpr: cxx(condBodyExpr: source.failure))
    } else {
      // We result in a statement, and we wrap the statement into an expression
      return CXXStmtExpr(
        stmt: CXXIfStmt(
          condition: condition, trueStmt: cxx(condBodyStmt: source.success)!,
          falseStmt: cxx(condBodyStmt: source.failure)))
    }
  }
  /// Returns a transpilation of `source` as an expression.
  private func cxx(condBodyExpr source: CondExpr.Body?) -> CXXExpr {
    switch source {
    case .expr(let alternativeDetails):
      return cxx(expr: wholeValProgram[alternativeDetails])
    case .block:
      fatalError("not implemented")
    case .none:
      return CXXComment(comment: "missing alternative")
    }
  }
  /// Returns a transpilation of `source` as a statement.
  private func cxx(condBodyStmt source: CondExpr.Body?) -> CXXStmt? {
    switch source {
    case .expr(let alternativeDetails):
      return CXXExprStmt(
        expr: CXXVoidCast(baseExpr: cxx(expr: wholeValProgram[alternativeDetails])))
    case .block(let alternativeDetails):
      return cxx(stmt: wholeValProgram[alternativeDetails])
    case .none:
      return nil
    }
  }

  // MARK: names

  /// Returns a transpilation of `source`.
  ///
  /// This usually result into a C++ identifier, but it can also result in pre-/post-fix operator calls.
  private func cxx(name source: NameExpr.Typed) -> CXXExpr {
    switch source.decl {
    case .direct(let callee) where callee.kind == BuiltinDecl.self:
      return cxx(nameOfBuiltin: source)
    case .direct(let callee) where callee.kind == FunctionDecl.self:
      return cxx(
        nameOfFunction: FunctionDecl.Typed(callee)!, withDomainExpr: source.domainExpr)
    case .direct(let callee) where callee.kind == InitializerDecl.self:
      return cxx(nameOfInitializer: InitializerDecl.Typed(callee)!)
    case .direct(let callee):
      // For variables, and other declarations, just use the name of the declaration
      return CXXIdentifier(nameOfDecl(callee))

    case .member(let callee) where callee.kind == FunctionDecl.self:
      return cxx(nameOfMemberFunction: FunctionDecl.Typed(callee)!, withDomain: source.domain)

    case .member(_):
      fatalError("not implemented")
    }
  }
  /// Returns a transpilation of `source`.
  private func cxx(nameOfBuiltin source: NameExpr.Typed) -> CXXExpr {
    return CXXIdentifier(source.name.value.stem)
  }
  /// Returns a transpilation of `source`.
  ///
  /// Usually this translates to an identifier, but it can translate to pre-/post-fix operator calls.
  private func cxx(
    nameOfFunction source: FunctionDecl.Typed, withDomainExpr domainExpr: AnyExprID.TypedNode?
  ) -> CXXExpr {
    // Check for prefix && postfix operator calls
    if source.notation != nil && source.notation!.value == .prefix {
      let prefixMapping: [String: CXXPrefixExpr.Operator] = [
        "++": .prefixIncrement,
        "--": .prefixDecrement,
        "+": .unaryPlus,
        "-": .unaryMinus,
        "!": .logicalNot,
      ]
      if let cxxPrefixOperator = prefixMapping[nameOfDecl(source)] {
        return CXXPrefixExpr(
          oper: cxxPrefixOperator, base: cxx(expr: domainExpr!))
      }
    } else if source.notation != nil && source.notation!.value == .postfix {
      let postfixMapping: [String: CXXPostfixExpr.Operator] = [
        "++": .suffixIncrement,
        "--": .suffixDecrement,
      ]
      if let cxxPostfixOperator = postfixMapping[nameOfDecl(source)] {
        return CXXPostfixExpr(
          oper: cxxPostfixOperator, base: cxx(expr: domainExpr!))
      }
    }

    // TODO: handle captures
    return CXXIdentifier(nameOfDecl(source))
  }
  /// Returns a transpilation of `source`.
  private func cxx(nameOfInitializer source: InitializerDecl.Typed) -> CXXExpr {
    fatalError("not implemented")
  }
  /// Returns a transpilation of `source`.
  private func cxx(
    nameOfMemberFunction source: FunctionDecl.Typed, withDomain domain: NameExpr.Typed.Domain
  ) -> CXXExpr {

    // TODO: revisit this whole function; simplify it, and check for correctness

    var receiver: CXXExpr
    switch domain {
    case .none:
      receiver = CXXReceiverExpr()
    case .expr(let domainDetails):
      receiver = cxx(expr: domainDetails)
    case .implicit:
      unreachable()
    }

    // Emit the function reference.
    return CXXInfixExpr(oper: .dotAccess, lhs: receiver, rhs: CXXIdentifier(nameOfDecl(source)))
  }

  // MARK: miscelaneous

  /// Returns a transpilation of `source`.
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

  /// Returns of `source`, without manging and labels.
  private func nameOfDecl<T: DeclID>(_ source: TypedNode<T>) -> String {
    switch source.kind {
    case ConformanceDecl.self, ExtensionDecl.self:
      fatalError("not implemented")

    case FunctionDecl.self:
      return FunctionDecl.Typed(source)!.identifier!.value

    case InitializerDecl.self:
      return "init"

    case MethodDecl.self:
      return MethodDecl.Typed(source)!.identifier.value

    case MethodImpl.self:
      switch MethodImpl.Typed(source)!.introducer.value {
      case .let: return "let"
      case .inout: return "inout"
      case .set: return "set"
      case .sink: return "sink"
      }

    case ProductTypeDecl.self:
      return ProductTypeDecl.Typed(source)!.baseName

    case ParameterDecl.self:
      return ParameterDecl.Typed(source)!.identifier.value
    case VarDecl.self:
      return VarDecl.Typed(source)!.identifier.value

    default:
      fatalError("not implemented")
    }

  }

  // MARK: entry point function body

  /// Returns the transpiled body of the entry point function in `source`.
  func entryPointBody(module source: ModuleDecl.Typed) -> CXXScopedBlock? {
    return source.topLevelDecls.first(transformedBy: { (d) in
      if FunctionDecl.Typed(d)?.identifier?.value == "main" {
        return entryPointBody(mainFunction: FunctionDecl.Typed(d)!, in: source)
      } else {
        return nil
      }
    })
  }
  /// Returns the transpiled body of the entry point function calling `source`.
  private func entryPointBody(mainFunction source: FunctionDecl.Typed, in module: ModuleDecl.Typed)
    -> CXXScopedBlock
  {
    // The expression that makes a call to the module `main` function.
    let callToMain = CXXFunctionCallExpr(
      callee: CXXInfixExpr(
        oper: .scopeResolution,
        lhs: CXXIdentifier(module.baseName),
        rhs: CXXIdentifier("main")),
      arguments: [])

    // Foward function result if the result type is Int32.
    let resultType = (source.type.base as! LambdaType).output
    let forwardReturn = resultType == wholeValProgram.ast.coreType(named: "Int32")!

    // Build the body of the CXX entry-point function.
    var bodyContent: [CXXStmt] = []
    if forwardReturn {
      // Forward the result of the function as the exit code.
      bodyContent = [CXXReturnStmt(expr: callToMain)]
    } else {
      // Make a plain function call, discarding the result
      bodyContent = [CXXExprStmt(expr: CXXVoidCast(baseExpr: callToMain))]
    }
    return CXXScopedBlock(stmts: bodyContent)
  }

}

extension Sequence {

  /// The result of the first scucessful transformation applied to elements in `self`.
  public func first<T>(transformedBy transform: (Element) throws -> T?) rethrows -> T? {
    for x in self {
      if let y = try transform(x) { return y }
    }
    return nil
  }

}
