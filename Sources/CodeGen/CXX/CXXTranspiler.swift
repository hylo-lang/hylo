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
        return cxx(typeExpr: valDeclType.output, asReturnType: true)

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
      let type = cxx(typeExpr: paramTypes[i].type)
      cxxParams.append(CXXFunctionDecl.Parameter(name, type))
    }
    return cxxParams
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
  private func cxx(type src: ProductTypeDecl.Typed) -> CXXClassDecl {
    assert(wholeValProgram.isGlobal(src.id))
    return CXXClassDecl(
      name: CXXIdentifier(src.identifier.value),
      members: cxx(classMembers: src.members))
  }

  /// Transpiles product type members to C++.
  private func cxx<C: Collection>(classMembers src: C) -> [CXXClassDecl.ClassMember]
  where C.Element == AnyDeclID.TypedNode {
    var cxxMembers: [CXXClassDecl.ClassMember] = []
    for member in src {
      switch member.kind {
      case BindingDecl.self:
        // One binding can expand into multiple class attributes
        cxx(productTypeBinding: BindingDecl.Typed(member)!, into: &cxxMembers)

      case InitializerDecl.self:
        cxxMembers.append(cxx(productTypeInitialzer: InitializerDecl.Typed(member)!))

      case MethodDecl.self, FunctionDecl.self:
        cxxMembers.append(.method)

      default:
        unreachable("unexpected class member")
      }
    }
    return cxxMembers
  }

  /// Translate a binding decl contained in a product type into one or more C++ class attributes.
  /// One class attribute will be generated for each name in the binding decl.
  /// The resulting class members are appended to `res` (the array of class members).
  private func cxx(
    productTypeBinding src: BindingDecl.Typed, into res: inout [CXXClassDecl.ClassMember]
  ) {
    // TODO: pattern introducer (let, var, sink, inout)
    for (_, name) in src.pattern.subpattern.names {
      let varDecl = name.decl
      let cxxAttribute = CXXClassAttribute(
        type: cxx(typeExpr: varDecl.type),
        name: CXXIdentifier(varDecl.name),
        initializer: nil,  // TODO
        isStatic: src.isStatic)
      res.append(.attribute(cxxAttribute))
    }
  }
  /// Translate an initializer found in product type to the corresponding C++ constructor.
  private func cxx(productTypeInitialzer src: InitializerDecl.Typed) -> CXXClassDecl.ClassMember {
    switch src.introducer.value {
    case .`init`:
      // TODO: emit constructor
      return .constructor
    case .memberwiseInit:
      // TODO: emit constructor
      return .constructor
    }
  }

  /// Transpiles a local binding declaration to a C++ statement containing a local variable.
  /// The statement can be a `CXXLocalVarDecl` or a `CXXScopedBlock` containing `CXXLocalVarDecl` objects.
  private func cxx(localBinding src: BindingDecl.Typed) -> CXXStmt {
    let cxxInitializer = src.initializer != nil ? cxx(expr: src.initializer!) : nil
    let numNames = src.pattern.subpattern.names.count

    assert(numNames > 0 || cxxInitializer != nil)

    if numNames > 1 {
      // We would output multiple variable declaration statements, gtoupped into a scoped block
      // TODO: scoped block is not good here, because of lifetime issues
      var varStmts: [CXXStmt] = []
      for (_, name) in src.pattern.subpattern.names {
        varStmts.append(cxx(localNamePattern: name, with: cxxInitializer))
      }
      return CXXScopedBlock(stmts: varStmts)
    } else if numNames == 1 {
      return cxx(localNamePattern: src.pattern.subpattern.names[0].pattern, with: cxxInitializer)
    } else if numNames == 0 {
      // No pattern found; just call the initializer, dropping the result.
      let cxxExpr = CXXVoidCast(baseExpr: cxxInitializer!)
      return CXXExprStmt(expr: cxxExpr)
    } else {
      unreachable()
    }
  }
  /// Transpile to a C++ local variable the Val name pattern, using a C++ expression as intializer.
  private func cxx(localNamePattern src: NamePattern.Typed, with cxxInitializer: CXXExpr?)
    -> CXXLocalVarDecl
  {
    CXXLocalVarDecl(
      type: cxx(typeExpr: src.decl.type),
      name: CXXIdentifier(src.decl.identifier.value),
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
  private func cxx(brace src: BraceStmt.Typed) -> CXXScopedBlock {
    return CXXScopedBlock(stmts: Array(src.stmts.map({ cxx(stmt: $0) })))
  }

  /// Transpiles a Val declaration statement into the corresponding C++ local decl.
  private func cxx(declStmt src: DeclStmt.Typed) -> CXXStmt {
    switch src.decl.kind {
    case BindingDecl.self:
      return cxx(localBinding: BindingDecl.Typed(src.decl)!)
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Transpiles a Val expression statement into the corresponding C++ expression statement.
  private func cxx(exprStmt src: ExprStmt.Typed) -> CXXStmt {
    return CXXExprStmt(expr: cxx(expr: src.expr))
  }

  /// Transpiles a Val assignment statement into an C++ statement containing an assign expression.
  private func cxx(assignStmt src: AssignStmt.Typed) -> CXXExprStmt {
    let cxxExpr = CXXInfixExpr(
      oper: .assignment,
      lhs: cxx(expr: src.left),
      rhs: cxx(expr: src.right))
    return CXXExprStmt(expr: cxxExpr)
  }

  /// Transpiles a Val return statement into the corresponding C++ return statement.
  private func cxx(returnStmt src: ReturnStmt.Typed) -> CXXReturnStmt {
    let returnValue = src.value != nil ? cxx(expr: src.value!) : nil
    return CXXReturnStmt(expr: returnValue)
  }

  /// Transpiles a Val while statement into the corresponding C++ while statement.
  private func cxx(whileStmt src: WhileStmt.Typed) -> CXXWhileStmt {
    // TODO: multiple conditions
    // TODO: bindings in conditions
    let condition: CXXExpr
    if src.condition.count == 1 {
      switch src.condition[0] {
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
      condition: condition, body: cxx(stmt: src.body))
  }
  /// Transpiles a Val do-while statement into the corresponding C++ do-while statement.
  private func cxx(doWhileStmt src: DoWhileStmt.Typed) -> CXXDoWhileStmt {
    return CXXDoWhileStmt(
      body: cxx(stmt: src.body),
      condition: cxx(expr: src.condition))
  }
  /// Transpiles a Val for statement into the corresponding C++ for statement.
  private func cxx(forStmt src: ForStmt.Typed) -> CXXStmt {
    return CXXComment(comment: "ForStmt")
  }
  /// Transpiles a Val break statement into a C++ break statement.
  private func cxx(breakStmt src: BreakStmt.Typed) -> CXXBreakStmt {
    return CXXBreakStmt()
  }
  /// Transpiles a Val continue statement into a C++ continue statement.
  private func cxx(continueStmt src: ContinueStmt.Typed) -> CXXContinueStmt {
    return CXXContinueStmt()
  }
  /// Transpiles a Val yield statement into a corresponding C++ statement.
  private func cxx(yieldStmt src: YieldStmt.Typed) -> CXXStmt {
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
  private func cxx(booleanLiteral src: BooleanLiteralExpr.Typed) -> CXXBooleanLiteralExpr {
    return CXXBooleanLiteralExpr(value: src.value)
  }

  /// Transpiles a Val integer literal into a corresponding C++ literal.
  private func cxx(integerLiteral src: IntegerLiteralExpr.Typed) -> CXXExpr {
    return CXXIntegerLiteralExpr(value: src.value)
  }

  /// Transpiles a Val sequence expression into a corresponding C++ expression.
  private func cxx(sequence src: SequenceExpr.Typed) -> CXXExpr {
    return cxx(foldedSequence: src.foldedSequenceExprs!)
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
    infix src: NameExpr.Typed,
    lhs: CXXExpr,
    rhs: CXXExpr
  ) -> CXXExpr {
    let name = nameOfDecl(src.decl.decl)
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
      let callee = cxx(name: src)
      let arguments = [lhs, rhs]
      return CXXFunctionCallExpr(callee: callee, arguments: arguments)
    }
  }

  /// Transpiles a Val function call into a corresponding C++ function call.
  private func cxx(
    functionCall src: FunctionCallExpr.Typed
  ) -> CXXExpr {
    // Transpile the arguments
    // TODO: passing conventions
    let arguments = Array(src.arguments.map({ cxx(expr: wholeValProgram[$0.value]) }))

    // Transpile the calee expression
    let callee: CXXExpr
    if let calleeNameExpr = NameExpr.Typed(src.callee) {
      // If the callee is a name expression, we might need the callee type when expanding the name.
      callee = cxx(name: calleeNameExpr)

      // The name expresssion might fully represent an prefix/postfix operator call;
      // in this case, we don't need to wrap this into a function call
      if callee is CXXPrefixExpr || callee is CXXPostfixExpr {
        assert(arguments.isEmpty)
        return callee
      }
    } else {
      callee = cxx(expr: src.callee)
    }

    return CXXFunctionCallExpr(callee: callee, arguments: arguments)
  }

  /// Transpiles a Val conditional expression into a corresponding C++ expression.
  /// As much as possible, this will be converted into a ternary operator (CXXConditionalExpr).
  /// There are, however, in wich this needs to be translated into an if statment, then trasformed into an expression.
  private func cxx(cond src: CondExpr.Typed) -> CXXExpr {
    // TODO: multiple conditions
    // TODO: bindings in conditions
    let condition: CXXExpr
    if src.condition.count == 1 {
      switch src.condition[0] {
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
    if src.type != .void {
      // We result in an expression
      // TODO: do we need to return an l-value?
      let trueExpr = cxx(condBodyExpr: src.success)
      let falseExpr = cxx(condBodyExpr: src.failure)
      return CXXConditionalExpr(condition: condition, trueExpr: trueExpr, falseExpr: falseExpr)
    } else {
      // We result in a statement, and we wrap the statement into an expression
      let trueStmt = cxx(condBodyStmt: src.success)!
      let falseStmt = cxx(condBodyStmt: src.failure)
      return CXXStmtExpr(
        stmt: CXXIfStmt(condition: condition, trueStmt: trueStmt, falseStmt: falseStmt))
    }
  }
  /// Transates a conditional expression body into the corresponding C++ expression
  private func cxx(condBodyExpr src: CondExpr.Body?) -> CXXExpr {
    switch src {
    case .expr(let altExpr):
      return cxx(expr: wholeValProgram[altExpr])
    case .block:
      fatalError("not implemented")
    case .none:
      return CXXComment(comment: "missing alternative")
    }
  }
  /// Transates a conditional expression body into the corresponding C++ statement
  private func cxx(condBodyStmt src: CondExpr.Body?) -> CXXStmt? {
    switch src {
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
  /// This usually result into a C++ identifier, but it can also result in pre-/post-fix operator calls.
  private func cxx(name src: NameExpr.Typed) -> CXXExpr {
    switch src.decl {
    case .direct(let calleeDecl) where calleeDecl.kind == BuiltinDecl.self:
      return cxx(nameOfBuiltin: src)
    case .direct(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
      return cxx(
        nameOfFunction: FunctionDecl.Typed(calleeDecl)!, withDomainExpr: src.domainExpr)
    case .direct(let calleeDecl) where calleeDecl.kind == InitializerDecl.self:
      return cxx(nameOfInitializer: InitializerDecl.Typed(calleeDecl)!)
    case .direct(let calleeDecl):
      // For variables, and other declarations, just use the name of the declaration
      return CXXIdentifier(nameOfDecl(calleeDecl))

    case .member(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
      return cxx(nameOfMemberFunction: FunctionDecl.Typed(calleeDecl)!, withDomain: src.domain)

    case .member(_):
      fatalError("not implemented")
    }
  }
  /// Translates a name pointing to a builin to a C++ expression.
  private func cxx(nameOfBuiltin src: NameExpr.Typed) -> CXXExpr {
    return CXXIdentifier(src.name.value.stem)
  }
  /// Translates a name pointing to a fumction declaration to a C++ expression.
  /// Usually this translates to an identifier, but it can translate to pre-/post-fix operator calls.
  private func cxx(
    nameOfFunction src: FunctionDecl.Typed, withDomainExpr domainExpr: AnyExprID.TypedNode?
  ) -> CXXExpr {
    // Check for prefix && postfix operator calls
    if src.notation != nil && src.notation!.value == .prefix {
      let prefixOperators: [String: CXXPrefixExpr.Operator] = [
        "++": .prefixIncrement,
        "--": .prefixDecrement,
        "+": .unaryPlus,
        "-": .unaryMinus,
        "!": .logicalNot,
      ]
      if let cxxPrefixOperator = prefixOperators[nameOfDecl(src)] {
        return CXXPrefixExpr(
          oper: cxxPrefixOperator, base: cxx(expr: domainExpr!))
      }
    } else if src.notation != nil && src.notation!.value == .postfix {
      let postfixOperators: [String: CXXPostfixExpr.Operator] = [
        "++": .suffixIncrement,
        "--": .suffixDecrement,
      ]
      if let cxxPostfixOperator = postfixOperators[nameOfDecl(src)] {
        return CXXPostfixExpr(
          oper: cxxPostfixOperator, base: cxx(expr: domainExpr!))
      }
    }

    // TODO: handle captures
    return CXXIdentifier(nameOfDecl(src))
  }
  /// Translates a name pointing to an initializer to a C++ expression.
  private func cxx(nameOfInitializer src: InitializerDecl.Typed) -> CXXExpr {
    fatalError("not implemented")
  }
  /// Translates a name pointing to a member function to a C++ expression.
  private func cxx(
    nameOfMemberFunction src: FunctionDecl.Typed, withDomain domain: NameExpr.Typed.Domain
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
    return CXXInfixExpr(oper: .dotAccess, lhs: receiver, rhs: CXXIdentifier(nameOfDecl(src)))
  }

  // MARK: miscelaneous

  /// Transates a Val type into a C++ type expression.
  private func cxx(typeExpr src: AnyType, asReturnType isReturnType: Bool = false) -> CXXTypeExpr {
    switch src.base {
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
