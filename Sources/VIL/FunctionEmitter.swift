import AST

/// A visitor that emits the VIL code of a function declaration.
final class FunctionEmitter: StmtVisitor, ExprVisitor {

  typealias StmtResult = Void
  typealias ExprResult = Value

  /// The top-level emitter.
  unowned let parent: Emitter

  /// The declaration of the function being emitted.
  let funDecl: BaseFunDecl

  /// The VIL builder used by the emitter.
  var builder: Builder { parent.builder }

  /// The context in which the function declaration is defined.
  var context: AST.Context { funDecl.type.context }

  /// A symbol table that locally visible declarations to their emitted value.
  ///
  /// This is populated by function parameters and pattern binding declarations.
  var locals: [ObjectIdentifier: Value] = [:]

  /// Creates a function emitter.
  ///
  /// - Parameters:
  ///   - parent: The top-level emitter.
  ///   - builder: The builder used to create new instructions.
  ///   - funDecl: The declaration of the function to emit. The initializer will fail if `funDecl`
  ///   is not type checked.
  init(parent: Emitter, funDecl: BaseFunDecl) {
    precondition(funDecl.state >= .typeChecked)
    self.parent = parent
    self.funDecl = funDecl
  }

  /// Emits the function.
  func emit() {
    // Create (i.e., declare) the function in the module.
    var mangler = Mangler()
    mangler.append(funDecl: funDecl)
    let name = mangler.finalize()
    let function = builder.getOrCreateFunction(name: name, type: funDecl.unappliedType as! FunType)

    // We're done if the function doesn't have body.
    guard let body = funDecl.body else { return }

    // Setup the local symbol table.
    var arguments = function.arguments[0...]
    if let selfDecl = funDecl.selfDecl {
      locals[ObjectIdentifier(selfDecl)] = arguments.first!
      arguments = arguments.dropFirst()
    }

    assert(funDecl.params.count == arguments.count)
    for (param, argument) in zip(funDecl.params, arguments) {
      locals[ObjectIdentifier(param)] = argument
    }

    // Emit the function's body.
    builder.block = function.createBasicBlock(arguments: function.arguments)
    visit(body)
  }

  func emit(localPBDecl node: PatternBindingDecl) {
    // Create the variable locations for each name in the pattern.
    let lvs = node.pattern.namedPatterns.map({ name in
      emit(localVarDecl: name.decl, type: name.type)
    })

    // Emit the initializer, if any.
    if let initializer = node.initializer {
      let rv = initializer.accept(self)

      // Emit a store right away if the pattern matches a single value.
      if node.pattern.singleVarDecl != nil {
        assert(lvs.count == 1)
        builder.buildStore(lvalue: lvs[0], rvalue: rv)
      } else {
        // FIXME: Handle destructuring,
        fatalError()
      }
    }
  }

  func emit(localVarDecl node: VarDecl, type: ValType) -> Value {
    guard node.state >= .typeChecked else {
      return ErrorValue(context: context)
    }
    precondition(node.hasStorage, "computed properties are not supported yet")

    // Allocate storage on the stack for the variable.
    let value = builder.buildAllocStack(type: node.type)
    locals[ObjectIdentifier(node)] = value
    return value
  }

  func visit(_ node: BraceStmt) {
    precondition(builder.block != nil, "not in a basic block")

    for i in 0 ..< node.stmts.count {
      switch node.stmts[i] {
      case let pdDecl as PatternBindingDecl:
        emit(localPBDecl: pdDecl)
        
      case let decl as Decl:
        parent.emit(decl: decl)

      case let stmt as Stmt:
        stmt.accept(self)

        // Discard everything after a return statement.
        if (stmt is RetStmt) && (i > node.stmts.count - 1) {
          context.report(.codeAfterReturnNeverExecuted(range: node.stmts[i + 1].range))
          break
        }

      case let expr as Expr:
        _ = expr.accept(self)

      default:
        fatalError("unreachable")
      }
    }
  }

  func visit(_ node: RetStmt) {
  }

  func visit(_ node: IntLiteralExpr) -> Value {
    if node.type is BuiltinIntType {
      return IntLiteralValue(type: node.type)
    }

    // FIXME: Handle other kinds of integer types.
    fatalError()
  }

  func visit(_ node: AssignExpr) -> Value {
    // Emit the left operand first.
    let lvalue = node.lvalue.accept(LValueEmitter(parent: self))
    let rvalue = node.rvalue.accept(self)
    builder.buildStore(lvalue: lvalue, rvalue: rvalue)
    return UnitValue(context: context)
  }

  func visit(_ node: TupleExpr) -> Value {
    return builder.buildTuple(
      type: node.type as! TupleType,
      elems: node.elems.map({ elem in elem.value.accept(self) }))
  }

  func visit(_ node: CallExpr) -> Value {
    fatalError()
  }

  func visit(_ node: UnresolvedDeclRefExpr) -> Value {
    fatalError()
  }

  func visit(_ node: UnresolvedMemberExpr) -> Value {
    fatalError()
  }

  func visit(_ node: UnresolvedQualDeclRefExpr) -> Value {
    fatalError()
  }

  func visit(_ node: OverloadedDeclRefExpr) -> Value {
    fatalError()
  }

  func visit(_ node: DeclRefExpr) -> Value {
    // If the expression refers to a variable, we have to emit its access.
    if let decl = node.decl as? VarDecl {
      precondition(decl.hasStorage)

      // If the variable declaration resides in a nominal type declaration, then the expression
      // must implicitly refers to a member declaration (i.e., without spelling `self`).
      if decl.parentDeclSpace is NominalTypeDecl {
        let selfDecl = funDecl.selfDecl!
        let selfLV = locals[ObjectIdentifier(selfDecl)]!
        return builder.buildRecordMemberAddr(record: selfLV, memberDecl: decl)
      }

      // The variable is local. Emit a load instruction.
      let lv = locals[ObjectIdentifier(node.decl)]!
      return builder.buildLoad(lvalue: lv)
    }

    // If the expression refers to a local function argument, just lookup the symbol table.
    if let decl = node.decl as? FunParamDecl {
      return locals[ObjectIdentifier(decl)]!
    }

    // FIXME: Handle global symbols.
    fatalError("unreachable")
  }

  func visit(_ node: TypeDeclRefExpr) -> Value {
    fatalError()
  }

  func visit(_ node: MemberRefExpr) -> Value {
    fatalError()
  }

  func visit(_ node: AddrOfExpr) -> Value {
    fatalError()
  }

  func visit(_ node: WildcardExpr) -> Value {
    fatalError()
  }

  func visit(_ node: ErrorExpr) -> Value {
    fatalError()
  }

}
