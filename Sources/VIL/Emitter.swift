import AST
import Basic

/// A VIL emitter.
///
/// This is the entry point to VIL code generation phase, which lowers a type checked module
/// declaration to a VIL module.
public final class Emitter: StmtVisitor, ExprVisitor {

  public typealias StmtResult = Void
  public typealias ExprResult = Value

  /// The context in which the pass runs.
  public let context: AST.Context

  /// The VIL builder used by the emitter.
  public let builder: Builder

  /// A stack of tables that map local symbols to their emitted value.
  private var locals: [ObjectIdentifier: Value] = [:]

  /// The declaration of `self` in the context of the member function being emitted.
  private var localSelfDecl: Value?

  public init(context: AST.Context, builder: Builder) {
    self.context = context
    self.builder = builder
  }

  public func emit(decl: Decl) {
    guard decl.state >= .typeChecked else { return }

    switch decl {
    case let typeDecl as NominalTypeDecl:
      // Nothing to emit for the type declaration itself, but we do have to emit members.
      for memberDecl in typeDecl.members {
        emit(decl: memberDecl)
      }

    case let pbDecl as PatternBindingDecl:
      for varDecl in pbDecl.varDecls {
        emit(varDecl: varDecl)
      }

    case let funDecl as BaseFunDecl:
      emit(funDecl: funDecl)

    default:
      fatalError("I don't know how to emit '\(decl)'")
    }
  }

  public func emit(varDecl: VarDecl) {
    guard varDecl.state >= .typeChecked else { return }
    precondition(varDecl.hasStorage, "computed properties are not supported yet")

    if varDecl.isMutable {
      let value = builder.buildAllocStack(type: varDecl.type)
      locals[ObjectIdentifier(varDecl)] = value
    } else {
      // FIXME: Handle non-addressable values.
      fatalError()
    }
  }

  public func emit(funDecl: BaseFunDecl) {
    guard funDecl.state >= .typeChecked else { return }

    // Create (i.e., declare) the function in the module.
    var mangler = Mangler()
    mangler.append(funDecl: funDecl)
    let name = mangler.finalize()
    let function = builder.getOrCreateFunction(name: name, type: funDecl.unappliedType as! FunType)

    // Emit the function's body.
    if let body = funDecl.body {
      // Setup the local symbol table.
      var arguments = function.arguments[0...]
      if funDecl.selfDecl != nil {
        localSelfDecl = arguments.first!
        arguments = arguments.dropFirst()
      }
      assert(funDecl.params.count == arguments.count)
      for (param, argument) in zip(funDecl.params, arguments) {
        locals[ObjectIdentifier(param)] = argument
      }

      // Emit the function.
      builder.block = function.createBasicBlock(arguments: function.arguments)
      body.accept(self)
    }

    print(function)
  }

  public func emit(stmt: Stmt) {
    stmt.accept(self)
  }

  public func emit(expr: Expr) -> Value {
    expr.accept(self)
  }

  public func visit(_ node: BraceStmt) {
    precondition(builder.block != nil, "not in a basic block")

    for i in 0 ..< node.stmts.count {
      switch node.stmts[i] {
      case let decl as Decl:
        emit(decl: decl)

      case let stmt as Stmt:
        emit(stmt: stmt)

        // Discard everything after a return statement.
        if (stmt is RetStmt) && (i > node.stmts.count - 1) {
          context.report(.codeAfterReturnNeverExecuted(range: node.stmts[i + 1].range))
          break
        }

      case let expr as Expr:
        _ = emit(expr: expr)

      default:
        fatalError("unreachable")
      }
    }
  }

  public func visit(_ node: RetStmt) {
  }

  public func visit(_ node: IntLiteralExpr) -> Value {
    if node.type is BuiltinIntType {
      return IntLiteralValue(type: node.type)
    }

    // FIXME: Handle other kinds of integer types.
    fatalError()
  }

  public func visit(_ node: AssignExpr) -> Value {
    // Emit the left operand first.
    let lvalue = node.lvalue.accept(self)
    let rvalue = node.rvalue.accept(self)
    builder.buildStore(lvalue: lvalue, rvalue: rvalue)
    return UnitValue(context: context)
  }

  public func visit(_ node: TupleExpr) -> Value {
    fatalError()
  }

  public func visit(_ node: CallExpr) -> Value {
    fatalError()
  }

  public func visit(_ node: UnresolvedDeclRefExpr) -> Value {
    fatalError()
  }

  public func visit(_ node: UnresolvedMemberExpr) -> Value {
    fatalError()
  }

  public func visit(_ node: UnresolvedQualDeclRefExpr) -> Value {
    fatalError()
  }

  public func visit(_ node: OverloadedDeclRefExpr) -> Value {
    fatalError()
  }

  public func visit(_ node: DeclRefExpr) -> Value {
    // If the expression refers to a variable, we have to emit its access.
    if let decl = node.decl as? VarDecl {
      precondition(decl.hasStorage)
      if decl.parentDeclSpace is NominalTypeDecl {
        return builder.buildRecordMemberAddr(record: localSelfDecl!, memberDecl: decl)
      }

      // FIXME: Emit a load statement.
      fatalError()
    }

    // If the expression refers to a local function argument, just lookup the symbol table.
    if node.decl is FunParamDecl {
      return locals[ObjectIdentifier(node.decl)]!
    }

    // FIXME: Handle global symbols.
    fatalError("unreachable")
  }

  public func visit(_ node: TypeDeclRefExpr) -> Value {
    fatalError()
  }

  public func visit(_ node: MemberRefExpr) -> Value {
    fatalError()
  }

  public func visit(_ node: AddrOfExpr) -> Value {
    fatalError()
  }

  public func visit(_ node: WildcardExpr) -> Value {
    fatalError()
  }

  public func visit(_ node: ErrorExpr) -> Value {
    fatalError()
  }

}
