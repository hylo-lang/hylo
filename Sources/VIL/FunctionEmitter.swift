import AST
import Basic

/// A visitor that emits the VIL code of a function declaration.
final class FunctionEmitter: StmtVisitor, ExprVisitor {

  typealias StmtResult = Void
  typealias ExprResult = Result<Value, EmitterError>

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
  ///     is not type checked.
  init(parent: Emitter, funDecl: BaseFunDecl) {
    precondition(funDecl.state >= .typeChecked)
    self.parent = parent
    self.funDecl = funDecl
  }

  /// Emits the function.
  func emit() {
    // Create (i.e., declare) the function in the module.
    let function = builder.getOrCreateFunction(from: funDecl)

    // We're done if the function doesn't have body.
    guard let body = funDecl.body else { return }

    // Create an entry block.
    builder.block = function.createBasicBlock(arguments: function.arguments)
    var arguments = function.arguments[0...]

    // Register the function's receiver in the local symbol table, if necessary.
    if funDecl.isMember {
      // Member functions accept their receiver has their first parameter.
      locals[ObjectIdentifier(funDecl.selfDecl!)] = function.arguments[0]
      arguments = arguments.dropFirst()
    } else if (funDecl is CtorDecl) {
      // Constructors should allocate `self`.
      let selfType = (funDecl.selfDecl!.type as! InoutType).base
      locals[ObjectIdentifier(funDecl.selfDecl!)] = builder.buildAllocStack(type: selfType)
    }

    // Register the function's formal parameters in the local symbol table..
    assert(funDecl.params.count == arguments.count)
    for (param, argument) in zip(funDecl.params, arguments) {
      locals[ObjectIdentifier(param)] = argument
    }

    // Emit the function's body.
    visit(body)

    // If the function's a constructor, emit the implicit return statement.
    if funDecl is CtorDecl {
      let selfLoc = locals[ObjectIdentifier(funDecl.selfDecl!)]
      let selfVal = builder.buildLoad(lvalue: selfLoc!)
      builder.buildRet(value: selfVal)
    }

    if !(builder.block?.instructions.last is RetInst) {
      if (funDecl.type as! FunType).retType == context.unitType {
        builder.buildRet(value: UnitValue(context: context))
      } else {
        let range = body.range.upperBound ..< body.range.upperBound
        context.report(.missingReturnValueInNonUnitFunction(range: range))
      }
    }
  }

  func emit(localPBDecl node: PatternBindingDecl) {
    // Create the variable locations for each name in the pattern.
    let lvs = node.pattern.namedPatterns.map({ name in
      emit(localVarDecl: name.decl, type: name.type)
    })

    // Emit the initializer, if any.
    if let initializer = node.initializer {
      let rv = emit(expr: initializer)

      // Emit a store right away if the pattern matches a single value.
      if let varDecl = node.pattern.singleVarDecl {
        assert(lvs.count == 1)

        // Check if the value to store must be packed into an existential container.
        // FIXME: Handle view composition.
        if let view = varDecl.type as? ViewType {
          let container = builder.buildPack(value: rv, interface: view)
          builder.buildStore(lvalue: lvs[0], rvalue: container)
        } else {
          builder.buildStore(lvalue: lvs[0], rvalue: rv)
        }
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

  func emit(expr node: Expr) -> Value {
    // Emit an error value for any expression that has an error type.
    guard node.type !== context.errorType else {
      return ErrorValue(context: context)
    }

    switch node.accept(self) {
    case .success(let value):
      return value

    case .failure(let error):
      print(error)
      return ErrorValue(context: context)
    }
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
    let value = node.value.map(emit(expr:)) ?? UnitValue(context: context)
    builder.buildRet(value: value)
  }

  func visit(_ node: IntLiteralExpr) -> ExprResult {
    // We can assume the expression's type conforms to `ExpressibleBy***Literal` (as type checking
    // succeeded). Therefore we can look for a conversion constructor `new(literal:)`.
    let view = context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral)!.instanceType as! ViewType

    let funref: Value
    if let type = node.type as? NominalType {
      // The node has a concrete type; we can dispatch `new(literal:)` statically.
      let conformance = type.decl.conformanceTable[view]!
      let function = builder.getOrCreateFunction(from: conformance.entries[0].impl as! CtorDecl)
      funref = FunRef(function: function)
    } else if let type = node.type as? SkolemType {
      // The node has an skolem type; we have to dispatch dynamically.
      funref = builder.buildWitnessFun(
        base: type,
        decl: view.decl.valueMemberTable["new"]![0] as! CtorDecl)
    } else {
      fatalError("unreachable")
    }

    // Emit a call to the constructor.
    let literal = IntLiteralValue(value: node.value, context: context)
    return .success(builder.buildApply(fun: funref, args: [literal]))
  }

  func visit(_ node: AssignExpr) -> ExprResult {
    // Emit the left operand first.
    switch node.lvalue.accept(LValueEmitter(parent: self)) {
    case .success(let lvalue):
      let rvalue = emit(expr: node.rvalue)
      builder.buildStore(lvalue: lvalue, rvalue: rvalue)

    case .failure(let error):
      // Diagnostic common l-value errors.
      switch error {
      case .immutableSelf:
        context.report(.cannotAssignImmutableSelf(range: node.lvalue.range))
      case .immutableLocation:
        context.report(.cannotAssignToImmutableLocation(range: node.lvalue.range))
      }

      return .success(UnitValue(context: context))
    }

    // Assignments always result in a unit value.
    return .success(UnitValue(context: context))
  }

  func visit(_ node: TupleExpr) -> ExprResult {
    let tuple = builder.buildTuple(
      type: node.type as! TupleType,
      elems: node.elems.map({ elem in emit(expr: elem.value) }))
    return .success(tuple)
  }

  func visit(_ node: CallExpr) -> ExprResult {
    let funref: Value
    var args: [Value] = []

    // Emit the function's callee.
    switch node.fun {
    case let memberRef as MemberRefExpr where memberRef.decl.isMember:
      if let methodDecl = memberRef.decl as? BaseFunDecl {
        // This is a call `foo.bar(x: 0, y: 1)`, where `bar` is a method. We have to determine
        // whether it should be dispatched statically or dynamically.
        if memberRef.base.type is ViewType {
          // The receiver is an existential container; dispatch dynamically.
          funref = builder.buildWitnessFun(base: memberRef.base.type, decl: methodDecl)
        } else {
          // The receiver is a concrete type; dispatch statically.
          funref = FunRef(function: builder.getOrCreateFunction(from: methodDecl))
        }

        // Since the call applies a method, we have to pass the receiver as an argument.
        args.append(emit(expr: memberRef.base))
      } else {
        fatalError()
      }

    default:
      // Emit the callee "as is"
      funref = emit(expr: node.fun)
    }

    // Emit the function's arguments.
    for arg in node.args {
      args.append(emit(expr: arg.value))
    }

    return .success(builder.buildApply(fun: funref, args: args))
  }

  func visit(_ node: UnresolvedDeclRefExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: UnresolvedMemberExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: UnresolvedQualDeclRefExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: OverloadedDeclRefExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: DeclRefExpr) -> ExprResult {

    // FIXME: Implicit references to `self` should be desugared during the sema, so we no longer
    // have to deal with this every time we emit a resolvable.

    // FIXME: We need a better, more reliable way to easily determine whether the node requires
    // l-value to r-value conversion.

    switch node.decl {
    case let decl as FunDecl where decl.props.contains(.isBuiltin):
      // Emit a regular function.
      return .success(BuiltinFunRef(decl: decl))

    case let decl as CtorDecl:
      // Emit a constructor.
      let function = builder.getOrCreateFunction(from: decl)
      return .success(FunRef(function: function))

    case let decl as FunParamDecl where !(decl.type is InoutType):
      // Emit a parameter.
      let rv = locals[ObjectIdentifier(decl)]!
      return .success(rv)

    default:
      break
    }

    // Emit a l-value and convert it to an r-value.
    switch node.accept(LValueEmitter(parent: self)) {
    case .success(let lvalue):
      return .success(builder.buildLoad(lvalue: lvalue))
    case let failure:
      return failure
    }
  }

  func visit(_ node: TypeDeclRefExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: MemberRefExpr) -> ExprResult {
    // Emit the base value.
    let base: Value
    switch node.base.accept(self) {
    case .success(let b):
      base = b
    case let failure:
      return failure
    }

    switch node.base.type {
    case is ProductType:
      if let decl = node.decl as? VarDecl {
        if decl.hasStorage {
          return .success(builder.buildRecordMember(record: base, memberDecl: decl))
        }
      }

    default:
      break
    }

    fatalError()
  }

  func visit(_ node: AddrOfExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: WildcardExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: ErrorExpr) -> ExprResult {
    fatalError()
  }

}
