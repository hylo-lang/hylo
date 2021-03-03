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
    guard (funDecl.body != nil) || funDecl.isSynthesized else { return }

    // Contextualize the function's arguments.
    let genericEnv = funDecl.genericEnv!
    var args = function.type.paramTypes.map({ type -> Value in
      return ArgumentValue(
        type: type.contextualized(in: genericEnv, from: funDecl), function: function)
    })

    // Create an entry block.
    builder.block = function.createBasicBlock(arguments: args)

    // Register the function's receiver in the local symbol table, if necessary.
    if let selfDecl = funDecl.selfDecl {
      var selfType = genericEnv.contextualize(selfDecl.type, from: funDecl)
      if funDecl.isMember {
        // Member functions accept their receiver an implicit parameter.
        locals[ObjectIdentifier(funDecl.selfDecl!)] = args[0]
        args.removeFirst()
      } else {
        // Constructors should allocate `self`.
        assert(funDecl is CtorDecl)
        selfType = (selfType as! InoutType).base
        locals[ObjectIdentifier(funDecl.selfDecl!)] = builder.buildAllocStack(
          type: .lower(selfType))
      }
    }

    // Register the function's formal parameters in the local symbol table.
    assert(funDecl.params.count == args.count)
    for (param, arg) in zip(funDecl.params, args) {
      locals[ObjectIdentifier(param)] = arg
    }

    // Emit the function's body.
    guard let body = funDecl.body else {
      return emitSynthesizedBody()
    }

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

  func emitSynthesizedBody() {
    assert(funDecl.isSynthesized)

    switch funDecl.name {
    case "new":
      // Emit a synthesized constructor.
      let base = locals[ObjectIdentifier(funDecl.selfDecl!)]!
      let type = funDecl.parentDeclSpace as! NominalTypeDecl

      for (varDecl, paramDecl) in zip(type.storedVars, funDecl.params) {
        let memberAddr = builder.buildRecordMemberAddr(
          record: base, memberDecl: varDecl, type: VILType.lower(varDecl.type).address)
        let value = locals[ObjectIdentifier(paramDecl)]!
        builder.buildStore(lvalue: memberAddr, rvalue: value)
      }

      let selfVal = builder.buildLoad(lvalue: base)
      builder.buildRet(value: selfVal)

    default:
      preconditionFailure("unexpected synthesized declaration '\(funDecl.name)'")
    }
  }

  func emit(localPBDecl node: PatternBindingDecl) {
    // Create the variable locations for each name in the pattern.
    let lvalues = node.pattern.namedPatterns.map({ name in
      emit(localVarDecl: name.decl, type: name.type)
    })

    // Emit the initializer, if any.
    if let initializer = node.initializer {
      // Emit a store right away if the pattern matches a single value.
      if let varDecl = node.pattern.singleVarDecl {
        assert((lvalues.count == 1) && (lvalues[0] === locals[ObjectIdentifier(varDecl)]))
        emit(assign: initializer, to: lvalues[0])
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
    let value = builder.buildAllocStack(type: .lower(node.type))
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
      // FIXME: This should be reported.
      print(error)
      return ErrorValue(context: context)
    }
  }

  func emit(assign expr: Expr, to lvalue: Value) {
    if lvalue.type.valType.isExistential {
      // The l-value is an existential container.
      if expr.type.isExistential {
        // Both operands are existential containers. Hence, the goal is to copy the existential
        // package from one container to the other. If the right operand can be treated as an
        // l-value, then this boils down to a mere `copy_addr`.
        if case .success(var rvalue) = expr.accept(LValueEmitter(parent: self)) {
          if lvalue.type.valType != expr.type {
            // The r-value has a different type as the l-value; we need a cast.
            rvalue = builder.buildUnsafeCastAddr(source: rvalue, type: lvalue.type)
          }
          builder.buildCopyAddr(dest: lvalue, source: rvalue)
        } else {
          // The right operand is a true r-value (e.g., the result of a cast on a r-value).
          var rvalue = emit(expr: expr)
          if lvalue.type.valType == expr.type {
            // The r-value has the same type as the l-value; we only need a store.
            builder.buildStore(lvalue: lvalue, rvalue: rvalue)
          } else {
            // The r-value has a different type; we need a temporary location to cast its package.
            let tmp: Value = builder.buildAllocStack(type: rvalue.type)
            builder.buildStore(lvalue: tmp, rvalue: rvalue)
            rvalue = builder.buildUnsafeCastAddr(source: tmp, type: lvalue.type)
            builder.buildCopyAddr(dest: lvalue, source: rvalue)
          }
        }
      } else {
        // The r-value is concrete; it has to be packed unto the l-value.
        let rvalue = emit(expr: expr)
        let lvalue = builder.buildAllocExistential(container: lvalue, witness: rvalue.type)
        builder.buildStore(lvalue: lvalue, rvalue: rvalue)
      }
    } else {
      // The l-value is concrete.
      assert(!expr.type.isExistential)
      let rvalue = emit(expr: expr)
      builder.buildStore(lvalue: lvalue, rvalue: rvalue)
    }
  }

  func emit(lvalue node: Expr) -> Value {
    // Emit an error value for any expression that has an error type.
    guard node.type !== context.errorType else {
      return ErrorValue(context: context)
    }

    switch node.accept(LValueEmitter(parent: self)) {
    case .success(let value):
      return value

    case .failure(let error):
      // FIXME: This should be reported.
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
    } else if node.type is SkolemType {
      // The node has an skolem type; we have to dispatch dynamically.
      fatalError("not implemented")
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
      emit(assign: node.rvalue, to: lvalue)

    case .failure(let error):
      // Diagnostic common l-value errors.
      switch error {
      case .immutableSelf:
        context.report(.cannotAssignImmutableSelf(range: node.lvalue.range))
      case .immutableLocation:
        context.report(.cannotAssignToImmutableLocation(range: node.lvalue.range))
      case .immutableExpr:
        context.report(.cannotAssignImmutableExpr(range: node.lvalue.range))
      }

      return .success(UnitValue(context: context))
    }

    // Assignments always result in a unit value.
    return .success(UnitValue(context: context))
  }

  func visit(_ node: UnsafeCastExpr) -> Result<Value, EmitterError> {
    // Check for trivial casts.
    guard node.type != node.value.type else {
      context.report(.unsafeCastToSameTimeHasNoEffect(type: node.type, range: node.range))
      return node.value.accept(self)
    }

    // If the target type is existential, store the node's value into a new container.
    if node.type.isExistential {
      let container = builder.buildAllocStack(type: .lower(node.type))
      emit(assign: node.value, to: container)
      return .success(builder.buildLoad(lvalue: container))
    }

    if node.value.type.isExistential {
      let container = emit(expr: node.value)
      let value = builder.buildOpenExistential(container: container, type: .lower(node.type))
      return.success(value)
    }

    // FIXME: Implement "structural cast".
    fatalError("not implemented")
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
    case let memberRef as MemberDeclRefExpr where memberRef.decl.isMember:
      if let methodDecl = memberRef.decl as? BaseFunDecl {
        // This is a call `foo.bar(x: 0, y: 1)`, where `bar` is a method and `foo` its receiver.
        let receiver = methodDecl.isMutating
          ? emit(lvalue: memberRef.base)
          : emit(expr: memberRef.base)
        args.append(receiver)

        // We have to determine whether it should be dispatched statically or dynamically.
        if memberRef.base.type is ViewType {
          // The receiver is an existential container; dispatch dynamically.
          funref = builder.buildWitnessMethod(container: receiver, decl: methodDecl)
        } else {
          // The receiver is a concrete type; dispatch statically.
          funref = FunRef(function: builder.getOrCreateFunction(from: methodDecl))
        }
      } else {
        fatalError()
      }

    default:
      // Emit the callee "as is"
      funref = emit(expr: node.fun)
    }

    // Emit the function's arguments.
    let fType = funref.type as! VILFunType
    for i in 0 ..< node.args.count {
      var value = emit(expr: node.args[i].value)

      // Check if the argument needs to be prepared before being passed.
      if fType.paramTypes[i].isExistential || (fType.paramConvs[i] == .exist) {
        // The parameter has an existential type, or its convention prescribes that it should be
        // passed as an existential container. If the argument isn't packaged as an existential,
        // then we need to wrap it into a container. Otherwise, we can pass it "as is", since all
        // existential containers have the same memory layout.
        if !value.type.isExistential {
          let tmp = builder.buildAllocStack(type: .lower(context.anyType))
          let ptr = builder.buildAllocExistential(container: tmp, witness: value.type)
          builder.buildStore(lvalue: ptr, rvalue: value)
          value = builder.buildLoad(lvalue: tmp)

          // FIXME: Allocating a container of type `Any`, completely erases the argument's type.
          // Hence, the resulting VIL is in fact ill-typed, since could call a function expecting
          // an argument of type `T` with a value of type `Any`. A better strategy would be to
          // create an existential container that meets the parameter's requirements.
        }
      }

      args.append(value)
    }

    // Emit the call.
    var value: Value = builder.buildApply(fun: funref, args: args)

    // Emit the extraction of the result value.
    if fType.retConv == .exist {
      value = builder.buildOpenExistential(container: value, type: .lower(node.type))
    }

    // FIXME: Deallocate the memory that we allocated for indirect parameters.
    return .success(value)
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
    case let decl as FunDecl where decl.isBuiltin:
      // Emit a built-in function.
      return .success(BuiltinFunRef(decl: decl))

    case let decl as BaseFunDecl:
      // Emit a function reference.
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

  func visit(_ node: MemberDeclRefExpr) -> ExprResult {
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
          let member = builder.buildRecordMember(
            record: base, memberDecl: decl, type: .lower(node.type))
          return .success(member)
        }
      }

    default:
      break
    }

    fatalError()
  }

  func visit(_ node: TupleMemberExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: AsyncExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: AwaitExpr) -> ExprResult {
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
