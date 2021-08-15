import AST
import Basic

/// A VIL emitter for statements and r-values.
struct RValueEmitter: ExprVisitor {

  typealias ExprResult = Result<Value, EmitterError>

  /// The declaration of the function being emitted.
  let funDecl: BaseFunDecl

  /// A symbol table that maps locally visible declarations to their emitted value, populated by
  /// function parameters and local pattern binding declarations.
  var locals: SymbolTable

  /// The VIL builder used by the emitter.
  let builder: Builder

  /// The context in which the function declaration is defined.
  var context: AST.Context { funDecl.type.context }

  /// Emits the assignment of `rvalue` to `lvalue`.
  func emit(assign rvalue: Value, ofType rvalueType: ValType, to lvalue: Value) {
    assert(rvalueType.isCanonical)

    // If the l-value has the same type as the r-value, we can emit a simple store.
    if lvalue.type.valType == rvalueType {
      builder.buildStore(lvalue: lvalue, rvalue: rvalue)
      return
    }

    // If both the l-value and the r-value have a union type, we can emit a simple store.
    if (rvalue.type.valType is UnionType) && (rvalueType is UnionType) {
      builder.buildStore(lvalue: lvalue, rvalue: rvalue)
      return
    }

    // If the l-value is an existential container, then there are two cases to consider.
    if lvalue.type.valType.isExistential {
      assert(lvalue.type.valType != rvalueType)

      if rvalueType.isExistential {
        // If the r-value is an existential container too (but with a different type), then we need
        // to cast its package.
        var tmp: Value = builder.buildAllocStack(type: rvalue.type)
        builder.buildStore(lvalue: tmp, rvalue: rvalue)
        tmp = builder.buildUnsafeCastAddr(source: tmp, type: lvalue.type)
        builder.buildCopyAddr(dest: lvalue, source: tmp)
      } else {
        // If the r-value has a concrete type, then it must be packed into an existential container
        // before being stored
        let lvalue = builder.buildAllocExistential(container: lvalue, witness: rvalue.type)
        builder.buildStore(lvalue: lvalue, rvalue: rvalue)
      }

      return
    }

    builder.buildStore(lvalue: lvalue, rvalue: rvalue)
  }

  /// Emits the assignment of the value represented by `expr` to `lvalue`.
  mutating func emit(assign expr: Expr, to lvalue: Value) {
    // If both operands have an existential layout and the r-value can be treated as a location,
    // then we may simply emit `copy_addr`.
    if lvalue.type.valType.isExistential && expr.type.isExistential {
      var emitter = LValueEmitter(funDecl: funDecl, locals: locals, builder: builder)
      if case .success(var rvalue) = expr.accept(&emitter) {
        // If the r-value has a different type than the l-value, it must be casted.
        if lvalue.type.valType != expr.type {
          rvalue = builder.buildUnsafeCastAddr(source: rvalue, type: lvalue.type)
        }

        /// Copies the contents from the location on the right to the location on the left.
        builder.buildCopyAddr(dest: lvalue, source: rvalue)
        return
      }
    }

    // Otherwise, emit the r-value and fall back to the regular path.
    let rvalue = emit(rvalue: expr)
    emit(assign: rvalue, ofType: expr.type.dealiased, to: lvalue)
  }

  /// Emits a r-value.
  mutating func emit(rvalue node: Expr) -> Value {
    // Emit an error value for any expression that has an error type.
    guard node.type !== context.errorType else {
      return ErrorValue(context: context)
    }

    switch node.accept(&self) {
    case .success(let value):
      return value

    case .failure(let error):
      // FIXME: This should be reported.
      print(error)
      return ErrorValue(context: context)
    }
  }

  /// Emits an l-value.
  func emit(lvalue node: Expr) -> Value {
    // Emit an error value for any expression that has an error type.
    guard node.type !== context.errorType else {
      return ErrorValue(context: context)
    }

    var emitter = LValueEmitter(funDecl: funDecl, locals: locals, builder: builder)
    switch node.accept(&emitter) {
    case .success(let value):
      return value

    case .failure(let error):
      // FIXME: This should be reported.
      print(error)
      return ErrorValue(context: context)
    }
  }

  /// Emits a nil value.
  func emitNil() -> Value {
    let decl = context.getTypeDecl(for: .Nil) as! ProductTypeDecl
    return builder.buildRecord(typeDecl: decl, type: .lower(decl.instanceType))
  }

  // ----------------------------------------------------------------------------------------------
  // MARK: Visitors
  // ----------------------------------------------------------------------------------------------

  func visit(_ node: BoolLiteralExpr) -> Result<Value, EmitterError> {
    fatalError("not implemented")
  }

  func visit(_ node: IntLiteralExpr) -> ExprResult {
    // We can assume the expression's type conforms to `ExpressibleBy***Literal` (as type checking
    // succeeded). Therefore we can look for a conversion constructor `new(literal:)`.
    let view = context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral)!.instanceType as! ViewType
    let funref: Value

    var type = node.type.dealiased.canonical
    if let inoutType = type as? InoutType {
      type = inoutType.base
    }

    if let type = type as? NominalType {
      // The node has a concrete type; we can dispatch `new(literal:)` statically.
      let conformance = type.decl.conformanceTable[view]!
      let function = builder.getOrCreateFunction(from: conformance.entries[0].impl as! CtorDecl)
      funref = FunRef(function: function)
    } else if type is SkolemType {
      // The node has an skolem type; we have to dispatch dynamically.
      fatalError("not implemented")
    } else {
      fatalError("unreachable")
    }

    // Emit a call to the constructor.
    let literal = IntLiteralValue(value: node.value, context: context)
    return .success(builder.buildApply(fun: funref, args: [literal]))
  }

  func visit(_ node: FloatLiteralExpr) -> Result<Value, EmitterError> {
    fatalError("not implemented")
  }

  func visit(_ node: StringLiteralExpr) -> Result<Value, EmitterError> {
    fatalError("not implemented")
  }

  mutating func visit(_ node: AssignExpr) -> ExprResult {
    // Emit the left operand first.
    var emitter = LValueEmitter(funDecl: funDecl, locals: locals, builder: builder)
    switch node.lvalue.accept(&emitter) {
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

  func visit(_ node: BaseCastExpr) -> Result<Value, EmitterError> {
    fatalError("unreachable")
  }

  mutating func visit(_ node: DynCastExpr) -> Result<Value, EmitterError> {
    // The type of the node should be `Maybe<T>`.
    guard let nodeType = node.type as? BoundGenericType else {
      preconditionFailure("dynamic cast should have a maybe type")
    }
    let targetType = nodeType.args[0]
    let vilTargetType = VILType.lower(targetType)

    // Allocate space for the result of the cast.
    let result = builder.buildAllocStack(type: .lower(node.type))

    let source: Value
    var emitter = LValueEmitter(funDecl: funDecl, locals: locals, builder: builder)
    if case .success(let s) = node.value.accept(&emitter) {
      source = s
    } else {
      source = builder.buildAllocStack(type: .lower(node.type))
      builder.buildStore(lvalue: source, rvalue: emit(rvalue: node.value))
    }

    // Cast the value.
    let cast = builder.buildCheckedCastAddr(source: source, type: vilTargetType.address)
    let test = builder.buildEqualAddr(
      lhs: cast, rhs: NullAddr(type: vilTargetType.address))

    guard let function = builder.function else { fatalError("unreachable") }
    let success = function.createBasicBlock()
    let failure = function.createBasicBlock()
    let finally = function.createBasicBlock()
    builder.buildCondBranch(cond: test, thenDest: failure, elseDest: success)

    // If the cast succeeded ...
    builder.block = success
    builder.buildCopyAddr(
      dest: builder.buildAllocExistential(container: result, witness: vilTargetType),
      source: cast)
    builder.buildBranch(dest: finally)

    // If the cast failed ...
    builder.block = failure
    let nilValue = emitNil()
    builder.buildStore(
      lvalue: builder.buildAllocExistential(container: result, witness: nilValue.type),
      rvalue: nilValue)
    builder.buildBranch(dest: finally)

    builder.block = finally
    return .success(builder.buildLoad(lvalue: result))
  }

  mutating func visit(_ node: UnsafeCastExpr) -> Result<Value, EmitterError> {
    let sourceType = node.value.type.dealiased
    let targetType = node.type.dealiased

    // Check for trivial casts.
    guard sourceType !== targetType else {
      context.report(.unsafeCastToSameTypeHasNoEffect(type: node.type, range: node.range))
      return node.value.accept(&self)
    }

    // If the target type is existential, store the node's value into a new container.
    if targetType.isExistential {
      let container = builder.buildAllocStack(type: .lower(targetType))
      emit(assign: node.value, to: container)
      return .success(builder.buildLoad(lvalue: container))
    }

    // If the value being cast is an existential container, open it.
    if sourceType.isExistential {
      let container = emit(rvalue: node.value)
      let value = builder.buildOpenExistential(container: container, type: .lower(targetType))
      return .success(value)
    }

    // FIXME: Implement "structural cast".
    fatalError("not implemented")
  }

  mutating func visit(_ node: TupleExpr) -> ExprResult {
    let tuple = builder.buildTuple(
      type: node.type as! TupleType,
      elems: node.elems.map({ elem in emit(rvalue: elem.value) }))
    return .success(tuple)
  }

  mutating func visit(_ node: CallExpr) -> ExprResult {
    let callee: Value
    var args: [Value] = []

    // Emit the function's callee.
    switch node.fun {
    case let memberRef as MemberDeclRefExpr where memberRef.decl.isMember:
      // The callee is a reference to a member declaration. It can either be a method or a
      // functional property.
      if let methodDecl = memberRef.decl as? BaseFunDecl {
        // This is a call `foo.bar(x: 0, y: 1)`, where `bar` is a method and `foo` is its receiver.
        let receiver = methodDecl.isMutating
          ? emit(lvalue: memberRef.base)
          : emit(rvalue: memberRef.base)
        args.append(receiver)

        // If the reciever is a existential, the method must be dispatched dynamically, otherwise
        // it must be dispatched statically.
        callee = memberRef.base.type.isExistential
          ? builder.buildWitnessMethod(container: receiver, decl: methodDecl)
          : FunRef(function: builder.getOrCreateFunction(from: methodDecl))
      } else {
        // This is a call `foo.bar(x: 0, y: 1)`, where `bar` is a functional property of `foo`.
        fatalError("not implemented")
      }

    default:
      // Emit the callee "as is"
      callee = emit(rvalue: node.fun)
    }

    // Emit the function's arguments.
    let calleeType = callee.type as! VILFunType
    for i in 0 ..< node.args.count {
      var value = emit(rvalue: node.args[i].value)

      // Check if the argument needs to be prepared before being passed.
      if calleeType.paramTypes[i].isExistential || (calleeType.paramConvs[i] == .exist) {
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
    var value: Value = builder.buildApply(fun: callee, args: args)

    // Emit the extraction of the result value.
    if calleeType.retConv == .exist {
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

    // FIXME: We need a better, more reliable way to easily determine whether the node requires
    // l-value to r-value conversion.

    switch node.decl {
    case let decl as VarDecl:
      if let value = locals[ObjectIdentifier(decl)] {
        return value.type.isAddress
          ? .success(builder.buildLoad(lvalue: value))
          : .success(value)
      }

    case let decl as FunDecl where decl.isBuiltin:
      // Emit a built-in function.
      return .success(BuiltinFunRef(decl: decl))

    case let decl as BaseFunDecl:
      // Look for the declaration in the function's locals.
      if let loc = locals[ObjectIdentifier(decl)] {
        assert(loc.type.isAddress)
        return .success(builder.buildLoad(lvalue: loc))
      }

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
    var emitter = LValueEmitter(funDecl: funDecl, locals: locals, builder: builder)
    switch node.accept(&emitter) {
    case .success(let lvalue):
      return .success(builder.buildLoad(lvalue: lvalue))
    case let failure:
      return failure
    }
  }

  func visit(_ node: TypeDeclRefExpr) -> ExprResult {
    fatalError()
  }

  mutating func visit(_ node: MemberDeclRefExpr) -> ExprResult {
    // Emit the base value.
    let base: Value
    switch node.base.accept(&self) {
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

  mutating func visit(_ node: AsyncExpr) -> ExprResult {
    // Collect the declarations being captured by the async expression.
    var collector = CaptureCollector(relativeTo: nil)
    collector.walk(expr: node.value)

    // Create the type of the function wrapping the async expression.
    assert(node.type is AsyncType)
    let context = node.type.context
    let fnType = context.funType(
      paramType: context.tupleType(types: collector.captures.map({ $0.value.decl.type })),
      retType: (node.type as! AsyncType).base)
    let args = collector.captures.map({ emit(rvalue: $0.value) })

    // Emit the expression wrapper.
    let fn = builder.getOrCreateFunction(name: "_async\(builder.buildUniqueID())", type: fnType)

    // Save the emitter context.
    let currentLocals = locals
    let currentBlock = builder.block

    // Emit the wrapper's body.
    locals = [:]
    builder.block = fn.createBasicBlock(arguments: args)
    for (i, box) in collector.captures.enumerated() {
      let type = fn.type.paramTypes[i].contextualized(in: funDecl.genericEnv!, from: funDecl)
      let alloc = builder.buildAllocStack(type: type)
      builder.buildStore(lvalue: alloc, rvalue: builder.block!.arguments[i])
      locals[ObjectIdentifier(box.value.decl)] = alloc
    }

    builder.buildRet(value: emit(rvalue: node.value))

    // Restore the emitter context.
    builder.block = currentBlock
    locals = currentLocals

    // Emit the async instruction.
    return .success(builder.buildAsync(fun: fn, args: args))
  }

  mutating func visit(_ node: AwaitExpr) -> ExprResult {
    return .success(builder.buildAwait(value: emit(rvalue: node.value)))
  }

  func visit(_ node: AddrOfExpr) -> ExprResult {
    fatalError()
  }

  mutating func visit(_ node: MatchExpr) -> ExprResult {
    guard let function = builder.function else { fatalError("unreachable") }
    assert(!node.cases.isEmpty)

    // Emit the subject of the match.
    let subject = emit(rvalue: node.subject)

    // If the node is a sub-expression, allocate storage for its "value".
    let storage: AllocStackInst? = node.isSubexpr
      ? builder.buildAllocStack(type: .lower(node.type))
      : nil

    // Create a block for all cases to branch unconditionally (unless they yield control).
    let lastBlock = function.createBasicBlock()

    func irrefutable(stmt: MatchCaseStmt) {
      if let decl = stmt.pattern.singleVarDecl {
        // Assign the subject to a local variable.
        let lvalue = Emitter.emit(localVar: decl, locals: &locals, into: builder)
        emit(assign: subject, ofType: node.type.dealiased, to: lvalue)
      }
      builder.buildBranch(dest: lastBlock)
    }

    // Emit each case statement.
    for (i, stmt) in node.cases.enumerated() {
      // Update the builder's insertion point and prepare the case's block.
      let thenBlock = function.createBasicBlock(before: lastBlock)
      var elseBlock = lastBlock

      // Handle irrefutable patterns.
      if !stmt.pattern.isRefutable {
        irrefutable(stmt: stmt)
        break
      } else if let pattern = stmt.pattern as? BindingPattern {
        // Check for trivial casts that actually make the pattern irrefutable.
        guard pattern.type !== node.subject.type else {
          irrefutable(stmt: stmt)
          break
        }

        // Refutable binding pattern require a type check.
        let patType = VILType.lower(pattern.type)
        var patLoc: Value = builder.buildAllocStack(type: subject.type)
        builder.buildStore(lvalue: patLoc, rvalue: subject)

        if node.subject.type.isExistential {
          if pattern.type.isExistential {
            patLoc = builder.buildCheckedCastAddr(source: patLoc, type: patType.address)
          } else {
            patLoc = builder.buildOpenExistentialAddr(container: patLoc, type: patType.address)
          }
        } else {
          fatalError("not implemented")
        }

        let cond = builder.buildEqualAddr(lhs: patLoc, rhs: NullAddr(type: patLoc.type))
        if i < node.cases.count - 1 {
          elseBlock = function.createBasicBlock(before: lastBlock)
        }
        builder.buildCondBranch(cond: cond, thenDest: elseBlock, elseDest: thenBlock)

        if let decl = stmt.pattern.singleVarDecl {
          locals[ObjectIdentifier(decl)] = patLoc
        } else {
          fatalError("not implemented")
        }
      } else {
        // FIXME: Handle complex conditional patterns recrusively.
        fatalError("not implemented")
      }

      builder.block = thenBlock

      // Emit the statement's body.
      if let storage = storage {
        let value = emit(rvalue: stmt.body.stmts[0] as! Expr)
        builder.buildStore(lvalue: storage, rvalue: value)
      } else {
        Emitter.emit(brace: stmt.body, funDecl: funDecl, locals: &locals, into: builder)
      }

      // Jump to the end of the match.
      builder.buildBranch(dest: lastBlock)
      builder.block = elseBlock
      guard stmt.pattern.isRefutable else { break }
    }

    if let storage = storage {
      return .success(builder.buildLoad(lvalue: storage))
    } else {
      return .success(UnitValue(context: context))
    }
  }

  func visit(_ node: WildcardExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: ErrorExpr) -> ExprResult {
    fatalError()
  }

}
