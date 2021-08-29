import AST
import Basic

/// A VIL emitter for r-values.
struct RValueEmitter: ExprVisitor {

  typealias ExprResult = Result<Value, EmitterError>

  /// The environment in with the r-value is emitted.
  let env: UnsafeMutablePointer<Emitter.Environment>

  /// The VIL builder used by the emitter.
  let builder: Builder

  var funDecl: BaseFunDecl { env.pointee.funDecl }

  var locals: SymbolTable {
    get     { env.pointee.locals }
    set     { env.pointee.locals = newValue }
    _modify { yield &env.pointee.locals }
  }

  var loans: Set<PathIdentifier> {
    get     { env.pointee.loans }
    set     { env.pointee.loans = newValue }
    _modify { yield &env.pointee.loans }
  }

  var context: AST.Context { env.pointee.funDecl.type.context }

  /// Emits the assignment of `value` to `dest`.
  func emit(assign value: Value, ofType valueType: ValType, to dest: Value) {
    assert(valueType.isCanonical)
    let destType = dest.type.valType

    // If the destination has the same type as the value to assign, or if both have a union type,
    // then we can emit a simple store instruction.
    if (destType == valueType) || (destType is UnionType && valueType is UnionType) {
      builder.buildStore(lvalue: dest, rvalue: value)
      return
    }

    // If the destination is an existential container, then there are two cases to consider:
    // 1. If the destination is an existential container too (but with a different type, ortherwise
    //    the test above would have succeeded), then we need to cast its package.
    // 2. If the value has a concrete type, then it must be packed into an existential container.
    if destType.isExistential {
      if valueType.isExistential {
        var tmp: Value = builder.buildAllocStack(type: value.type)
        builder.buildStore(lvalue: tmp, rvalue: value)
        tmp = builder.buildUnsafeCastAddr(source: tmp, type: dest.type)
        builder.buildCopyAddr(dest: dest, source: tmp)
      } else {
        let lvalue = builder.buildAllocExistential(container: dest, witness: value.type)
        builder.buildStore(lvalue: lvalue, rvalue: value)
      }
      return
    }

    // FIXME: We should check that the assignment is legal.
    builder.buildStore(lvalue: dest, rvalue: value)
  }

  /// Emits the assignment of the value represented by `expr` to `dest`.
  mutating func emit(assign expr: Expr, to dest: Value) {
    let exprType = expr.type.dealiased.canonical
    let destType = dest.type.valType

    // If both operands have an existential layout and the r-value can be treated as a location,
    // then we may simply emit `copy_addr`.
    if destType.isExistential && exprType.isExistential {
      var emitter = LValueEmitter(env: env, builder: builder)
      if case .success(let result) = expr.accept(&emitter) {
        // Convert the r-value into the l-value's type if it has a different type.
        var source = result.loc
        if destType != exprType {
          source = builder.buildUnsafeCastAddr(source: source, type: dest.type)
        }

        /// Copies the contents from the location on the right to the location on the left.
        builder.buildCopyAddr(dest: dest, source: source)
        return
      }
    }

    // Otherwise, emit the r-value and fall back to the regular path.
    let value = emit(rvalue: expr)
    emit(assign: value, ofType: exprType, to: dest)
  }

  /// Emits a r-value.
  mutating func emit(rvalue expr: Expr) -> Value {
    Emitter.emit(rvalue: expr, in: &env.pointee, with: builder)
  }

  /// Emits an l-value.
  func emit(lvalue expr: Expr) -> Value {
    Emitter.emit(lvalue: expr, in: &env.pointee, with: builder)
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
    var emitter = LValueEmitter(env: env, builder: builder)
    switch node.lvalue.accept(&emitter) {
    case .success(let result):
      emit(assign: node.rvalue, to: result.loc)

    case .failure(let error):
      switch error {
      case .immutableBinding(let decl):
        context.report(.assignToImmut(binding: decl.name, range: node.lvalue.range))
      case .immutableCapture(let decl):
        context.report(.assignToImmutCapture(binding: decl.name, range: node.lvalue.range))
      case .immutableSelf(let property):
        context.report(.assignToImmutSelf(propertyName: property.name, range: node.lvalue.range))
      case .immutableExpr:
        context.report(.assignToImmutValue(range: node.lvalue.range))
      }
    }

    // An assignment always results in a unit value.
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

    let lvalue: Value
    var emitter = LValueEmitter(env: env, builder: builder)
    if case .success(let result) = node.value.accept(&emitter) {
      lvalue = result.loc
    } else {
      lvalue = builder.buildAllocStack(type: .lower(node.type))
      builder.buildStore(lvalue: lvalue, rvalue: emit(rvalue: node.value))
    }

    // Cast the value.
    let cast = builder.buildCheckedCastAddr(source: lvalue, type: vilTargetType.address)
    let test = builder.buildEqualAddr(
      lhs: cast, rhs: NullAddr(type: vilTargetType.address))

    guard let function = builder.insertionPoint?.function else { fatalError("unreachable") }
    let success = function.createBasicBlock()
    let failure = function.createBasicBlock()
    let finally = function.createBasicBlock()
    builder.buildCondBranch(cond: test, thenDest: failure, elseDest: success)

    // If the cast succeeded ...
    builder.insertionPoint!.blockID = success
    builder.buildCopyAddr(
      dest: builder.buildAllocExistential(container: result, witness: vilTargetType),
      source: cast)
    builder.buildBranch(dest: finally)

    // If the cast failed ...
    builder.insertionPoint!.blockID = failure
    let nilValue = Emitter.emitNil(context: context, with: builder)
    builder.buildStore(
      lvalue: builder.buildAllocExistential(container: result, witness: nilValue.type),
      rvalue: nilValue)
    builder.buildBranch(dest: finally)

    builder.insertionPoint!.blockID = finally
    return .success(builder.buildLoad(lvalue: result))
  }

  mutating func visit(_ node: UnsafeCastExpr) -> Result<Value, EmitterError> {
    let sourceType = node.value.type.dealiased.canonical
    let targetType = node.type.dealiased.canonical

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
    case let member as MemberDeclRefExpr where member.decl.isMember:
      // The callee is a reference to a member declaration: it's either a method or a functional
      // property. If it's the former, we have to emit its receiver; othwerise, we just have to
      // emit a member access.
      if let methodDecl = member.decl as? BaseFunDecl {
        let receiver = methodDecl.isMutating
          ? emit(lvalue: member.base)
          : emit(rvalue: member.base)
        args.append(receiver)

        // If the reciever is a existential, the method must be dispatched dynamically, otherwise
        // it must be dispatched statically.
        callee = member.base.type.isExistential
          ? builder.buildWitnessMethod(container: receiver, decl: methodDecl)
          : FunRef(function: builder.getOrCreateFunction(from: methodDecl))
      } else {
        fatalError("not implemented")
      }

    default:
      // Emit the callee "as is"
      callee = emit(rvalue: node.fun)
    }

    // Emit the function's arguments.
    let calleeType = callee.type as! VILFunType
    let oldLoans = loans

    for i in 0 ..< node.args.count {
      var value = emit(rvalue: node.args[i].value)

      if calleeType.paramConvs[i] == .mut {
        // A parameter with the 'mut' convention prescribes that the argument be mutable argument.
        assert(node.args[i].value is AddrOfExpr || value is PoisonValue)
        args.append(value)
        continue
      }

      if calleeType.paramTypes[i].isExistential || (calleeType.paramConvs[i] == .exist) {
        // The parameter has an existential type, or its convention prescribes that it should be
        // passed as an existential container. If the argument isn't already packaged, we have to
        // wrap it into a container. Otherwise, we can pass it "as is", since all existential
        // containers have the same memory layout.
        if !value.type.isExistential {
          // FIXME: Allocating a container of type `Any`, completely erases the argument's type.
          // Hence, the resulting VIL is in fact ill-typed, as we could call a function expecting
          // an argument of type `T` with a value of type `Any`. A better strategy would be to
          // create an existential container that meets the parameter's requirements.
          let tmp = builder.buildAllocStack(type: .lower(context.anyType))
          let ptr = builder.buildAllocExistential(container: tmp, witness: value.type)
          builder.buildStore(lvalue: ptr, rvalue: value)
          value = builder.buildLoad(lvalue: tmp)
        }
      }

      args.append(value)
    }

    // Emit the call.
    var value: Value = builder.buildApply(fun: callee, args: args)
    loans = oldLoans

    // Emit the extraction of the result value.
    if calleeType.retConv == .exist {
      value = builder.buildOpenExistential(container: value, type: .lower(node.type))
    }

    // FIXME: Deallocate the memory that we allocated for indirect parameters.
    return .success(value)
  }

  func visit(_ node: UnresolvedDeclRefExpr) -> ExprResult {
    fatalError("unreachable")
  }

  func visit(_ node: UnresolvedMemberExpr) -> ExprResult {
    fatalError("unreachable")
  }

  func visit(_ node: UnresolvedQualDeclRefExpr) -> ExprResult {
    fatalError("unreachable")
  }

  func visit(_ node: OverloadedDeclRefExpr) -> ExprResult {
    fatalError("unreachable")
  }

  func visit(_ node: DeclRefExpr) -> ExprResult {
    // First, we look for an entry in the local symbol table.
    if let value = locals[ObjectIdentifier(node.decl)] {
      // FIXME: We should have a more reliable way to determine whether an address must be loaded.
      return value.type.isAddress
        ? .success(builder.buildLoad(lvalue: value))
        : .success(value)
    }

    switch node.decl {
    case is VarDecl:
      // FIXME: Handle computed properties.
      fatalError("not implemented")

    case let decl as FunDecl where decl.isBuiltin:
      // Emit a built-in function reference.
      return .success(BuiltinFunRef(decl: decl))

    case let decl as BaseFunDecl:
      // Emit a function reference, wrapped into a thick container.
      let function = builder.getOrCreateFunction(from: decl)
      let thick = builder.buildThinToThick(ref: FunRef(function: function))
      return .success(thick)

    default:
      fatalError("unreachable")
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

    fatalError("not implemented")
  }

  func visit(_ node: TupleMemberExpr) -> ExprResult {
    fatalError()
  }

  mutating func visit(_ node: AsyncExpr) -> ExprResult {
    // Emit the function representing the body of the expression.
    let fun = Emitter.emit(function: node.body, with: builder)

    // Emit the value of each captured declaration. Capture with `val` or `var` semantics are
    // copied from the environment, and so we must emit a r-value either way.
    let captureTable = node.body.computeAllCaptures()
    let partialArgs = captureTable.map({ (key, value) -> Value in
      // FIXME: Implement capture-by-reference (requires local bindings).
      assert(value.semantics != .mut, "not implemented")
      let expr = DeclRefExpr(decl: key.capturedDecl, type: value.type)
      return emit(rvalue: expr)
    })

    return .success(builder.buildAsync(fun: fun, args: partialArgs))
  }

  mutating func visit(_ node: AwaitExpr) -> ExprResult {
    return .success(builder.buildAwait(value: emit(rvalue: node.value)))
  }

  mutating func visit(_ node: AddrOfExpr) -> ExprResult {
    var emitter = LValueEmitter(env: env, builder: builder)
    switch node.value.accept(&emitter) {
    case .success(let result):
      // Check for violation of exclusive access.
      if loans.insert(result.pathID).inserted {
        return .success(result.loc)
      } else {
        context.report(.exclusiveAccessViolation(range: node.range))
      }

    case .failure(let error):
      switch error {
      case .immutableBinding(let decl):
        context.report(.mutRefToImmut(binding: decl.name, range: node.value.range))
      case .immutableCapture(let decl):
        context.report(.mutRefToImmutCapture(binding: decl.name, range: node.value.range))
      case .immutableSelf(let property):
        context.report(.mutRefToImmutSelf(propertyName: property.name, range: node.value.range))
      case .immutableExpr:
        context.report(.mutRefToImmutValue(range: node.value.range))
      }
    }

    return .success(PoisonValue(context: node.type.context))
  }

  mutating func visit(_ node: MatchExpr) -> ExprResult {
    guard let function = builder.insertionPoint?.function else { fatalError("unreachable") }
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
        let lvalue = Emitter.emit(localVar: decl, into: &locals, with: builder)
        emit(assign: subject, ofType: node.type.dealiased.canonical, to: lvalue)
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
          patLoc = builder.buildCheckedCastAddr(source: patLoc, type: patType.address)
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
        // FIXME: Handle complex conditional patterns recursively.
        fatalError("not implemented")
      }

      builder.insertionPoint!.blockID = thenBlock

      // Emit the statement's body.
      if let storage = storage {
        let value = emit(rvalue: stmt.body.stmts[0] as! Expr)
        builder.buildStore(lvalue: storage, rvalue: value)
      } else {
        Emitter.emit(brace: stmt.body, in: &env.pointee, with: builder)
      }

      // Jump to the end of the match.
      builder.buildBranch(dest: lastBlock)
      builder.insertionPoint!.blockID = elseBlock
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
