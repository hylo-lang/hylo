import AST
import Basic

/// A VIL emitter for r-values.
struct RValueEmitter: ExprVisitor {

  typealias ExprResult = Result<Value, EmitterError>

  /// The state in which the r-value is emitted.
  let _state: UnsafeMutablePointer<Emitter.State>

  /// The VIL builder used by the emitter.
  let _builder: UnsafeMutablePointer<Builder>

  var context: Context { _state.pointee.funDecl.type.context }

  var funDecl: BaseFunDecl { _state.pointee.funDecl }

  var locals: SymbolTable {
    get { _state.pointee.locals }
    _modify { yield &_state.pointee.locals }
  }

  var allocs: [AllocStackInst] {
    get { _state.pointee.allocs }
    _modify { yield &_state.pointee.allocs }
  }

  var loans: Set<PathIdentifier> {
    get { _state.pointee.loans }
    _modify { yield &_state.pointee.loans }
  }

  var builder: Builder {
    get { _builder.pointee }
    _modify { yield &_builder.pointee }
  }

  /// Emits the assignment of `value` to `target`.
  mutating func emit(assign value: Value, ofType valueType: ValType, to target: Value) {
    assert(valueType.isCanonical)
    let targetType = target.type.valType

    // If the destination has the same type as the value to assign, or if both have a union type,
    // then we can emit a simple store instruction.
    if (targetType == valueType) || (targetType is UnionType && valueType is UnionType) {
      builder.buildStore(target: target, value: value)
      return
    }

    // If the destination is an existential container, then there are two cases to consider:
    // 1. If the destination is an existential container too (but with a different type, ortherwise
    //    the test above would have succeeded), then we need to cast its package.
    // 2. If the value has a concrete type, then it must be packed into an existential container.
    if targetType.isExistential {
      if valueType.isExistential {
        let loc = builder.buildAllocStack(type: value.type)
        builder.buildStore(target: loc, value: value, semantics: .init_)
        let tmp = builder.buildUnsafeCastAddr(source: loc, type: target.type)
        builder.buildCopyAddr(target: target, source: tmp)
        builder.buildDeallocStack(alloc: loc)
      } else {
        let lvalue = builder.buildAllocExistential(container: target, witness: value.type)
        builder.buildStore(target: lvalue, value: value)
      }
      return
    }

    // FIXME: We should check that the assignment is legal.
    builder.buildStore(target: target, value: value)
  }

  /// Emits the assignment of the value represented by `expr` to `target`.
  mutating func emit(assign expr: Expr, to target: Value) {
    let exprType = expr.type.dealiased.canonical
    let targetType = target.type.valType

    // If both operands have an existential layout and the r-value can be treated as a location,
    // then we may simply emit `copy_addr`.
    if targetType.isExistential && exprType.isExistential {
      var emitter = LValueEmitter(_state: _state, _builder: _builder)
      if case .success(let result) = expr.accept(&emitter) {
        // Convert the r-value into the l-value's type if it has a different type.
        var source = result.loc
        if targetType != exprType {
          source = builder.buildUnsafeCastAddr(source: source, type: target.type)
        }

        /// Copies the contents from the location on the right to the location on the left.
        builder.buildCopyAddr(target: target, source: source)
        return
      }
    }

    // Otherwise, emit the r-value and fall back to the regular path.
    let value = emit(rvalue: expr)
    emit(assign: value, ofType: exprType, to: target)
  }

  /// Emits a r-value.
  mutating func emit(rvalue expr: Expr) -> Value {
    Emitter.emit(rvalue: expr, in: &_state.pointee, with: &_builder.pointee)
  }

  /// Emits an l-value.
  mutating func emit(lvalue expr: Expr) -> Value {
    var emitter = LValueEmitter(_state: _state, _builder: _builder)
    switch expr.accept(&emitter) {
    case .success(let result):
      // Check for violation of exclusive access.
      if loans.insert(result.pathID).inserted {
        return result.loc
      } else {
        context.report(.exclusiveAccessViolation(range: expr.range))
      }

    case .failure(let error):
      switch error {
      case .immutableBinding(let decl):
        context.report(.mutRefToImmut(binding: decl.name, range: expr.range))
      case .immutableCapture(let decl):
        context.report(.mutRefToImmutCapture(binding: decl.name, range: expr.range))
      case .immutableSelf(let property):
        context.report(.mutRefToImmutSelf(propertyName: property.name, range: expr.range))
      case .immutableExpr:
        context.report(.mutRefToImmutValue(range: expr.range))
      }
    }

    return builder.buildPoison()
  }

  // ----------------------------------------------------------------------------------------------
  // MARK: Visitors
  // ----------------------------------------------------------------------------------------------

  func visit(_ node: BoolLiteralExpr) -> Result<Value, EmitterError> {
    fatalError("not implemented")
  }

  mutating   func visit(_ node: IntLiteralExpr) -> ExprResult {
    // We can assume the expression's type conforms to `ExpressibleBy***Literal` (as type checking
    // succeeded). Therefore we can look for a conversion constructor `new(literal:)`.
    let view = context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral)!.instanceType as! ViewType
    let callee: Value

    var type = node.type.dealiased.canonical
    if let inoutType = type as? InoutType {
      type = inoutType.base
    }

    if let type = type as? NominalType {
      // The node has a concrete type; we can dispatch `new(literal:)` statically.
      let conformance = type.decl.conformanceTable[view]!
      let fun = builder.getOrCreateFunction(from: conformance.entries[0].impl as! CtorDecl)
      callee = builder.buildFunRef(function: fun)
    } else if type is SkolemType {
      // The node has an skolem type; we have to dispatch dynamically.
      fatalError("not implemented")
    } else {
      fatalError("unreachable")
    }

    // Emit a call to the constructor.
    let literal = builder.buildIntLiteral(value: node.value)
    return .success(builder.buildApply(callee: callee, args: [literal]))
  }

  func visit(_ node: FloatLiteralExpr) -> Result<Value, EmitterError> {
    fatalError("not implemented")
  }

  func visit(_ node: StringLiteralExpr) -> Result<Value, EmitterError> {
    fatalError("not implemented")
  }

  mutating func visit(_ node: AssignExpr) -> ExprResult {
    // Emit the right operand first.
    let value = emit(rvalue: node.rvalue)

    // Emit the left operand, followed by the assignment.
    var emitter = LValueEmitter(_state: _state, _builder: _builder)
    switch node.lvalue.accept(&emitter) {
    case .success(let result):
      emit(assign: value, ofType: node.rvalue.type, to: result.loc)

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
    return .success(builder.buildUnit())
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

    // Allocate an existential container to hold the result of the cast.
    let container = builder.buildAllocStack(type: .lower(node.type))
    allocs.append(container)

    // Cast the value.
    let cast: CheckedCastAddrInst
    var emitter = LValueEmitter(_state: _state, _builder: _builder)
    if case .success(let result) = node.value.accept(&emitter) {
      cast = builder.buildCheckedCastAddr(source: result.loc, type: vilTargetType.address)
    } else {
      let tmp = builder.buildAllocStack(type: .lower(node.type))
      builder.buildStore(target: tmp, value: emit(rvalue: node.value), semantics: .init_)
      cast = builder.buildCheckedCastAddr(source: tmp, type: vilTargetType.address)
      builder.buildDeallocStack(alloc: tmp)
    }

    // Handle control flow, depending on the outcome of the cast.
    let test = builder.buildEqualAddr(
      lhs: cast, rhs: builder.buildNullAddr(type: vilTargetType.address))
    let success = builder.buildBasicBlock()
    let failure = builder.buildBasicBlock()
    let finally = builder.buildBasicBlock()
    builder.buildCondBranch(cond: test, thenDest: failure, elseDest: success)

    // If the cast succeeded ...
    builder.insertionPointer!.blockID = success
    builder.buildCopyAddr(
      target: builder.buildAllocExistential(container: container, witness: vilTargetType),
      source: cast)
    builder.buildBranch(dest: finally)

    // If the cast failed ...
    builder.insertionPointer!.blockID = failure
    let nilValue = builder.buildNil()
    builder.buildStore(
      target: builder.buildAllocExistential(container: container, witness: nilValue.type),
      value: nilValue)
    builder.buildBranch(dest: finally)

    builder.insertionPointer!.blockID = finally
    let result = builder.buildLoad(location: container)
    builder.buildDeallocStack(alloc: container)
    return .success(result)
  }

  mutating func visit(_ node: UnsafeCastExpr) -> Result<Value, EmitterError> {
    let sourceType = node.value.type.dealiased.canonical
    let targetType = node.type.dealiased.canonical

    // Check for trivial casts.
    guard sourceType !== targetType else {
      context.report(.unsafeCastToSameTypeHasNoEffect(type: node.type, range: node.range))
      return node.value.accept(&self)
    }

    // If the target type is existential, store the node's value into a temporary container.
    if targetType.isExistential {
      let tmp = builder.buildAllocStack(type: .lower(targetType))
      emit(assign: node.value, to: tmp)
      let result = builder.buildLoad(location: tmp)
      builder.buildDeallocStack(alloc: tmp)
      return .success(result)
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
      operands: node.elems.map({ elem in emit(rvalue: elem.value) }))
    return .success(tuple)
  }

  mutating func visit(_ node: CallExpr) -> ExprResult {
    let oldLoans = loans
    defer { loans = oldLoans }

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

        // If the reciever is a existential, the method must be dispatched dynamically.
        callee = member.base.type.isExistential
          ? builder.buildWitnessMethod(container: receiver, decl: methodDecl)
          : builder.buildFunRef(function: builder.getOrCreateFunction(from: methodDecl))
      } else {
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
          builder.buildStore(target: ptr, value: value)
          value = builder.buildLoad(location: tmp)
          builder.buildDeallocStack(alloc: tmp)
        }
      }

      args.append(value)
    }

    // Emit the call.
    var value: Value = builder.buildApply(callee: callee, args: args)

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

  mutating func visit(_ node: DeclRefExpr) -> ExprResult {
    // First, we look for an entry in the local symbol table.
    if let value = locals[ObjectIdentifier(node.decl)] {
      // FIXME: We should have a more reliable way to determine whether an address must be loaded.
      return value.type.isAddress
        ? .success(builder.buildLoad(location: value))
        : .success(value)
    }

    switch node.decl {
    case is VarDecl:
      // FIXME: Handle computed properties.
      fatalError("not implemented")

    case let decl as FunDecl where decl.isBuiltin:
      // Emit a built-in function reference.
      return .success(builder.buildBuiltinFunRef(decl: decl))

    case let decl as BaseFunDecl:
      // Emit a function reference, wrapped into a thick container.
      let fun = builder.getOrCreateFunction(from: decl)
      let thick = builder.buildThinToThick(ref: builder.buildFunRef(function: fun))
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

    if let decl = node.decl as? VarDecl, decl.hasStorage {
      let member = builder.buildRecordMember(
        record: base, memberDecl: decl, type: .lower(node.type))
      return .success(member)
    }

    fatalError("not implemented")
  }

  func visit(_ node: TupleMemberExpr) -> ExprResult {
    fatalError()
  }

  mutating func visit(_ node: AsyncExpr) -> ExprResult {
    // Emit the function representing the body of the expression.
    let fun = Emitter.emit(function: node.body, with: &builder)
    let ref = builder.buildFunRef(function: fun)

    // Emit the value of each captured declaration. Capture with `val` or `var` semantics are
    // copied from the environment, and so we must emit a r-value either way.
    let captureTable = node.body.computeAllCaptures()
    let captures = captureTable.map({ (key, value) -> Value in
      // FIXME: Implement capture-by-reference (requires local bindings).
      assert(value.semantics != .mut, "not implemented")
      let expr = DeclRefExpr(decl: key.capturedDecl, type: value.type)
      return emit(rvalue: expr)
    })

    return .success(builder.buildAsync(ref: ref, captures: captures))
  }

  mutating func visit(_ node: AwaitExpr) -> ExprResult {
    return .success(builder.buildAwait(value: emit(rvalue: node.value)))
  }

  mutating func visit(_ node: AddrOfExpr) -> ExprResult {
    return .success(emit(lvalue: node.value))
  }

  mutating func visit(_ node: MatchExpr) -> ExprResult {
    assert(!node.cases.isEmpty)

    // Emit the subject of the match.
    let subject = emit(rvalue: node.subject)

    // If the node is a sub-expression, allocate storage for its "value".
    let storage: AllocStackInst? = node.isSubexpr
      ? builder.buildAllocStack(type: .lower(node.type))
      : nil

    // Create a block for all cases to branch unconditionally (unless they yield control).
    let lastBlock = builder.buildBasicBlock()

    /// Emits the body of a case in the current block.
    func emitCaseBody(body: BraceStmt) {
      if let storage = storage {
        let value = emit(rvalue: body.stmts[0] as! Expr)
        builder.buildStore(target: storage, value: value)
      } else {
        Emitter.emit(brace: body, in: &_state.pointee, with: &_builder.pointee)
      }
    }

    // Emit each case statement.
    for stmt in node.cases {
      // Wildcard patterns are irrefutable.
      if stmt.pattern is WildcardPattern {
        assert(stmt.pattern.type == node.subject.type)
        emitCaseBody(body: stmt.body)
        builder.buildBranch(dest: lastBlock)
        builder.insertionPointer!.blockID = lastBlock
        break
      }

      // Binding patterns must be allocated.
      if let pattern = stmt.pattern as? BindingPattern {
        let patterns = pattern.namedPatterns
        // FIXME: Handle destructuring.
        precondition(patterns.count == 1, "not implemented")
        let patLoc = Emitter.emit(
          storedLocalVar: patterns[0].decl, in: &_state.pointee, with: &_builder.pointee)

        // If the subject has the same type as the pattern, the latter is actually irrefutable.
        // Otherwise, we must try to cast the subject and branch over the result.
        if pattern.type === node.subject.type {
          Emitter.emit(
            assign: subject,
            ofType: node.subject.type,
            to: patLoc,
            asInit: true,
            with: &_builder.pointee)
          emitCaseBody(body: stmt.body)

          // Deallocate the pattern's memory.
          if let loc = patLoc as? AllocStackInst { builder.buildDeallocStack(alloc: loc) }
          builder.buildBranch(dest: lastBlock)
          builder.insertionPointer!.blockID = lastBlock
          break
        }

        // FIXME: Handle down casts.
        precondition(node.subject.type.isExistential, "not implemented")

        let patType = VILType.lower(pattern.type)
        let tmp = builder.buildAllocStack(type: subject.type)
        builder.buildStore(target: tmp, value: subject, semantics: .init_)
        let cast = builder.buildCheckedCastAddr(source: tmp, type: patType.address)
        builder.buildDeallocStack(alloc: tmp)

        let thenBlock = builder.buildBasicBlock()
        let elseBlock = builder.buildBasicBlock()

        let cond = builder.buildEqualAddr(
          lhs: cast, rhs: builder.buildNullAddr(type: patType.address))
        builder.buildCondBranch(cond: cond, thenDest: elseBlock, elseDest: thenBlock)

        builder.insertionPointer!.blockID = thenBlock
        locals[ObjectIdentifier(patterns[0].decl)] = cast
        emitCaseBody(body: stmt.body)
        if let loc = patLoc as? AllocStackInst { builder.buildDeallocStack(alloc: loc) }
        builder.buildBranch(dest: lastBlock)

        builder.insertionPointer!.blockID = elseBlock
        if let loc = patLoc as? AllocStackInst { builder.buildDeallocStack(alloc: loc) }
        builder.buildBranch(dest: lastBlock)
        builder.insertionPointer!.blockID = lastBlock

        continue
      }

      // FIXME: Handle complex conditional patterns recursively.
      fatalError("not implemented")
    }

    assert(builder.insertionPointer?.blockID == lastBlock)
    if let storage = storage {
      let result = builder.buildLoad(location: storage)
      builder.buildDeallocStack(alloc: storage)
      return .success(result)
    } else {
      return .success(builder.buildUnit())
    }
  }

  func visit(_ node: WildcardExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: ErrorExpr) -> ExprResult {
    fatalError()
  }

}
