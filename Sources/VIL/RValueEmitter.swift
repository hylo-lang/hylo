import AST
import Basic

/// A VIL emitter for r-values.
struct RValueEmitter: ExprVisitor {

  typealias ExprResult = Result<Value, EmitterError>

  /// The state in which the r-value is emitted.
  let _state: UnsafeMutablePointer<Emitter.State>

  /// The VIL builder used by the emitter.
  let _builder: UnsafeMutablePointer<Builder>

  var context: Context { _builder.pointee.context }

  var funDecl: BaseFunDecl { _state.pointee.funDecl }

  var locals: SymbolTable {
    get { _state.pointee.locals }
    _modify { yield &_state.pointee.locals }
  }

  var allocs: [AllocStackInst] {
    get { _state.pointee.allocs }
    _modify { yield &_state.pointee.allocs }
  }

  var builder: Builder {
    get { _builder.pointee }
    _modify { yield &_builder.pointee }
  }

  mutating func emit(
    assign rvalue: Value,
    to target: Value,
    into builder: inout Builder,
    range: SourceRange? = nil
  ) {
    Emitter.emit(assign: rvalue, to: target, into: &_builder.pointee, range: range)
  }

  /// Emits an r-value.
  mutating func emit(rvalue expr: Expr) -> Value {
    Emitter.emit(rvalue: expr, in: &_state.pointee, into: &_builder.pointee)
  }

  /// Emits an l-value.
  mutating func emit(lvalue expr: Expr) -> Value {
    Emitter.emit(lvalue: expr, in: &_state.pointee, into: &_builder.pointee)
  }

  /// Emits the address of a cell holding the value of the specified expression.
  mutating func emit(borrowable expr: Expr) -> Value {
    Emitter.emit(borrowable: expr, in: &_state.pointee, into: &_builder.pointee)
  }

  // ----------------------------------------------------------------------------------------------
  // MARK: Visitors
  // ----------------------------------------------------------------------------------------------

  func visit(_ node: BoolLiteralExpr) -> Result<Value, EmitterError> {
    fatalError("not implemented")
  }

  mutating func visit(_ node: IntLiteralExpr) -> ExprResult {
    // We can assume the expression's type conforms to `ExpressibleBy***Literal` (as type checking
    // succeeded). Therefore we can look for a conversion constructor `new(literal:)`.
    let view = context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral)!.instanceType as! ViewType
    let callee: Value

    let type = node.type.dealiased
    switch type {
    case let type as NominalType:
      // The node has a concrete type; we can dispatch `new(literal:)` statically.
      let conformance = type.decl.conformanceTable[view]!
      let fun = builder.getOrCreateFunction(from: conformance.entries[0].impl as! CtorDecl)
      callee = builder.buildFunRef(function: fun)

    case is SkolemType:
      // The node has an skolem type; we have to dispatch dynamically.
      fatalError("not implemented")

    default:
      fatalError("unreachable")
    }

    // Emit a call to the constructor.
    let literal = builder.buildIntLiteral(value: node.value)
    return .success(builder.buildApply(callee: callee, args: [literal], range: node.range))
  }

  func visit(_ node: FloatLiteralExpr) -> Result<Value, EmitterError> {
    fatalError("not implemented")
  }

  func visit(_ node: StringLiteralExpr) -> Result<Value, EmitterError> {
    fatalError("not implemented")
  }

  mutating func visit(_ node: AssignExpr) -> ExprResult {
    // Emit the right operand first and the left operand second.
    let rvalue = emit(rvalue: node.rvalue)
    let lvalue = emit(lvalue: node.lvalue)
    Emitter.emit(assign: rvalue, to: lvalue, into: &builder, range: node.range)

    // An assignment always results in a unit value.
    return .success(builder.buildUnit())
  }

  func visit(_ node: BaseCastExpr) -> Result<Value, EmitterError> {
    fatalError("unreachable")
  }

  mutating func visit(_ node: DynCastExpr) -> Result<Value, EmitterError> {
    guard let nodeType = node.type as? BoundGenericType
      else { preconditionFailure("dynamic cast must have a maybe type") }

    let lhsType = node.value.type.dealiased
    let rhsType = nodeType.args[0].dealiased

    // Allocate an existential container to hold the result of the cast.
    let container = builder.buildAllocStack(allocType: .lower(nodeType), range: node.range)
    defer { builder.buildDeallocStack(alloc: container) }

    // Emit the value to convert.
    let value: Value
    switch node.value.accept(&self) {
    case .success(let val):
      value = val
    case let failure:
      return failure
    }

    // FIXME: Handle structural casts.
    precondition(!(lhsType is TupleType) || !(rhsType is TupleType))

    // Runtime conversion of function types always fails.
    if (lhsType is FunType) && (rhsType is FunType) {
      context.report(.runtimeFunctionTypeConversion(range: node.range))
      builder.buildDelete(value: value)
      builder.buildInitExistAddr(container: container, value: builder.buildNil())
      return .success(builder.buildLoad(source: container))
    }

    // Cast the value.
    let succ = builder.buildBasicBlock(paramTypes: [.lower(rhsType)])
    let fail = builder.buildBasicBlock(paramTypes: [.lower(lhsType)])
    let tail = builder.buildBasicBlock()
    builder.buildCheckedCastBranch(
      value: value,
      type: .lower(rhsType),
      succ: succ,
      fail: fail,
      range: node.range)

    // If the cast succeeded ...
    builder.insertionPointer!.blockID = succ
    builder.buildInitExistAddr(
      container: container, value: builder.currentFun!.blocks[succ]!.params[0])
    builder.buildBranch(dest: tail)

    // If the cast failed ...
    builder.insertionPointer!.blockID = fail
    builder.buildDelete(value: builder.currentFun!.blocks[fail]!.params[0])
    builder.buildInitExistAddr(container: container, value: builder.buildNil())
    builder.buildBranch(dest: tail)

    // Finally ...
    builder.insertionPointer!.blockID = tail
    return .success(builder.buildLoad(source: container))
  }

  mutating func visit(_ node: UnsafeCastExpr) -> Result<Value, EmitterError> {
    let lhsType = node.value.type.dealiased
    let rhsType = node.type.dealiased

    // Emit the value to convert.
    let value: Value
    switch node.value.accept(&self) {
    case .success(let val):
      value = val
    case let failure:
      return failure
    }

    // FIXME: Handle structural casts.
    precondition(!(lhsType is TupleType) || !(rhsType is TupleType))

    // Runtime conversion of function types always fails.
    if (lhsType is FunType) && (rhsType is FunType) {
      context.report(.runtimeFunctionTypeConversion(range: node.range))
      builder.buildDelete(value: value)
      builder.buildCondFail(cond: builder.buildTrue(), range: node.range)
      return .success(builder.buildPoison())
    }

    // Convert the value.
    let converted = builder.buildCheckedCast(
      value: value, type: .lower(rhsType), range: node.range)
    return .success(converted)
  }

  mutating func visit(_ node: TupleExpr) -> ExprResult {
    let tuple = builder.buildTuple(
      type: node.type as! TupleType,
      operands: node.elems.map({ elem in emit(rvalue: elem.value) }))
    return .success(tuple)
  }

  mutating func visit(_ node: CallExpr) -> ExprResult {
    let callee: Value
    var args: [Value] = []

    // Emit the function's callee.
    switch node.fun {
    case let expr as MemberDeclRefExpr where expr.decl.isMember:
      // The callee is a reference to a member declaration.
      if let methodDecl = expr.decl as? BaseFunDecl {
        // The callee is a method; emit the receiver along with the dispatched function ref.
        if methodDecl.isConsuming {
          // The receiver is consumed; emit an r-value whether it's mutable or not.
          args.append(emit(rvalue: expr.base))
          callee = expr.base.type.isExistential
            ? builder.buildWitnessMethod(container: args[0], decl: methodDecl)
            : builder.buildFunRef(function: builder.getOrCreateFunction(from: methodDecl))
        } else {
          // The receiver is borrowed; emit an l-value or allocate temporary storage.
          let receiver = methodDecl.isMutating
            ? emit(lvalue: expr.base)
            : emit(borrowable: expr.base)
          args.append(builder.buildBorrowAddr(
            isMutable: methodDecl.isMutating, source: receiver, range: expr.base.range))
          callee = expr.base.type.isExistential
            ? builder.buildWitnessMethodAddr(container: args[0], decl: methodDecl)
            : builder.buildFunRef(function: builder.getOrCreateFunction(from: methodDecl))
        }
      } else {
        // The callee is a functional property; emit a regular member access.
        callee = emit(borrowable: node.fun)
      }

    case let expr as DeclRefExpr:
      switch expr.decl {
      case let decl as FunDecl where decl.isBuiltin:
        // The callee is a reference to a built-in function.
        callee = builder.buildBuiltinFunRef(decl: decl)

      case let decl as BaseFunDecl
        where (decl is CtorDecl) || decl.computeAllCaptures().isEmpty:
        // The callee is a reference to a thin function.
        callee = builder.buildFunRef(function: builder.getOrCreateFunction(from: decl))

      default:
        /// The callee is a thick function.
        callee = emit(borrowable: node.fun)
      }

    default:
      // The calle is an expression producing a thick function.
      callee = emit(borrowable: node.fun)
    }

    // Emit the function's arguments.
    let params = (node.fun.type as! FunType).params
    for i in 0 ..< node.args.count {
      switch params[0].policy! {
      case .local:
        // Local parameters are passed by reference. If the argument isn't an l-value, we must
        // allocate temporary storage.
        let loc = emit(borrowable: node.args[i].value)
        args.append(
          builder.buildBorrowAddr(isMutable: false, source: loc, range: node.args[i].range))

      case .inout:
        // Mutating parameters are passed by reference. The argument must be an l-value.
        let loc = emit(borrowable: node.args[i].value)
        args.append(
          builder.buildBorrowAddr(isMutable: true, source: loc, range: node.args[i].range))

      case .consuming, .consumingMutable:
        // Consuming parameters are passed by value.
        args.append(emit(rvalue: node.args[i].value))
      }

      // If the parameter has an existential type and the argument doesn't, the latter has to be
      // wrapped before being passed.
      if params[i].type.isExistential && !node.args[i].value.type.isExistential {
        assert(params[i].policy != .inout)
        switch params[i].policy! {
        case .local:
          args[i] = builder.buildPackBorrow(
            source: args[i],
            type: callee.type.paramType(at: i).address,
            range: node.args[i].value.range)

        case .consuming, .consumingMutable:
          let loc = builder.buildAllocStack(allocType: callee.type.paramType(at: i))
          allocs.append(loc)
          builder.buildInitExistAddr(container: loc, value: args[i], range: node.args[i].range)
          args[i] = loc

        case .inout:
          // Because arguments passed mutating must have the same type.
          fatalError("unreachable")
        }
      }
    }

    // Emit the call.
    let value = builder.buildApply(callee: callee, args: args)
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
    // If the identifier refers to a variable or a function parameter, its value should appear in
    // the local symbol table.
    if let value = locals[ObjectIdentifier(node.decl)] {
      // FIXME: Is there a more reliable way to determine whether an address must be loaded?
      return value.type.isAddress
        ? .success(builder.buildLoad(source: value))
        : .success(value)
    }

    // If the identifier refers to a function, wrap a reference into a thick function container.
    // Note: the referred function must be thin or it would have already been found in the local
    // symbol table above.
    if let decl = node.decl as? BaseFunDecl {
      assert(!decl.isBuiltin, "cannot wrap built-in function into a closure")
      let ref = builder.buildFunRef(function: builder.getOrCreateFunction(from: decl))
      let thk = builder.buildThinToThick(ref: ref)
      return .success(thk)
    }

    // FIXME: Handle computed properties.
    fatalError("not implemented")
  }

  func visit(_ node: TypeDeclRefExpr) -> ExprResult {
    fatalError()
  }

  mutating func visit(_ node: MemberDeclRefExpr) -> ExprResult {
    // Emit the base value.
    let base: Value
    switch node.base.accept(&self) {
    case .success(let val):
      base = val
    case let failure:
      return failure
    }

    if let decl = node.decl as? VarDecl, decl.hasStorage {
      // Stored properties cannot be extracted out of a record unless they are copyable.
      guard node.type.isCopyable else { return .failure(.nonCopyableProperty(decl)) }
      let member = builder.buildRecordMember(
        record: base, memberDecl: decl, type: .lower(node.type))
      return .success(member)
    }

    fatalError("not implemented")
  }

  func visit(_ node: TupleMemberExpr) -> ExprResult {
    fatalError("not implemented")
  }

  mutating func visit(_ node: AsyncExpr) -> ExprResult {
    // Emit the function representing the body of the expression.
    let fun = Emitter.emit(function: node.body, into: &builder)
    let ref = builder.buildFunRef(function: fun)

    // Emit the value of each captured declaration. Capture with `val` or `var` semantics are
    // copied from the environment, and so we must emit an r-value either way.
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

    /// Emits the body of a case in the current block.
    func emitCaseBody(body: BraceStmt) {
      if let storage = storage {
        let value = emit(rvalue: body.stmts[0] as! Expr)
        builder.buildStore(value, to: storage)
      } else {
        Emitter.emit(brace: body, in: &_state.pointee, into: &_builder.pointee)
      }
    }

    // If the node is a sub-expression, allocate storage for its "value".
    let storage: AllocStackInst? = node.isSubexpr
      ? builder.buildAllocStack(allocType: .lower(node.type))
      : nil

    // Emit the subject of the match as a borrowable address.
    let subjectLoc = emit(borrowable: node.subject)

    // Create a "sink" block where all cases will branch unconditionally.
    let sink = builder.buildBasicBlock()

    // Emit each case statement.
    let subjectType = node.subject.type.dealiased
    for stmt in node.cases {
      // Wildcard patterns are irrefutable.
      if stmt.pattern is WildcardPattern {
        assert(stmt.pattern.type == node.subject.type)
        emitCaseBody(body: stmt.body)
        break
      }

      if let pattern = stmt.pattern as? BindingPattern {
        // FIXME: Handle destructuring.
        precondition(pattern.namedPatterns.count == 1, "not implemented")
        let decl = pattern.namedPatterns[0].decl
        let patternType = pattern.type.dealiased

        // The pattern is irrefutable if it has the same type as the subject.
        if patternType == subjectType {
          if decl.isMutable {
            // `var` bindings are consuming.
            let loc = Emitter.emit(storedVar: decl, in: &_state.pointee, into: &_builder.pointee)
            locals[ObjectIdentifier(decl)] = loc
            builder.buildMoveAddr(from: subjectLoc, to: loc, range: pattern.range)
          } else {
            // `val` bindings are borrowing immutably.
            locals[ObjectIdentifier(decl)] = builder.buildBorrowAddr(
              source: subjectLoc, range: pattern.range)
          }

          emitCaseBody(body: stmt.body)
          builder.buildBranch(dest: sink)
          builder.insertionPointer!.blockID = sink
          break
        }

        // If the pattern has a different type, attempt to cast it.
        else {
          // If the subject is subtype of the pattern, the pattern is irrefutable.
          if subjectType.isSubtype(of: patternType) {
            assert(patternType.isExistential)
            if decl.isMutable {
              let loc = Emitter.emit(storedVar: decl, in: &_state.pointee, into: &_builder.pointee)
              locals[ObjectIdentifier(decl)] = loc
              let val = builder.buildCheckedCast(
                value: builder.buildLoad(source: subjectLoc, range: pattern.range),
                type: .lower(patternType),
                range: pattern.range)
              builder.buildStore(val, to: loc, range: pattern.range)
            } else {
              locals[ObjectIdentifier(decl)] = builder.buildPackBorrow(
                source: subjectLoc,
                type: .lower(patternType).address,
                range: pattern.range)
            }

            emitCaseBody(body: stmt.body)
            builder.buildBranch(dest: sink)
            builder.insertionPointer!.blockID = sink
            break
          }

          // If the pattern is not a subtype of the subject, skip to the next case.
          if !patternType.isSubtype(of: subjectType) {
            context.report(
              .dynamicCastAlwaysFails(
                from: node.subject.type, to: pattern.type, range: pattern.range))
            continue
          }

          // Runtime conversion of function types always fails.
          if (subjectType is FunType) && (patternType is FunType) {
            context.report(.runtimeFunctionTypeConversion(range: pattern.range))
            continue
          }

          let succ: BasicBlock.ID
          let next: BasicBlock.ID
          if decl.isMutable {
            // `var` bindings are consuming.
            succ = builder.buildBasicBlock(paramTypes: [.lower(patternType)])
            next = builder.buildBasicBlock(paramTypes: [subjectLoc.type.object])
            builder.buildCheckedCastBranch(
              value: builder.buildLoad(source: subjectLoc, range: pattern.range),
              type: .lower(patternType),
              succ: succ,
              fail: next,
              range: pattern.range)
          } else {
            // `val` bindings are borrowing immutably.
            succ = builder.buildBasicBlock(paramTypes: [.lower(patternType).address])
            next = builder.buildBasicBlock(paramTypes: [subjectLoc.type])
            builder.buildBorrowExistAddrBranch(
              container: subjectLoc,
              type: .lower(patternType).address,
              succ: succ,
              fail: next,
              range: pattern.range)
          }

          // If the match succeeds, the pattern becomes the argument of the "succ" block.
          builder.insertionPointer!.blockID = succ
          var bb = builder.currentFun!.blocks[succ]!
          if decl.isMutable {
            let loc = Emitter.emit(storedVar: decl, in: &_state.pointee, into: &_builder.pointee)
            locals[ObjectIdentifier(decl)] = loc
            builder.buildStore(bb.params[0], to: loc)
            emitCaseBody(body: stmt.body)
          } else {
            locals[ObjectIdentifier(decl)] = bb.params[0]
            emitCaseBody(body: stmt.body)
          }
          builder.buildBranch(dest: sink)

          // If the match fails, reassign `subjectLoc` if its value was consumed.
          builder.insertionPointer!.blockID = next
          if decl.isMutable {
            bb = builder.currentFun!.blocks[next]!
            builder.buildStore(bb.params[0], to: subjectLoc)
          }
        }
      }

      // FIXME: Handle complex conditional patterns recursively.
      fatalError("not implemented")
    }

    builder.buildBranch(dest: sink)
    builder.insertionPointer!.blockID = sink

    if let storage = storage {
      let result = builder.buildLoad(source: storage)
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
