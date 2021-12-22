import AST
import Basic

/// A VIL emitter for r-values.
struct RValueEmitter: ExprVisitor {

  typealias ExprResult = Result<Operand, EmitterError>

  /// The state in which the r-value is emitted.
  let _state: UnsafeMutablePointer<Emitter.State>

  /// The VIL module used by the emitter.
  let _module: UnsafeMutablePointer<Module>

  var locals: SymbolTable {
    get { state.locals }
    _modify { yield &state.locals }
  }

  var state: Emitter.State {
    get { _state.pointee }
    _modify { yield &_state.pointee }
  }

  var module: Module {
    get { _module.pointee }
    _modify { yield &_module.pointee }
  }

  var allocs: [InstIndex] {
    get { _state.pointee.allocs }
    _modify { yield &_state.pointee.allocs }
  }

  mutating func emit(
    assign rvalue: Operand,
    to target: Operand,
    state: inout Emitter.State,
    into module: inout Module,
    range: SourceRange? = nil
  ) {
    Emitter.emit(assign: rvalue, to: target, state: &state, into: &_module.pointee, range: range)
  }

  /// Emits an r-value.
  mutating func emit(rvalue expr: Expr) -> Operand {
    Emitter.emit(rvalue: expr, state: &_state.pointee, into: &_module.pointee)
  }

  /// Emits an l-value.
  mutating func emit(lvalue expr: Expr) -> Operand {
    Emitter.emit(lvalue: expr, state: &_state.pointee, into: &_module.pointee)
  }

  /// Emits the address of a cell holding the value of the specified expression.
  mutating func emit(borrowable expr: Expr) -> Operand {
    Emitter.emit(borrowable: expr, state: &_state.pointee, into: &_module.pointee)
  }

  // ----------------------------------------------------------------------------------------------
  // MARK: Visitors
  // ----------------------------------------------------------------------------------------------

  func visit(_ node: BoolLiteralExpr) -> ExprResult {
    fatalError("not implemented")
  }

  mutating func visit(_ node: IntLiteralExpr) -> ExprResult {
    // We can assume the expression's type conforms to `ExpressibleBy***Literal` (as type checking
    // succeeded). Therefore we can look for a conversion constructor `new(literal:)`.
    let view = module.context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral)!.instanceType
      as! ViewType
    let callee: Operand

    let type = node.type.dealiased
    switch type {
    case let type as NominalType:
      // The node has a concrete type; we can dispatch `new(literal:)` statically.
      let conformance = type.decl.conformanceTable[view]!
      let fun = module.getOrCreateFunction(from: conformance.entries[0].impl as! CtorDecl)
      callee = Operand(FunRef(function: fun))

    case is SkolemType:
      // The node has an skolem type; we have to dispatch dynamically.
      fatalError("not implemented")

    default:
      fatalError("unreachable")
    }

    // Emit a call to the constructor.
    let literal = IntLiteralValue(value: node.value, context: module.context)
    let apply = module.insertApply(
      callee: callee,
      args: [Operand(literal)],
      range: node.range,
      at: state.ip)
    return .success(Operand(apply))
  }

  func visit(_ node: FloatLiteralExpr) -> ExprResult {
    fatalError("not implemented")
  }

  func visit(_ node: StringLiteralExpr) -> ExprResult {
    fatalError("not implemented")
  }

  mutating func visit(_ node: AssignExpr) -> ExprResult {
    // Emit the right operand first and the left operand second.
    let rvalue = emit(rvalue: node.rvalue)
    let lvalue = emit(lvalue: node.lvalue)
    Emitter.emit(
      assign: rvalue,
      to: lvalue,
      state: &_state.pointee,
      into: &_module.pointee,
      range: node.range)

    // An assignment always results in a unit value.
    return .success(Operand(UnitValue(context: module.context)))
  }

  func visit(_ node: BaseCastExpr) -> ExprResult {
    fatalError("unreachable")
  }

  mutating func visit(_ node: DynCastExpr) -> ExprResult {
    guard let nodeType = node.type as? BoundGenericType
      else { preconditionFailure("dynamic cast must have a maybe type") }

    let lhsType = node.value.type.dealiased
    let rhsType = nodeType.args[0].dealiased

    // Allocate an existential container to hold the result of the cast.
    let container = Operand(module.insertAllocStack(
      allocType: .lower(nodeType), range: node.range, at: state.ip))
    defer { module.insertDeallocStack(alloc: container, at: state.ip) }

    // Emit the value to convert.
    let value: Operand
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
      module.context.report(.runtimeFunctionTypeConversion(range: node.range))
      module.insertDelete(value: value, at: state.ip)
      module.insertInitExistAddr(
        container: container,
        value: Operand(module.buildNil(at: state.ip)),
        at: state.ip)
      return .success(Operand(module.insertLoad(source: container, at: state.ip)))
    }

    // Cast the value.
    let succ = module.insertBasicBlock(paramTypes: [.lower(rhsType)], in: state.funName)
    let fail = module.insertBasicBlock(paramTypes: [.lower(lhsType)], in: state.funName)
    let tail = module.insertBasicBlock(in: state.funName)
    module.insertCheckedCastBranch(
      value: value,
      type: .lower(rhsType),
      succ: succ,
      fail: fail,
      range: node.range,
      at: state.ip)

    // If the cast succeeded ...
    state.ip = .endOf(succ)
    module.insertInitExistAddr(
      container: container, value: Operand(module.blocks[succ].params[0]), at: state.ip)
    module.insertBranch(dest: tail, at: state.ip)

    // If the cast failed ...
    state.ip = .endOf(fail)
    module.insertDelete(value: Operand(module.blocks[fail].params[0]), at: state.ip)
    module.insertInitExistAddr(
      container: container, value: Operand(module.buildNil(at: state.ip)), at: state.ip)
    module.insertBranch(dest: tail, at: state.ip)

    // Finally ...
    state.ip = .endOf(tail)
    return .success(Operand(module.insertLoad(source: container, at: state.ip)))
  }

  mutating func visit(_ node: UnsafeCastExpr) -> ExprResult {
    let lhsType = node.value.type.dealiased
    let rhsType = node.type.dealiased

    // Emit the value to convert.
    let value: Operand
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
      module.context.report(.runtimeFunctionTypeConversion(range: node.range))
      module.insertDelete(value: value, at: state.ip)
      module.insertCondFail(
        cond: Operand(IntValue.makeTrue(context: module.context)), range: node.range, at: state.ip)
      return .success(Operand(PoisonValue(type: .lower(rhsType))))
    }

    // Convert the value.
    let converted = module.insertCheckedCast(
      value: value, type: .lower(rhsType), range: node.range, at: state.ip)
    return .success(Operand(converted))
  }

  mutating func visit(_ node: TupleExpr) -> ExprResult {
    let tuple = module.insertTuple(
      type: node.type as! TupleType,
      operands: node.elems.map({ elem in emit(rvalue: elem.value) }), at: state.ip)
    return .success(Operand(tuple))
  }

  mutating func visit(_ node: CallExpr) -> ExprResult {
    let callee: Operand
    var args: [Operand] = []
    var argsRanges: [SourceRange?] = []
    var borrowedIndex: [Int] = []

    // Emit the function's callee.
    switch node.fun {
    case let expr as MemberDeclRefExpr where expr.decl.isMember:
      // The callee is a reference to a member declaration.
      if let decl = expr.decl as? BaseFunDecl {
        // The callee is a method; emit the receiver along with the dispatched function ref.
        argsRanges.append(expr.base.range)
        if decl.isConsuming {
          // The receiver is consumed; emit an r-value.
          args.append(emit(rvalue: expr.base))
          callee = expr.base.type.isExistential
            ? Operand(module.insertWitnessMethod(container: args[0], decl: decl, at: state.ip))
            : Operand(FunRef(function: module.getOrCreateFunction(from: decl)))
        } else {
          // The receiver is borrowed; emit a borrowable l-value.
          let receiver = decl.isMutating
            ? emit(lvalue: expr.base)
            : emit(borrowable: expr.base)
          let borrow = module.insertBorrowAddr(
            isMutable: decl.isMutating,
            source: receiver,
            range: expr.base.range,
            at: state.ip)
          borrowedIndex.append(args.count)
          args.append(Operand(borrow))
          callee = expr.base.type.isExistential
            ? Operand(module.insertWitnessMethod(container: args[0], decl: decl, at: state.ip))
            : Operand(FunRef(function: module.getOrCreateFunction(from: decl)))
        }
      } else {
        // The callee is a functional property; emit a regular member access.
        callee = emit(borrowable: node.fun)
      }

    case let expr as DeclRefExpr:
      switch expr.decl {
      case let decl as FunDecl where decl.isBuiltin:
        // The callee is a reference to a built-in function.
        callee = Operand(BuiltinFunRef(decl: decl))

      case let decl as BaseFunDecl
        where (decl is CtorDecl) || decl.computeAllCaptures().isEmpty:
        // The callee is a reference to a thin function.
        callee = Operand(FunRef(function: module.getOrCreateFunction(from: decl)))

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
      argsRanges.append(node.args[i].value.range)
      switch params[i].policy! {
      case .local:
        // Local and mutating parameters are passed by reference; emit a borrowable l-value.
        let source = emit(borrowable: node.args[i].value)
        let borrow = module.insertBorrowAddr(
          isMutable: false,
          source: source,
          range: node.args[i].value.range,
          at: state.ip)
        borrowedIndex.append(args.count)
        args.append(Operand(borrow))

      case .inout:
        let source = emit(lvalue: node.args[i].value)
        let borrow = module.insertBorrowAddr(
          isMutable: true,
          source: source,
          range: node.args[i].value.range,
          at: state.ip)
        borrowedIndex.append(args.count)
        args.append(Operand(borrow))

      case .consuming:
        // Consuming parameters are passed by value.
        args.append(emit(rvalue: node.args[i].value))
      }

      // If the parameter has an existential type but the argument doesn't, the latter has to be
      // wrapped before being passed.
      if params[i].type.isExistential && !node.args[i].value.type.isExistential {
        assert(params[i].policy != .inout)
        switch params[i].policy! {
        case .local:
          args[i] = Operand(module.insertPackBorrow(
            source: args[i],
            type: module.type(of: callee).paramType(at: i).address,
            range: node.args[i].value.range,
            at: state.ip))

        case .consuming:
          let alloc = module.insertAllocStack(
            allocType: module.type(of: callee).paramType(at: i), at: state.ip)
          allocs.append(alloc)
          module.insertInitExistAddr(
            container: Operand(alloc), value: args[i], range: node.args[i].range, at: state.ip)
          args[i] = Operand(alloc)

        case .inout:
          // Arguments passed mutating must have the same type.
          fatalError("unreachable")
        }
      }
    }

    // Emit the call.
    let apply = module.insertApply(
      callee: callee,
      args: args,
      range: node.range,
      argsRanges: argsRanges,
      at: state.ip)

    // End the borrowed values.
    for i in borrowedIndex.reversed() {
      module.insertEndBorrowAddr(source: args[i], range: node.range, at: state.ip)
    }

    return .success(Operand(apply))
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
      return module.type(of: value).isAddress
        ? .success(Operand(module.insertLoad(source: value, range: node.range, at: state.ip)))
        : .success(value)
    }

    // If the identifier refers to a function, wrap a reference into a thick function container.
    // Note: the referred function must be thin or it would have already been found in the local
    // symbol table above.
    if let decl = node.decl as? BaseFunDecl {
      assert(!decl.isBuiltin, "cannot wrap built-in function into a closure")
      let ref = FunRef(function: module.getOrCreateFunction(from: decl))
      let thk = module.insertThinToThick(ref: ref, at: state.ip)
      return .success(Operand(thk))
    }

    // FIXME: Handle computed properties.
    fatalError("not implemented")
  }

  func visit(_ node: TypeDeclRefExpr) -> ExprResult {
    fatalError()
  }

  mutating func visit(_ node: MemberDeclRefExpr) -> ExprResult {
    // Emit the base value.
    let base: Operand
    switch node.base.accept(&self) {
    case .success(let val):
      base = val
    case let failure:
      return failure
    }

    if let decl = node.decl as? VarDecl, decl.hasStorage {
      // If the base is a tuple, extract the selected member and end destroy the other ones.
      // Extracting stored properties from other data types is illegal.
      guard node.base.type.dealiased is TupleType else {
        return .failure(.moveOfStoredProperty(decl))
      }

      let member = module.insertRecordMember(
        record: base, memberDecl: decl, type: .lower(node.type), range: node.range, at: state.ip)
      // return .success(Operand(member))
      fatalError("not implemented")
    }

    fatalError("not implemented")
  }

  func visit(_ node: TupleMemberExpr) -> ExprResult {
    fatalError("not implemented")
  }

  mutating func visit(_ node: AsyncExpr) -> ExprResult {
    // Emit the function representing the body of the expression.
    let fun = Emitter.emit(function: node.body, into: &module)
    let ref = FunRef(function: fun)

    // Emit the value of each captured declaration.
    let captureTable = node.body.computeAllCaptures()
    var captures: [Operand] = []
    for (key, value) in captureTable {
      // FIXME: locate captures more precisely.
      let expr = DeclRefExpr(decl: key.decl, type: value.type)
      expr.range = node.range

      switch value.policy {
      case .local:
        // Capture is borrowed.
        let source = emit(borrowable: expr)
        captures.append(Operand(module.insertBorrowAddr(
          isMutable: false, source: source, range: expr.range, at: state.ip)))

      case .inout:
        let source = emit(lvalue: expr)
        captures.append(Operand(module.insertBorrowAddr(
          isMutable: true, source: source, range: expr.range, at: state.ip)))

      case .consuming:
        // Capture is consumed.
        captures.append(emit(rvalue: expr))
      }
    }

    let future = module.insertAsync(
      ref: ref,
      captures: captures,
      range: node.range,
      captureRanges: Array(repeating: node.range, count: captureTable.count),
      at: state.ip)
    return .success(Operand(future))
  }

  mutating func visit(_ node: AwaitExpr) -> ExprResult {
    let awaited = emit(rvalue: node.value)
    return .success(Operand(module.insertAwait(value: awaited, range: node.range, at: state.ip)))
  }

  mutating func visit(_ node: AddrOfExpr) -> ExprResult {
    return .success(emit(lvalue: node.value))
  }

  mutating func visit(_ node: MatchExpr) -> ExprResult {
    assert(!node.cases.isEmpty)

    // If the node is a sub-expression, allocate storage for its "value".
    let storage: InstIndex? = node.isSubexpr
      ? module.insertAllocStack(allocType: .lower(node.type), at: state.ip)
      : nil

    /// Emits the body of a case in the current block.
    func emitCaseBody(body: BraceStmt) {
      if let storage = storage {
        let value = emit(rvalue: body.stmts[0] as! Expr)
        module.insertStore(value, to: Operand(storage), at: state.ip)
      } else {
        Emitter.emit(brace: body, state: &_state.pointee, into: &_module.pointee)
      }
    }

    // Emit the subject of the match as a borrowable address.
    let subjectLoc = emit(borrowable: node.subject)

    // Create a "sink" block where all cases will branch unconditionally.
    let sink = module.insertBasicBlock(in: state.funName)

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
            // Mutable bindings are consuming.
            let loc = Operand(Emitter.emit(
              storedVar: decl, state: &_state.pointee, into: &_module.pointee))
            locals[ObjectIdentifier(decl)] = loc
            module.insertMoveAddr(from: subjectLoc, to: loc, range: pattern.range, at: state.ip)
          } else {
            // Immutable bindings are borrowing immutably.
            let borrow = module.insertBorrowAddr(
              source: subjectLoc, range: pattern.range, at: state.ip)
            locals[ObjectIdentifier(decl)] = Operand(borrow)
          }

          emitCaseBody(body: stmt.body)
          module.insertBranch(dest: sink, at: state.ip)
          state.ip = .endOf(sink)
          break
        }

        // If the pattern has a different type, attempt to cast it.
        else {
          // If the subject is subtype of the pattern, the pattern is irrefutable.
          if subjectType.isSubtype(of: patternType) {
            assert(patternType.isExistential)
            if decl.isMutable {
              let loc = Operand(Emitter.emit(
                storedVar: decl, state: &_state.pointee, into: &_module.pointee))
              locals[ObjectIdentifier(decl)] = loc
              let val = module.insertCheckedCast(
                value: Operand(
                  module.insertLoad(source: subjectLoc, range: pattern.range, at: state.ip)),
                type: .lower(patternType),
                range: pattern.range,
                at: state.ip)
              module.insertStore(Operand(val), to: loc, range: pattern.range, at: state.ip)
            } else {
              locals[ObjectIdentifier(decl)] = Operand(module.insertPackBorrow(
                source: subjectLoc,
                type: .lower(patternType).address,
                range: pattern.range,
                at: state.ip))
            }

            emitCaseBody(body: stmt.body)
            module.insertBranch(dest: sink, at: state.ip)
            state.ip = .endOf(sink)
            break
          }

          // If the pattern is not a subtype of the subject, skip to the next case.
          if !patternType.isSubtype(of: subjectType) {
            module.context.report(.dynamicCastAlwaysFails(
              from: node.subject.type, to: pattern.type, range: pattern.range))
            continue
          }

          // Runtime conversion of function types always fails.
          if (subjectType is FunType) && (patternType is FunType) {
            module.context.report(.runtimeFunctionTypeConversion(range: pattern.range))
            continue
          }

          let succ: BasicBlockIndex
          let next: BasicBlockIndex
          if decl.isMutable {
            // `var` bindings are consuming.
            succ = module.insertBasicBlock(
              paramTypes: [.lower(patternType)], in: state.funName)
            next = module.insertBasicBlock(
              paramTypes: [.lower(node.subject.type)], in: state.funName)
            module.insertCheckedCastBranch(
              value: Operand(
                module.insertLoad(source: subjectLoc, range: pattern.range, at: state.ip)),
              type: .lower(patternType),
              succ: succ,
              fail: next,
              range: pattern.range,
              at: state.ip)
          } else {
            // `val` bindings are borrowing immutably.
            succ = module.insertBasicBlock(
              paramTypes: [.lower(patternType).address], in: state.funName)
            next = module.insertBasicBlock(
              paramTypes: [.lower(node.subject.type).address], in: state.funName)
            module.insertBorrowExistAddrBranch(
              container: subjectLoc,
              type: .lower(patternType).address,
              succ: succ,
              fail: next,
              range: pattern.range,
              at: state.ip)
          }

          // If the match succeeds, the pattern becomes the argument of the "succ" block.
          state.ip = .endOf(succ)
          if decl.isMutable {
            let loc = Operand(Emitter.emit(
              storedVar: decl, state: &_state.pointee, into: &_module.pointee))
            locals[ObjectIdentifier(decl)] = loc
            module.insertStore(Operand(module.blocks[succ].params[0]), to: loc, at: state.ip)
            emitCaseBody(body: stmt.body)
          } else {
            locals[ObjectIdentifier(decl)] = Operand(module.blocks[succ].params[0])
            emitCaseBody(body: stmt.body)
          }
          module.insertBranch(dest: sink, at: state.ip)

          // If the match fails, reassign `subjectLoc` if its value was consumed.
          state.ip = .endOf(next)
          if decl.isMutable {
            module.insertStore(
              Operand(module.blocks[succ].params[0]), to: subjectLoc, at: state.ip)
          }
        }
      }

      // FIXME: Handle complex conditional patterns recursively.
      fatalError("not implemented")
    }

    module.insertBranch(dest: sink, at: state.ip)
    state.ip = .endOf(sink)

    if let storage = storage {
      let result = module.insertLoad(source: Operand(storage), at: state.ip)
      module.insertDeallocStack(alloc: Operand(storage), at: state.ip)
      return .success(Operand(result))
    } else {
      return .success(Operand(UnitValue(context: module.context)))
    }
  }

  func visit(_ node: WildcardExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: ErrorExpr) -> ExprResult {
    fatalError()
  }

}
