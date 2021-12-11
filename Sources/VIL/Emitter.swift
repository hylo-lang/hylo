import AST
import Basic

/// A VIL emitter.
///
/// VIL generation consists of transforming the high-level representation of Val (i.e., AST nodes)
/// into a lower-level language, better suited for data-flow analysis and code optimizations.
public enum Emitter {

  /// The internal state of the emitter.
  public struct State {

    /// The declaration of the function being emitted.
    let funDecl: BaseFunDecl

    /// A symbol table that maps locally visible declarations to their emitted value, populated by
    /// function parameters and local pattern binding declarations.
    var locals: SymbolTable

    /// A list containing the stack allocations that have been built in the current scope and must
    /// be balanced by a deallocation.
    var allocs: [AllocStackInst]

    /// A Boolean value that indicates whether the emitter encountered an error.
    var hasError = false

  }

  /// Lowers to VIL a module declaration into the specified builder.
  ///
  /// This method is the entry point to VIL code generation. It lowers a module declaration to its
  /// VIL representation, inserting VIL code into the module associated with the given builder.
  ///
  /// - Parameter decl: A module declaration. `decl` must be type checked: all symbols must be
  ///   resolved or substituted by error nodes.
  public static func emit(module decl: ModuleDecl) -> Module {
    var builder = Builder(module: Module(id: decl.name), context: decl.type.context)
    for decl in decl {
      emit(topLevel: decl, into: &builder)
    }
    return builder.module
  }

  /// Lowers the given "top-level" declaration.
  public static func emit(topLevel decl: Decl, into builder: inout Builder) {
    switch decl {
    case is ModuleDecl:
      fatalError("unreachable")
    case is ImportDecl:
      fatalError("not implemented")
    case is PatternBindingDecl:
      fatalError("not implemented")
    case let decl as BaseFunDecl:
      _ = emit(function: decl, into: &builder)
    case let decl as ProductTypeDecl:
      emit(productType: decl, into: &builder)
    case let decl as TypeExtnDecl:
      emit(typeExtn: decl, into: &builder)

    case is ViewTypeDecl, is AbstractTypeDecl:
      // Views and abstract types are not concrete; there's nothing to emit.
      break

    case is AliasTypeDecl:
      // FIXME: Emit the type's witness table.
      break

    default:
      fatalError("unreachable")
    }
  }

  public static func emit(member decl: Decl, into builder: inout Builder) {
    switch decl {
    case let decl as PatternBindingDecl:
      emit(property: decl, into: &builder)
    default:
      emit(topLevel: decl, into: &builder)
    }
  }

  public static func emit(property decl: PatternBindingDecl, into builder: inout Builder) {
    assert(decl.isMember)

    // FIXME: Emit computed properties and handle initializers in synthesized constructors.
    assert(decl.varDecls.isEmpty || decl.varDecls[0].hasStorage)
    assert(decl.initializer == nil)
  }

  public static func emit(productType decl: ProductTypeDecl, into builder: inout Builder) {
    // Emit the the type's witness table(s).
    for conformance in decl.conformanceTable.values {
      var entries: [(decl: BaseFunDecl, impl: VILFun)] = []
      for (req, impl) in conformance.entries {
        if let reqFunDecl = req as? BaseFunDecl {
          let implFunDecl = impl as! BaseFunDecl
          let fun = emit(witness: implFunDecl, for: reqFunDecl, into: &builder)
          entries.append((reqFunDecl, fun))
        }
      }

      let table = ViewWitnessTable(
        type: decl.instanceType as! NominalType, view: conformance.viewType, entries: entries)
      builder.module.viewWitnessTables.append(table)
    }

    // Emit the direct members of the declaration.
    for member in decl.members {
      emit(member: member, into: &builder)
    }
  }

  public static func emit(typeExtn decl: TypeExtnDecl, into builder: inout Builder) {
    for member in decl.members {
      emit(member: member, into: &builder)
    }
  }

  /// Emits a function.
  public static func emit(function decl: BaseFunDecl, into builder: inout Builder) -> VILFun {
    // Create (i.e., declare) the function in the module.
    let fun = builder.getOrCreateFunction(from: decl)

    // We're done if the function doesn't have body.
    if (decl.body == nil) && !decl.isSynthesized { return fun }

    var state = State(funDecl: decl, locals: [:], allocs: [])
    let oldIP = builder.insertionPointer
    defer { builder.insertionPointer = oldIP }
    builder.insertionPointer = InsertionPointer(funName: fun.name)

    // Contextualize the function's arguments.
    let genericEnv = decl.genericEnv!
    let paramTypes = fun.type.params!.map({ (param) -> VILType in
      let ty = VILType.lower(param.type).contextualized(in: genericEnv, from: decl)
      switch param.policy! {
      case .local, .inout:
        return ty.address
      case .consuming, .consumingMutable:
        return ty
      }
    })

    // Create the function's entry block.
    builder.insertionPointer!.blockID = builder.buildBasicBlock(
      paramTypes: paramTypes, isEntry: true)
    var params = builder.currentFun!.entry!.params

    // If the function has a declaration for `self`, it must be either a method or a constructor.
    // In both cases, we must register the receiver in the local symbol table.
    if let selfDecl = decl.selfDecl {
      let (selfType, _) = genericEnv.contextualize(selfDecl.type, from: decl)
      if decl.isMember {
        // Member functions accept their receiver as an implicit parameter.
        state.locals[ObjectIdentifier(selfDecl)] = params[0]
        params.removeFirst()
      } else {
        // Constructors should allocate `self`.
        assert(decl is CtorDecl)
        let loc = builder.buildAllocStack(allocType: .lower(selfType), isReceiver: true)
        state.locals[ObjectIdentifier(selfDecl)] = loc
      }
    }

    // Register the function's formal parameters in the local symbol table.
    let captureTable = decl.computeAllCaptures()
    for (capture, param) in zip(captureTable, params) {
      state.locals[ObjectIdentifier(capture.value.referredDecl)] = param
    }
    for (paramDecl, param) in zip(decl.params, params[captureTable.count...]) {
      state.locals[ObjectIdentifier(paramDecl)] = param
    }

    // Emit the function's body.
    guard let body = decl.body else {
      emit(synthesizedBodyOf: decl, locals: state.locals, into: &builder)
      return fun
    }
    emit(brace: body, in: &state, into: &builder)

    // Emit the function's epilogue.
    if decl is CtorDecl {
      // If the function's a constructor, emit the implicit return statement.
      let selfLoc = state.locals[ObjectIdentifier(decl.selfDecl!)]
      let selfVal = builder.buildLoad(source: selfLoc!)
      builder.buildDeallocStack(alloc: selfLoc as! AllocStackInst)
      builder.buildRet(value: selfVal)
    } else {
      let funType = decl.type as! FunType

      switch funType.retType {
      case funType.context.nothingType:
        // If the function never returns, emit a halt statement.
        builder.buildHalt()

      case funType.context.unitType:
        // The function returns "unit".
        builder.buildRet(value: builder.buildUnit())

      default:
        break
      }
    }

    return fun
  }

  /// Emits the body of a synthesized function declaration.
  static func emit(
    synthesizedBodyOf decl: BaseFunDecl,
    locals: SymbolTable,
    into builder: inout Builder
  ) {
    assert(decl.isSynthesized)

    switch decl {
    case is CtorDecl:
      // Emit a synthesized constructor.
      let base = locals[ObjectIdentifier(decl.selfDecl!)] as! AllocStackInst
      let type = decl.parentDeclSpace as! NominalTypeDecl

      for (varDecl, paramDecl) in zip(type.storedVars, decl.params) {
        let memberAddr = builder.buildRecordMemberAddr(
          record: base, memberDecl: varDecl, type: VILType.lower(varDecl.type).address)
        let value = locals[ObjectIdentifier(paramDecl)]!
        builder.buildStore(value, to: memberAddr)
      }

      let selfVal = builder.buildLoad(source: base)
      builder.buildDeallocStack(alloc: base)
      builder.buildRet(value: selfVal)

    default:
      preconditionFailure("unexpected synthesized declaration '\(decl.name)'")
    }
  }

  /// Emits a function wrapping the implementation satisfying a conformance requirement.
  ///
  /// - Parameters:
  ///   - impl: The declaration of the function that implements the requirement.
  ///   - req: The declaration of the function requirement.
  ///   - builder: A VIL builder.
  static func emit(
    witness impl: BaseFunDecl,
    for req: BaseFunDecl,
    into builder: inout Builder
  ) -> VILFun {
    // Create the VIL function object.
    var mangler = Mangler()
    mangler.append(witnessImpl: impl, for: req)
    let name = VILName(mangler.finalize())
    let fun = builder.getOrCreateFunction(name: name, type: req.unappliedType as! FunType)

    // Create the function's entry point.
    builder.insertionPointer = InsertionPointer(funName: name)
    builder.insertionPointer!.blockID = builder.buildBasicBlock(
      paramTypes: fun.type.params!.map({ .lower($0.type) }),
      isEntry: true)
    let params = builder.currentFun!.entry!.params

    // Emit the function's body.
    var args: [Value] = params
    if !(req is CtorDecl) {
      // Unless the function is a constructor, we have to open the self parameter.
      let openedSelfType = impl.selfDecl!.type
      args[0] = builder.buildBorrowExistAddr(
        isMutable: impl.isMutating, container: args[0], type: .lower(openedSelfType))
    }

    let openedFun = builder.getOrCreateFunction(from: impl)
    let ret = builder.buildApply(callee: builder.buildFunRef(function: openedFun), args: args)
    builder.buildRet(value: ret)

    return fun
  }

  /// Emits a local pattern binding declaration.
  static func emit(
    binding decl: PatternBindingDecl,
    in state: inout State,
    into builder: inout Builder
  ) {
    // FIXME: implement local computed properties.
    assert(decl.varDecls.first?.hasStorage ?? true, "not implemented")

    let patterns = decl.pattern.namedPatterns
    precondition(patterns.count == 1, "not implemented") // FIXME: destructuring

    if !decl.isMutable {
      // Local immutable bindings require an initializer.
      guard let initializer = decl.initializer else {
        builder.context.report(.immutableBindingRequiresInitializer(decl: decl))
        state.locals[ObjectIdentifier(patterns[0].decl)] = builder.buildPoison()
        state.hasError = true
        return
      }

      // If the initializer is an l-value, the binding must be initialized as a borrowed reference.
      // Otherwise, it requires storage to receive ownership, just as mutable bindings.
      if case .success(let loc) = withLValueEmitter(
        in: &state, into: &builder, do: initializer.accept)
      {
        let loc = builder.buildBorrowAddr(
          isMutable: false,
          source: loc,
          range: initializer.range)
        state.locals[ObjectIdentifier(patterns[0])] = loc
        return
      }
    }

    let loc = emit(storedVar: patterns[0].decl, in: &state, into: &builder)
    state.locals[ObjectIdentifier(patterns[0].decl)] = loc
    if let initializer = decl.initializer {
      emit(
        assign: emit(rvalue: initializer, in: &state, into: &builder),
        to: loc,
        into: &builder,
        range: initializer.range)
    }
  }

  /// Emits the declaration of a stored local variable.
  static func emit(
    storedVar decl: VarDecl,
    in state: inout State,
    into builder: inout Builder
  ) -> AllocStackInst {
    assert(decl.hasStorage)
    assert(decl.state == .typeChecked)

    // Allocate storage on the stack for the variable.
    let loc = builder.buildAllocStack(allocType: .lower(decl.type), decl: decl)
    state.allocs.append(loc)
    return loc
  }

  static func emit(
    brace: BraceStmt,
    in state: inout State,
    into builder: inout Builder
  ) {
    assert(builder.insertionPointer != nil, "insertion block not configured")

    for i in 0 ..< brace.stmts.count {
      switch brace.stmts[i] {
      case let decl as PatternBindingDecl:
        emit(binding: decl, in: &state, into: &builder)

      case let decl as FunDecl:
        _ = emit(function: decl, into: &builder)

        // Emit the value of each captured declaration. Capture with `val` or `var` semantics are
        // copied from the environment, and so we must emit an r-value either way.
        let captureTable = decl.computeAllCaptures()
        let partialArgs = captureTable.map({ (key, value) -> Value in
          // FIXME: Implement capture-by-reference (requires local bindings).
          assert(value.semantics != .mut, "not implemented")
          let expr = DeclRefExpr(decl: key.capturedDecl, type: value.type)
          return emit(rvalue: expr, in: &state, into: &builder)
        })

        // Local function with capture declarations require stack allocation.
        if !partialArgs.isEmpty {
          let fun = builder.buildFunRef(function: builder.getOrCreateFunction(from: decl))
          let loc = builder.buildAllocStack(allocType: .lower(decl.type), decl: decl)
          state.locals[ObjectIdentifier(decl)] = loc
          state.allocs.append(loc)

          builder.buildStore(
            builder.buildPartialApply(delegator: fun, partialArgs: partialArgs),
            to: loc,
            range: decl.range)
        }

      case let decl as NominalTypeDecl:
        emit(topLevel: decl, into: &builder)

      case let stmt as BraceStmt:
        emit(brace: stmt, in: &state, into: &builder)

      case let stmt as RetStmt:
        emit(stmt: stmt, in: &state, into: &builder)
        if i > brace.stmts.count - 1 {
          let context = state.funDecl.type.context
          context.report(.codeAfterReturnNeverExecuted(range: brace.stmts[i + 1].range))
          return
        }

      case let expr as Expr:
        // FIXME: Drop the result of the expression.
        _ = emit(rvalue: expr, in: &state, into: &builder)

      default:
        fatalError("unreachable")
      }
    }

    // Deallocate the `alloc_stack`s in scope.
    while let loc = state.allocs.popLast() {
      builder.buildDeallocStack(alloc: loc)
    }
  }

  static func emit(
    stmt: RetStmt,
    in state: inout State,
    into builder: inout Builder
  ) {
    let result: Value
    if let expr = stmt.value {
      result = emit(rvalue: expr, in: &state, into: &builder)
    } else {
      result = builder.buildUnit()
    }

    builder.buildRet(value: result, range: stmt.range)
  }

  /// Emits the assignment of `rvalue` to `target`.
  static func emit(
    assign rvalue: Value,
    to target: Value,
    into builder: inout Builder,
    range: SourceRange? = nil
  ) {
    // Assuming type checking succeeded, we know that the LHS can't have an existential layout
    // unless the RHS does too.
    if target.type.isExistential && !rvalue.type.isExistential {
      builder.buildInitExistAddr(container: target, value: rvalue, range: range)
    } else {
      assert(!target.type.isExistential || rvalue.type.isExistential)
      builder.buildStore(rvalue, to: target, range: range)
    }
  }

  /// Emits an r-value.
  static func emit(
    rvalue expr: Expr,
    in state: inout State,
    into builder: inout Builder
  ) -> Value {
    // Emit a poison value for any expression that has an error type.
    guard !expr.type.isError else { return builder.buildPoison() }

    let result = withRValueEmitter(in: &state, into: &builder, do: expr.accept)
    switch result {
    case .success(let val):
      return val

    case .failure(let error):
      builder.context.report(error.diag())
      state.hasError = true
      return builder.buildPoison()
    }
  }

  /// Emits an l-value.
  static func emit(
    lvalue expr: Expr,
    in state: inout State,
    into builder: inout Builder
  ) -> Value {
    // Emit a poison value for any expression that has an error type.
    guard !expr.type.isError else { return builder.buildPoison() }

    let result = withLValueEmitter(in: &state, into: &builder, do: expr.accept)
    switch result {
    case .success(let loc):
      return loc

    case .failure(let error):
      builder.context.report(error.diag())
      state.hasError = true
      return builder.buildPoison()
    }
  }

  /// Emits the address of a cell holding the value of the specified expression.
  static func emit(
    borrowable expr: Expr,
    in state: inout State,
    into builder: inout Builder
  ) -> Value {
    if case .success(let loc) = withLValueEmitter(in: &state, into: &builder, do: expr.accept) {
      return loc
    } else {
      let val = emit(rvalue: expr, in: &state, into: &builder)
      let loc = builder.buildAllocStack(allocType: .lower(expr.type), range: expr.range)
      state.allocs.append(loc)
      builder.buildStore(val, to: loc, range: expr.range)
      return loc
    }
  }

  static func withRValueEmitter<T>(
    in state: inout State,
    into builder: inout Builder,
    do action: (inout RValueEmitter) -> T
  ) -> T {
    return (
      withUnsafeMutablePointer(to: &state, { (_state) -> T in
      withUnsafeMutablePointer(to: &builder, { (_builder) -> T in
        var emitter = RValueEmitter(_state: _state, _builder: _builder)
        return action(&emitter)
      })
      }))
  }

  static func withLValueEmitter<T>(
    in state: inout State,
    into builder: inout Builder,
    do action: (inout LValueEmitter) -> T
  ) -> T {
    return (
      withUnsafeMutablePointer(to: &state, { (_state) -> T in
      withUnsafeMutablePointer(to: &builder, { (_builder) -> T in
        var emitter = LValueEmitter(_state: _state, _builder: _builder)
        return action(&emitter)
      })
      }))
  }

}
