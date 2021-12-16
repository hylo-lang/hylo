import AST
import Basic
import Darwin

/// A symbol table.
typealias SymbolTable = [ObjectIdentifier: Operand]

/// A VIL emitter.
///
/// VIL generation consists of transforming the high-level representation of Val (i.e., AST nodes)
/// into a lower-level language, better suited for data-flow analysis and code optimizations.
public enum Emitter {

  /// The internal state of the emitter.
  public struct State {

    /// The declaration of the function being emitted.
    let funDecl: BaseFunDecl

    /// The mangled name of the function being emitted.
    let funName: String

    /// The current insertion point.
    var ip: InsertionPoint

    /// A symbol table that maps locally visible declarations to their emitted value, populated by
    /// function parameters and local pattern binding declarations.
    var locals: SymbolTable = [:]

    /// A list containing the stack allocations that have been built in the current scope and must
    /// be balanced by a deallocation.
    var allocs: [InstIndex] = []

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
    var module = Module(id: decl.name, context: decl.type.context)
    for decl in decl {
      emit(topLevel: decl, into: &module)
    }
    return module
  }

  /// Lowers the given "top-level" declaration.
  public static func emit(topLevel decl: Decl, into module: inout Module) {
    switch decl {
    case is ModuleDecl:
      fatalError("unreachable")
    case is ImportDecl:
      fatalError("not implemented")
    case is PatternBindingDecl:
      fatalError("not implemented")
    case let decl as BaseFunDecl:
      _ = emit(function: decl, into: &module)
    case let decl as ProductTypeDecl:
      emit(productType: decl, into: &module)
    case let decl as TypeExtnDecl:
      emit(typeExtn: decl, into: &module)

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

  public static func emit(member decl: Decl, into module: inout Module) {
    switch decl {
    case let decl as PatternBindingDecl:
      emit(property: decl, into: &module)
    default:
      emit(topLevel: decl, into: &module)
    }
  }

  public static func emit(property decl: PatternBindingDecl, into module: inout Module) {
    assert(decl.isMember)

    // FIXME: Emit computed properties and handle initializers in synthesized constructors.
    assert(decl.varDecls.isEmpty || decl.varDecls[0].hasStorage)
    assert(decl.initializer == nil)
  }

  public static func emit(productType decl: ProductTypeDecl, into module: inout Module) {
    // Emit the the type's witness table(s).
    for conformance in decl.conformanceTable.values {
      var entries: [(decl: BaseFunDecl, impl: VILFun)] = []
      for (req, impl) in conformance.entries {
        if let reqFunDecl = req as? BaseFunDecl {
          let implFunDecl = impl as! BaseFunDecl
          let fun = emit(witness: implFunDecl, for: reqFunDecl, into: &module)
          entries.append((reqFunDecl, fun))
        }
      }

      let table = ViewWitnessTable(
        type: decl.instanceType as! NominalType, view: conformance.viewType, entries: entries)
      module.viewWitnessTables.append(table)
    }

    // Emit the direct members of the declaration.
    for member in decl.members {
      emit(member: member, into: &module)
    }
  }

  public static func emit(typeExtn decl: TypeExtnDecl, into module: inout Module) {
    for member in decl.members {
      emit(member: member, into: &module)
    }
  }

  /// Emits a function.
  public static func emit(function decl: BaseFunDecl, into module: inout Module) -> VILFun {
    // Create (i.e., declare) the function in the module.
    let fun = module.getOrCreateFunction(from: decl)

    // We're done if the function doesn't have body.
    if (decl.body == nil) && !decl.isSynthesized { return fun }

    // Contextualize the function's arguments.
    let genericEnv = decl.genericEnv!
    let paramTypes = fun.type.params!.map({ (param) -> VILType in
      let ty = VILType.lower(param.type).contextualized(in: genericEnv, from: decl)
      switch param.policy! {
      case .local, .inout:
        return ty.address
      case .consuming:
        return ty
      }
    })

    // Create the function's entry block.
    let entry = module.insertBasicBlock(paramTypes: paramTypes, in: fun.name, isEntry: true)
    var params = module.blocks[entry].params
    var state = State(funDecl: decl, funName: fun.name, ip: .endOf(entry))

    // If the function has a declaration for `self`, it must be either a method or a constructor.
    // In both cases, we must register the receiver in the local symbol table.
    if let selfDecl = decl.selfDecl {
      let (selfType, _) = genericEnv.contextualize(selfDecl.type, from: decl)
      if decl.isMember {
        // Member functions accept their receiver as an implicit parameter.
        state.locals[ObjectIdentifier(selfDecl)] = Operand(params[0])
        params.removeFirst()
      } else {
        // Constructors should allocate `self`.
        assert(decl is CtorDecl)
        let alloc = module.insertAllocStack(
          allocType: .lower(selfType), isReceiver: true, at: state.ip)
        state.locals[ObjectIdentifier(selfDecl)] = Operand(alloc)
      }
    }

    // Register the function's formal parameters in the local symbol table.
    let captureTable = decl.computeAllCaptures()
    for (capture, param) in zip(captureTable, params) {
      state.locals[ObjectIdentifier(capture.value.referredDecl)] = Operand(param)
    }
    for (paramDecl, param) in zip(decl.params, params[captureTable.count...]) {
      state.locals[ObjectIdentifier(paramDecl)] = Operand(param)
    }

    // Emit the function's body.
    guard let body = decl.body else {
      emit(synthesizedBodyOf: decl, state: state, into: &module)
      module.functions[fun.name]!.stage.insert(.optimized)
      return fun
    }
    emit(brace: body, state: &state, into: &module)

    // Emit the function's epilogue.
    if decl is CtorDecl {
      // If the function's a constructor, emit the implicit return statement.
      let alloc = state.locals[ObjectIdentifier(decl.selfDecl!)]!
      let value = module.insertLoad(source: alloc, at: state.ip)
      module.insertDeallocStack(alloc: alloc, at: state.ip)
      module.insertRet(value: Operand(value), at: state.ip)
    } else {
      let funType = decl.type as! FunType

      switch funType.retType {
      case funType.context.nothingType:
        // If the function never returns, emit a halt statement.
        module.insertHalt(at: state.ip)

      case funType.context.unitType:
        // The function returns "unit".
        let unit = UnitValue(context: module.context)
        module.insertRet(value: Operand(unit), at: state.ip)

      default:
        break
      }
    }

    return fun
  }

  /// Emits the body of a synthesized function declaration.
  static func emit(synthesizedBodyOf decl: BaseFunDecl, state: State, into module: inout Module) {
    assert(decl.isSynthesized)

    switch decl {
    case is CtorDecl:
      // Emit a synthesized constructor.
      let alloc = state.locals[ObjectIdentifier(decl.selfDecl!)]!
      let type = decl.parentDeclSpace as! NominalTypeDecl

      for (varDecl, paramDecl) in zip(type.storedVars, decl.params) {
        let memberAddr = module.insertRecordMemberAddr(
          record: alloc,
          memberDecl: varDecl,
          type: .lower(varDecl.type).address,
          at: state.ip)
        let value = state.locals[ObjectIdentifier(paramDecl)]!
        module.insertStore(value, to: Operand(memberAddr), at: state.ip)
      }

      let value = module.insertLoad(source: alloc, at: state.ip)
      module.insertDeallocStack(alloc: alloc, at: state.ip)
      module.insertRet(value: Operand(value), at: state.ip)

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
    into module: inout Module
  ) -> VILFun {
    // Create the VIL function object.
    var mangler = Mangler()
    mangler.append(witnessImpl: impl, for: req)
    let name = mangler.finalize()
    let fun = module.getOrCreateFunction(name: name, type: req.unappliedType as! FunType)

    // Create the function's entry point.
    let entry = module.insertBasicBlock(
      paramTypes: fun.type.params!.map({ .lower($0.type) }),
      in: fun.name,
      isEntry: true)

    // Emit the function's body.
    var args = module.blocks[entry].params.map(Operand.init)
    if !(req is CtorDecl) {
      // Unless the function is a constructor, we have to open the self parameter.
      let openedSelfType = impl.selfDecl!.type
      let openedSelf = module.insertBorrowExistAddr(
        isMutable: impl.isMutating,
        container: args[0],
        type: .lower(openedSelfType).address,
        at: .endOf(entry))
      args[0] = Operand(openedSelf)
    }

    let openedFun = module.getOrCreateFunction(from: impl)
    let result = module.insertApply(
      callee: Operand(FunRef(function: openedFun)),
      args: args,
      at: .endOf(entry))
    if !(req is CtorDecl) {
      module.insertEndBorrowAddr(source: args[0], at: .endOf(entry))
    }
    module.insertRet(value: Operand(result), at: .endOf(entry))

    return fun
  }

  /// Emits a local pattern binding declaration.
  static func emit(
    binding decl: PatternBindingDecl,
    state: inout State,
    into module: inout Module
  ) {
    // FIXME: implement local computed properties.
    assert(decl.varDecls.first?.hasStorage ?? true, "not implemented")

    let patterns = decl.pattern.namedPatterns
    precondition(patterns.count == 1, "not implemented") // FIXME: destructuring

    if !decl.isMutable {
      // Local immutable bindings require an initializer.
      guard let initializer = decl.initializer else {
        module.context.report(.immutableBindingRequiresInitializer(decl: decl))
        state.locals[ObjectIdentifier(patterns[0].decl)] = Operand(PoisonValue(
          type: .lower(decl.pattern.type)))
        state.hasError = true
        return
      }

      // If the initializer is an l-value, the binding must be initialized as a borrowed reference.
      // Otherwise, it requires storage to receive ownership, just as mutable bindings.
      if case .success(let source) = withLValueEmitter(
        state: &state, into: &module, do: initializer.accept)
      {
        let loc = module.insertBorrowAddr(
          isMutable: false,
          source: source,
          range: initializer.range,
          at: state.ip)
        state.locals[ObjectIdentifier(patterns[0])] = Operand(loc)
        return
      }
    }

    // Allocate storage if the binding is mutable or should receive ownership.
    let alloc = emit(storedVar: patterns[0].decl, state: &state, into: &module)
    state.locals[ObjectIdentifier(patterns[0].decl)] = Operand(alloc)
    if let initializer = decl.initializer {
      emit(
        assign: emit(rvalue: initializer, state: &state, into: &module),
        to: Operand(alloc),
        state: &state,
        into: &module,
        range: initializer.range)
    }
  }

  /// Emits the declaration of a stored local variable.
  static func emit(
    storedVar decl: VarDecl,
    state: inout State,
    into module: inout Module
  ) -> InstIndex {
    assert(decl.hasStorage)
    assert(decl.state == .typeChecked)

    // Allocate storage on the stack for the variable.
    let alloc = module.insertAllocStack(allocType: .lower(decl.type), decl: decl, at: state.ip)
    state.allocs.append(alloc)
    return alloc
  }

  static func emit(
    brace: BraceStmt,
    state: inout State,
    into module: inout Module
  ) {
    for i in 0 ..< brace.stmts.count {
      switch brace.stmts[i] {
      case let decl as PatternBindingDecl:
        emit(binding: decl, state: &state, into: &module)

      case let decl as FunDecl:
        _ = emit(function: decl, into: &module)

        // Emit the value of each captured declaration. Capture with `val` or `var` semantics are
        // copied from the environment, and so we must emit an r-value either way.
        let captureTable = decl.computeAllCaptures()
        let partialArgs = captureTable.map({ (key, value) -> Operand in
          // FIXME: Implement capture-by-reference (requires local bindings).
          assert(value.semantics != .mut, "not implemented")
          let expr = DeclRefExpr(decl: key.capturedDecl, type: value.type)
          return emit(rvalue: expr, state: &state, into: &module)
        })

        // Local function with capture declarations require stack allocation.
        if !partialArgs.isEmpty {
          let fun = FunRef(function: module.getOrCreateFunction(from: decl))
          let loc = module.insertAllocStack(allocType: .lower(decl.type), decl: decl, at: state.ip)
          state.locals[ObjectIdentifier(decl)] = Operand(loc)
          state.allocs.append(loc)

          let partial = module.insertPartialApply(
            delegator: Operand(fun), partialArgs: partialArgs, at: state.ip)
          module.insertStore(Operand(partial), to: Operand(loc), range: decl.range, at: state.ip)
        }

      case let decl as NominalTypeDecl:
        emit(topLevel: decl, into: &module)

      case let stmt as BraceStmt:
        emit(brace: stmt, state: &state, into: &module)

      case let stmt as RetStmt:
        emit(stmt: stmt, state: &state, into: &module)
        if i > brace.stmts.count - 1 {
          let context = state.funDecl.type.context
          context.report(.codeAfterReturnNeverExecuted(range: brace.stmts[i + 1].range))
          return
        }

      case let expr as Expr:
        // FIXME: Drop the result of the expression.
        _ = emit(rvalue: expr, state: &state, into: &module)

      default:
        fatalError("unreachable")
      }
    }

    // Deallocate the `alloc_stack`s in scope.
    while let alloc = state.allocs.popLast() {
      module.insertDeallocStack(alloc: Operand(alloc), at: state.ip)
    }
  }

  static func emit(
    stmt: RetStmt,
    state: inout State,
    into module: inout Module
  ) {
    let result: Operand
    if let expr = stmt.value {
      result = emit(rvalue: expr, state: &state, into: &module)
    } else {
      result = Operand(UnitValue(context: module.context))
    }
    module.insertRet(value: result, range: stmt.range, at: state.ip)
  }

  /// Emits the assignment of `rvalue` to `target`.
  static func emit(
    assign rvalue: Operand,
    to target: Operand,
    state: inout State,
    into module: inout Module,
    range: SourceRange? = nil
  ) {
    // Assuming type checking succeeded, we know that the LHS can't have an existential layout
    // unless the RHS does too.
    let rhsType = module.type(of: rvalue)
    let lhsType = module.type(of: target)
    if lhsType.isExistential && !rhsType.isExistential {
      module.insertInitExistAddr(container: target, value: rvalue, range: range, at: state.ip)
    } else {
      assert(!lhsType.isExistential || rhsType.isExistential)
      module.insertStore(rvalue, to: target, range: range, at: state.ip)
    }
  }

  /// Emits an r-value.
  static func emit(
    rvalue expr: Expr,
    state: inout State,
    into module: inout Module
  ) -> Operand {
    // Emit a poison value for any expression that has an error type.
    guard !expr.type.isError else {
      return Operand(PoisonValue(type: .lower(module.context.errorType)))
    }

    let result = withRValueEmitter(state: &state, into: &module, do: expr.accept)
    switch result {
    case .success(let val):
      return val

    case .failure(let error):
      module.context.report(error.diag())
      state.hasError = true
      return Operand(PoisonValue(type: .lower(module.context.errorType)))
    }
  }

  /// Emits an l-value.
  static func emit(
    lvalue expr: Expr,
    state: inout State,
    into module: inout Module
  ) -> Operand {
    // Emit a poison value for any expression that has an error type.
    guard !expr.type.isError else {
      return Operand(PoisonValue(type: .lower(module.context.errorType).address))
    }

    let result = withLValueEmitter(state: &state, into: &module, do: expr.accept)
    switch result {
    case .success(let loc):
      return loc

    case .failure(let error):
      module.context.report(error.diag())
      state.hasError = true
      return Operand(PoisonValue(type: .lower(module.context.errorType).address))
    }
  }

  /// Emits the address of a cell holding the value of the specified expression.
  static func emit(
    borrowable expr: Expr,
    state: inout State,
    into module: inout Module
  ) -> Operand {
    if case .success(let loc) = withLValueEmitter(state: &state, into: &module, do: expr.accept) {
      return loc
    } else {
      let val = emit(rvalue: expr, state: &state, into: &module)
      let alloc = module.insertAllocStack(
        allocType: .lower(expr.type), range: expr.range, at: state.ip)

      state.allocs.append(alloc)
      module.insertStore(val, to: Operand(alloc), range: expr.range, at: state.ip)
      return Operand(alloc)
    }
  }

  static func withRValueEmitter<T>(
    state: inout State,
    into module: inout Module,
    do action: (inout RValueEmitter) -> T
  ) -> T {
    return (
      withUnsafeMutablePointer(to: &state, { (_state) -> T in
      withUnsafeMutablePointer(to: &module, { (_module) -> T in
        var emitter = RValueEmitter(_state: _state, _module: _module)
        return action(&emitter)
      })
      }))
  }

  static func withLValueEmitter<T>(
    state: inout State,
    into module: inout Module,
    do action: (inout LValueEmitter) -> T
  ) -> T {
    return (
      withUnsafeMutablePointer(to: &state, { (_state) -> T in
      withUnsafeMutablePointer(to: &module, { (_module) -> T in
        var emitter = LValueEmitter(_state: _state, _module: _module)
        return action(&emitter)
      })
      }))
  }

}
