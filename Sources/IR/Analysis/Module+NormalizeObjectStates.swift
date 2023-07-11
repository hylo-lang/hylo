import Core
import DequeModule
import Utils

extension Module {

  /// A "program counter" in the abstract machine.
  private typealias PC = Block.Instructions.Address

  /// Ensures that objects in `f` are initialized before use and deinitialized after last use,
  /// reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func normalizeObjectStates(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    var machine = AbstractInterpreter(analyzing: f, in: self, entryContext: entryContext(of: f))

    // Verify that object states are properly initialized/deinitialized in `b` given `context`,
    // updating `self` as necessary and reporting violations in `diagnostics`.
    machine.fixedPoint { (b, machine, context) in
      var pc = self[f][b].instructions.firstAddress
      while let a = pc {
        let user = InstructionID(f, b, a)

        switch self[f][b].instructions[a] {
        case is AddressToPointerInstruction:
          pc = interpret(addressToPointer: user, in: &context)
        case is AdvancedByBytesInstruction:
          pc = interpret(advancedByBytes: user, in: &context)
        case is AllocStackInstruction:
          pc = interpret(allocStack: user, in: &context)
        case is BorrowInstruction:
          pc = interpret(borrow: user, in: &context)
        case is BranchInstruction:
          pc = successor(of: user)
        case is CallInstruction:
          pc = interpret(call: user, in: &context)
        case is CallFFIInstruction:
          pc = interpret(callFFI: user, in: &context)
        case is CloseSumInstruction:
          pc = interpret(closeSum: user, in: &context)
        case is CondBranchInstruction:
          pc = interpret(condBranch: user, in: &context)
        case is DeallocStackInstruction:
          pc = interpret(deallocStack: user, in: &context)
        case is EndBorrowInstruction:
          pc = successor(of: user)
        case is EndProjectInstruction:
          pc = interpret(endProject: user, in: &context)
        case is GlobalAddrInstruction:
          pc = interpret(globalAddr: user, in: &context)
        case is LLVMInstruction:
          pc = interpret(llvm: user, in: &context)
        case is LoadInstruction:
          pc = interpret(load: user, in: &context)
        case is MarkStateInstruction:
          pc = interpret(markState: user, in: &context)
        case is MoveInstruction:
          pc = interpret(move: user, in: &context)
        case is OpenSumInstruction:
          pc = interpret(openSum: user, in: &context)
        case is PartialApplyInstruction:
          pc = interpret(partialApply: user, in: &context)
        case is PointerToAddressInstruction:
          pc = interpret(pointerToAddress: user, in: &context)
        case is ProjectInstruction:
          pc = interpret(project: user, in: &context)
        case is ReturnInstruction:
          pc = interpret(return: user, in: &context)
        case is StoreInstruction:
          pc = interpret(store: user, in: &context)
        case is SubfieldViewInstruction:
          pc = interpret(subfieldView: user, in: &context)
        case is UnrechableInstruction:
          pc = successor(of: user)
        case is UnsafeCastInstruction:
          pc = interpret(unsafeCast: user, in: &context)
        case is WrapExistentialAddrInstruction:
          pc = interpret(wrapExistentialAddr: user, in: &context)
        case is YieldInstruction:
          pc = interpret(yield: user, in: &context)
        default:
          unreachable("unexpected instruction")
        }
      }
    }

    /// Returns the successor of `i`, if any.
    func successor(of i: InstructionID) -> PC? {
      self[i.function][i.block].instructions.address(after: i.address)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(addressToPointer i: InstructionID, in context: inout Context) -> PC? {
      initializeRegisters(createdBy: i, in: &context)
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(allocStack i: InstructionID, in context: inout Context) -> PC? {
      // Create an abstract location denoting the newly allocated memory.
      let l = AbstractLocation.root(.register(i, 0))
      precondition(context.memory[l] == nil, "stack leak")

      // Update the context.
      let s = self[i] as! AllocStackInstruction
      let t = AbstractTypeLayout(of: s.allocatedType, definedIn: program)

      context.memory[l] = .init(layout: t, value: .full(.uninitialized))
      context.locals[.register(i, 0)] = .locations([l])
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(advancedByBytes i: InstructionID, in context: inout Context) -> PC? {
      let s = self[i] as! AdvancedByBytesInstruction
      consume(s.base, with: i, at: s.site, in: &context)
      consume(s.byteOffset, with: i, at: s.site, in: &context)
      initializeRegisters(createdBy: i, in: &context)
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(borrow i: InstructionID, in context: inout Context) -> PC? {
      let borrow = self[i] as! BorrowInstruction

      // Operand must be a location.
      let locations: Set<AbstractLocation>
      if case .constant = borrow.location {
        // Operand is a constant.
        fatalError("not implemented")
      } else {
        locations = context.locals[borrow.location]!.unwrapLocations()!
      }

      // Objects at each location have the same state unless DI or LoE has been broken.
      let o = context.withObject(at: locations.first!, { $0 })

      switch borrow.capability {
      case .let, .inout:
        // `let` and `inout` require the borrowed object to be initialized.
        switch o.value {
        case .full(.initialized):
          break
        case .full(.uninitialized):
          diagnostics.insert(.useOfUninitializedObject(at: borrow.site))
        case .full(.consumed):
          diagnostics.insert(.useOfConsumedObject(at: borrow.site))
        case .partial:
          if o.value.subfields!.consumed.isEmpty {
            diagnostics.insert(.useOfPartiallyInitializedObject(at: borrow.site))
          } else {
            diagnostics.insert(.useOfPartiallyConsumedObject(at: borrow.site))
          }
        }

      case .set:
        // `set` requires the borrowed object to be uninitialized.
        let p = o.value.initializedSubfields
        if p.isEmpty { break }

        insertDeinit(
          borrow.location, at: p, anchoredTo: borrow.site, before: i,
          reportingDiagnosticsTo: &diagnostics)
        for l in locations {
          context.withObject(at: l, { $0.value = .full(.uninitialized) })
        }

      case .yielded, .sink:
        unreachable()
      }

      context.locals[.register(i, 0)] = .locations(locations)
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(call i: InstructionID, in context: inout Context) -> PC? {
      let call = self[i] as! CallInstruction
      let callee = LambdaType(type(of: call.callee).ast)!

      if callee.receiverEffect == .sink {
        consume(call.callee, with: i, at: call.site, in: &context)
      } else {
        assert(isBorrowOrConstant(call.callee))
      }

      for (p, a) in zip(callee.inputs, call.arguments) {
        switch ParameterType(p.type)!.access {
        case .let, .inout:
          assert(isBorrowOrConstant(call.callee))

        case .set:
          context.forEachObject(at: a) { (o) in
            assert(o.value.initializedSubfields.isEmpty || o.layout.type.base is BuiltinType)
            o.value = .full(.initialized)
          }

        case .sink:
          consume(a, with: i, at: call.site, in: &context)

        case .yielded:
          unreachable()
        }
      }

      context.forEachObject(at: call.output) { (o) in
        assert(o.value.initializedSubfields.isEmpty || o.layout.type.base is BuiltinType)
        o.value = .full(.initialized)
      }

      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(callFFI i: InstructionID, in context: inout Context) -> PC? {
      let s = self[i] as! CallFFIInstruction
      for a in s.operands {
        consume(a, with: i, at: s.site, in: &context)
      }
      initializeRegisters(createdBy: i, in: &context)
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(closeSum i: InstructionID, in context: inout Context) -> PC? {
      let s = self[i] as! CloseSumInstruction
      let payload = context.locals[s.start]!.unwrapLocations()!.uniqueElement!

      // The state of the projected payload can't be partial.
      let o = context.withObject(at: payload, { $0 })
      guard case .full(let payloadInitializationState) = o.value else {
        fatalError()
      }

      // Copy the state of the payload to set the state of the container.
      let start = self[s.start.instruction!] as! OpenSumInstruction
      context.forEachObject(at: start.container) { (o) in
        o.value = .full(payloadInitializationState)
      }

      context.memory[payload] = nil
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(condBranch i: InstructionID, in context: inout Context) -> PC? {
      let branch = self[i] as! CondBranchInstruction
      consume(branch.condition, with: i, at: branch.site, in: &context)
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(deallocStack i: InstructionID, in context: inout Context) -> PC? {
      let s = self[i] as! DeallocStackInstruction
      let l = context.locals[s.location]!.unwrapLocations()!.uniqueElement!

      // Make sure the memory at the deallocated location is consumed or uninitialized before
      // erasing the deallocated memory from the context.
      let p = context.withObject(at: l, \.value.initializedSubfields)
      insertDeinit(
        s.location, at: p, anchoredTo: s.site, before: i,
        reportingDiagnosticsTo: &diagnostics)
      context.memory[l] = nil
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(endProject i: InstructionID, in context: inout Context) -> PC? {
      let s = self[i] as! EndProjectInstruction
      let l = context.locals[s.projection]!.unwrapLocations()!.uniqueElement!
      let projection = context.withObject(at: l, { $0 })

      let source = self[s.projection.instruction!] as! ProjectInstruction

      switch source.projection.access {
      case .let, .inout, .set:
        for c in projection.value.consumers {
          diagnostics.insert(.error(cannotConsume: source.projection.access, at: self[c].site))
        }

      case .sink:
        insertDeinit(
          s.projection, at: projection.value.initializedSubfields, anchoredTo: s.site, before: i,
          reportingDiagnosticsTo: &diagnostics)
        context.withObject(at: l, { $0.value = .full(.uninitialized) })

      case .yielded:
        unreachable()
      }

      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(globalAddr i: InstructionID, in context: inout Context) -> PC? {
      let l = AbstractLocation.root(.register(i, 0))
      context.memory[l] = .init(
        layout: AbstractTypeLayout(
          of: (self[i] as! GlobalAddrInstruction).valueType, definedIn: program),
        value: .full(.initialized))
      context.locals[.register(i, 0)] = .locations([l])
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(llvm i: InstructionID, in context: inout Context) -> PC? {
      // TODO: Check that operands are initialized.
      initializeRegisters(createdBy: i, in: &context)
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(load i: InstructionID, in context: inout Context) -> PC? {
      let load = self[i] as! LoadInstruction
      if case .constant = load.source { unreachable("load source is a constant") }

      // Operand must be a location.
      let locations = context.locals[load.source]!.unwrapLocations()!

      // Object at target location must be initialized.
      for l in locations {
        context.withObject(at: l) { (o) in
          switch o.value {
          case .full(.initialized):
            if !o.layout.type.isBuiltin {
              o.value = .full(.consumed(by: [i]))
            }
          case .full(.uninitialized):
            diagnostics.insert(.useOfUninitializedObject(at: load.site))
          case .full(.consumed):
            diagnostics.insert(.useOfConsumedObject(at: load.site))
          case .partial:
            let p = o.value.subfields!
            if p.consumed.isEmpty {
              diagnostics.insert(.useOfPartiallyInitializedObject(at: load.site))
            } else {
              diagnostics.insert(.useOfPartiallyConsumedObject(at: load.site))
            }
          }
        }
      }

      initializeRegisters(createdBy: i, in: &context)
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(markState i: InstructionID, in context: inout Context) -> PC? {
      let s = self[i] as! MarkStateInstruction

      let locations = context.locals[s.storage]!.unwrapLocations()!
      for l in locations {
        context.withObject(at: l) { (o) in
          o.value = .full(s.initialized ? .initialized : .uninitialized)
        }
      }

      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(move i: InstructionID, in context: inout Context) -> PC? {
      let s = self[i] as! MoveInstruction

      let k: AccessEffect = context.isStaticallyInitialized(s.target) ? .inout : .set
      let oper = demandMoveOperatorDeclaration(k, from: s.movable)
      let move = FunctionReference(to: oper, usedIn: s.movable.scope, in: self)

      let x0 = insert(makeBorrow(k, from: s.target, at: s.site), before: i)[0]
      let x1 = insert(makeAllocStack(.void, at: s.site), before: i)[0]
      let x2 = insert(makeBorrow(.set, from: x1, at: s.site), before: i)[0]
      let call = makeCall(
        applying: .constant(move), to: [x0, s.object], writingResultTo: x2, at: s.site)
      insert(call, before: i)
      insert(makeEndBorrow(x0, at: s.site), before: i)
      removeInstruction(i)

      return x0.instruction!.address
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(openSum i: InstructionID, in context: inout Context) -> PC? {
      let s = self[i] as! OpenSumInstruction
      let l = AbstractLocation.root(.register(i, 0))
      precondition(context.memory[l] == nil, "overlapping accesses to sum payload")

      // Operand must be a location.
      let locations = context.locals[s.container]!.unwrapLocations()!

      // Objects at each location have the same state unless DI or LoE has been broken.
      let o = context.withObject(at: locations.first!, { $0 })
      let t = AbstractTypeLayout(of: s.payloadType, definedIn: program)

      context.memory[l] = .init(layout: t, value: o.value)
      context.locals[.register(i, 0)] = .locations([l])
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(partialApply i: InstructionID, in context: inout Context) -> PC? {
      let x = self[i] as! PartialApplyInstruction
      consume(x.environment, with: i, at: x.site, in: &context)
      initializeRegisters(createdBy: i, in: &context)
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(pointerToAddress i: InstructionID, in context: inout Context) -> PC? {
      let s = self[i] as! PointerToAddressInstruction
      consume(s.source, with: i, at: s.site, in: &context)

      let l = AbstractLocation.root(.register(i, 0))
      context.memory[l] = .init(
        layout: AbstractTypeLayout(of: s.target.bareType, definedIn: program),
        value: .full(s.target.access == .set ? .uninitialized : .initialized))
      context.locals[.register(i, 0)] = .locations([l])
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(project i: InstructionID, in context: inout Context) -> PC? {
      // TODO: Process arguments

      let s = self[i] as! ProjectInstruction
      let l = AbstractLocation.root(.register(i, 0))
      context.memory[l] = .init(
        layout: AbstractTypeLayout(of: s.projection.bareType, definedIn: program),
        value: .full(s.projection.access == .set ? .uninitialized : .initialized))
      context.locals[.register(i, 0)] = .locations([l])
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(return i: InstructionID, in context: inout Context) -> PC? {
      // Make sure that all non-sink parameters are initialized on exit.
      let entry = entry(of: f)!
      for (i, p) in self[f].inputs.enumerated() where p.type.access != .sink {
        ensureInitializedOnExit(
          .parameter(entry, i), passed: p.type.access, in: &context,
          reportingDiagnosticsAt: diagnosticSite(for: p, in: f))
      }

      // Make sure that the return value is initialized on exit.
      if !self[f].isSubscript {
        ensureInitializedOnExit(
          .parameter(entry, self[f].inputs.count), passed: .set, in: &context,
          reportingDiagnosticsAt: .empty(at: self[f].site.first()))
      }

      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(store i: InstructionID, in context: inout Context) -> PC? {
      let store = self[i] as! StoreInstruction
      consume(store.object, with: i, at: store.site, in: &context)
      context.forEachObject(at: store.target) { (o) in
        assert(o.value.initializedSubfields.isEmpty || o.layout.type.isBuiltin)
        o.value = .full(.initialized)
      }
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(subfieldView i: InstructionID, in context: inout Context) -> PC? {
      let addr = self[i] as! SubfieldViewInstruction

      // Operand must a location.
      let locations: [AbstractLocation]
      if case .constant = addr.recordAddress {
        // Operand is a constant.
        fatalError("not implemented")
      } else {
        locations =
          context.locals[addr.recordAddress]!.unwrapLocations()!.map({
            $0.appending(addr.subfield)
          })
      }

      context.locals[.register(i, 0)] = .locations(Set(locations))
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(unsafeCast i: InstructionID, in context: inout Context) -> PC? {
      let s = self[i] as! UnsafeCastInstruction
      consume(s.source, with: i, at: s.site, in: &context)
      initializeRegisters(createdBy: i, in: &context)
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(wrapExistentialAddr i: InstructionID, in context: inout Context) -> PC? {
      let s = self[i] as! WrapExistentialAddrInstruction
      if case .constant = s.witness {
        // Operand is a constant.
        fatalError("not implemented")
      }

      context.locals[.register(i, 0)] = context.locals[s.witness]
      return successor(of: i)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(yield i: InstructionID, in context: inout Context) -> PC? {
      let s = self[i] as! YieldInstruction
      assert(isBorrowOrConstant(s.projection))
      return successor(of: i)
    }

    /// Updates the state of the `o` in `context` to mark it has been consumed by `consumer` at
    /// `site` or reports a diagnostic explaining why `o` can't be consumed.
    func consume(
      _ o: Operand,
      with consumer: InstructionID,
      at site: SourceRange,
      in context: inout Context
    ) {
      // Constant values are synthesized on demand. Built-ins are never consumed.
      if case .constant = o { return }
      if type(of: o).ast.isBuiltin { return }

      var v = context.locals[o]!.unwrapObject()!
      if v.value == .full(.initialized) {
        v.value = .full(.consumed(by: [consumer]))
        context.locals[o]! = .object(v)
      } else {
        diagnostics.insert(.illegalMove(at: site))
      }
    }

    /// Checks that entry parameter `p`, passed with capability `k`, is initialized in `context`,
    /// reporting diagnostics at `site` if it isn't.
    func ensureInitializedOnExit(
      _ p: Operand, passed k: AccessEffect, in context: inout Context,
      reportingDiagnosticsAt site: SourceRange
    ) {
      context.withObject(at: .root(p)) { (o) in
        if o.value == .full(.initialized) { return }

        if k == .set {
          diagnostics.insert(
            .uninitializedSetParameter(beforeReturningFrom: f, in: self, at: site))
          return
        }

        // If the parameter is `let` or `inout`, it's been (partially) consumed since it was
        // initialized in the entry context.
        diagnostics.insert(
          .illegalParameterEscape(consumedBy: o.value.consumers, in: self, at: site))
      }
    }
  }

  /// Returns the initial context in which `f` should be interpreted.
  private func entryContext(of f: Function.ID) -> Context {
    let function = self[f]
    var result = Context()

    let entry = Block.ID(f, function.entry!)
    addParameter(.set, function.output, of: entry, at: function.inputs.count, in: &result)
    for i in function.inputs.indices {
      addParameter(function.inputs[i].type, of: entry, at: i, in: &result)
    }

    return result
  }

  /// Configure in `context` the initial state of the parameter at `position` in `entry`, which
  /// has type `t`.
  private func addParameter(
    _ t: ParameterType, of entry: Block.ID, at position: Int,
    in context: inout Context
  ) {
    addParameter(t.access, t.bareType, of: entry, at: position, in: &context)
  }

  /// Configure in `context` the initial state of the parameter at `position` in `entry`, which
  /// has type `t` passed with capability `k`.
  private func addParameter(
    _ k: AccessEffect, _ t: AnyType, of entry: Block.ID, at position: Int,
    in context: inout Context
  ) {
    let l = AbstractTypeLayout(of: t, definedIn: program)
    let p = Operand.parameter(entry, position)

    switch k {
    case .let, .inout, .sink:
      let a = AbstractLocation.root(p)
      context.locals[p] = .locations([a])
      context.memory[a] = .init(layout: l, value: .full(.initialized))

    case .set:
      let a = AbstractLocation.root(p)
      context.locals[p] = .locations([a])
      context.memory[a] = .init(layout: l, value: .full(.uninitialized))

    case .yielded:
      preconditionFailure("cannot represent instance of yielded type")
    }
  }

  /// Returns `true` iff `o` is the result of a borrow instruction or a constant value.
  private func isBorrowOrConstant(_ o: Operand) -> Bool {
    switch o {
    case .constant:
      return true
    case .parameter:
      return false
    case .register(let i, _):
      return self[i] is BorrowInstruction
    }
  }

  /// Assigns in `context` a fully initialized object to each virtual register defined by `i`.
  private func initializeRegisters(createdBy i: InstructionID, in context: inout Context) {
    for (j, t) in self[i].types.enumerated() {
      context.locals[.register(i, j)] = .object(
        .init(layout: .init(of: t.ast, definedIn: program), value: .full(.initialized)))
    }
  }

  /// Inserts IR for the deinitialization of `root` at given `initializedSubfields` before
  /// instruction `i`, anchoring instructions to `site`
  private mutating func insertDeinit(
    _ root: Operand, at initializedSubfields: [RecordPath], anchoredTo site: SourceRange,
    before i: InstructionID, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    for path in initializedSubfields {
      let s = insert(makeSubfieldView(of: root, subfield: path, at: site), before: i)[0]

      let useScope = functions[i.function]![i.block].scope
      let success = Emitter.insertDeinit(
        s, usingDeinitializerExposedTo: useScope, at: site, .before(i), in: &self)

      if !success {
        log.insert(.error(nonDeinitializable: type(of: s).ast, at: site))
      }
    }
  }

  /// Returns the site at which diagnostics related to the parameter `p` should be reported in `f`.
  private func diagnosticSite(for p: Parameter, in f: Function.ID) -> SourceRange {
    guard let d = p.decl else { return .empty(at: self[f].site.first()) }
    switch d.kind {
    case ParameterDecl.self:
      return program.ast[ParameterDecl.ID(d)!].identifier.site
    default:
      return program.ast[d].site
    }
  }

}

/// An abstract interpretation context.
private typealias Context = AbstractContext<State>

extension Context {

  /// Returns `true` iff the object at `a` is fully initialized.
  fileprivate mutating func isStaticallyInitialized(_ a: Operand) -> Bool {
    if case .constant = a { unreachable("operand is a constant") }
    let locations = locals[a]!.unwrapLocations()!

    let v = withObject(at: locations.first!, \.value)
    assert(
      locations.allSatisfy({ (l) in withObject(at: l, { $0.value == v }) }),
      "bad context")

    switch v {
    case .full(.initialized):
      return true
    case .full(.uninitialized), .full(.consumed):
      return false
    default:
      fatalError("not implemented")
    }
  }

}

/// A map fron function block to the context of the abstract interpreter before and after the
/// evaluation of its instructions.
private typealias Contexts = [Function.Blocks.Address: (before: Context, after: Context)]

/// The initialization state of an object or sub-object.
///
/// Instances form a lattice whose supremum is `.initialized` and infimum is `.consumed(by: s)`
/// where `s` is the set of all instructions. The meet of two elements denotes the conservative
/// superposition of two initialization states.
private enum State: AbstractDomain {

  /// A set of consumers.
  typealias Consumers = Set<InstructionID>

  /// Object is initialized.
  case initialized

  /// Object is uninitialized.
  case uninitialized

  /// Object was consumed the users in the payload.
  ///
  /// An object can be consumed by multiple users after merging after-contexts in which it's been
  /// consumed by different users.
  ///
  /// - Requires: The payload is not empty.
  case consumed(by: Consumers)

  /// Forms a new state by merging `lhs` with `rhs`.
  static func && (lhs: State, rhs: State) -> State {
    switch lhs {
    case .initialized:
      return rhs

    case .uninitialized:
      return rhs == .initialized ? lhs : rhs

    case .consumed(let a):
      if case .consumed(let b) = rhs {
        return .consumed(by: a.union(b))
      } else {
        return .consumed(by: a)
      }
    }
  }

}

extension State: CustomStringConvertible {

  var description: String {
    switch self {
    case .initialized:
      return "\u{23Fa}"
    case .uninitialized:
      return "\u{25cb}"
    case .consumed(let consumers):
      return "←\(consumers)"
    }
  }

}

/// Classification of a record type's subfields into uninitialized, initialized, and consumed sets.
private struct SubfieldsByInitializationState {

  /// The paths to the initialized parts.
  var initialized: [RecordPath]

  /// The paths to the uninitialized parts.
  var uninitialized: [RecordPath]

  /// The paths to the consumed parts, along with the users that consumed them.
  var consumed: [(subfield: RecordPath, consumers: State.Consumers)]

}

extension AbstractObject.Value where Domain == State {

  /// If `self` is `.partial`, the paths to `self`'s parts; otherwise, `nil`.
  fileprivate var subfields: SubfieldsByInitializationState? {
    if case .full = self { return nil }
    var paths = SubfieldsByInitializationState(initialized: [], uninitialized: [], consumed: [])
    gatherSubobjectPaths(prefixedBy: [], into: &paths)
    return paths
  }

  /// The initialized subfields.
  fileprivate var initializedSubfields: [RecordPath] {
    switch self {
    case .full(.initialized):
      return [[]]
    case .full(.uninitialized), .full(.consumed):
      return []
    case .partial:
      return subfields!.initialized
    }
  }

  /// If `self` is `.partial`, inserts the paths to its parts into `paths`, prefixing each inserted
  /// element by `prefix`.
  ///
  /// - Requires: `self` is canonical.
  private func gatherSubobjectPaths(
    prefixedBy prefix: RecordPath,
    into paths: inout SubfieldsByInitializationState
  ) {
    guard case .partial(let subobjects) = self else { return }

    for i in 0 ..< subobjects.count {
      switch subobjects[i] {
      case .full(.initialized):
        paths.initialized.append(prefix + [i])
      case .full(.uninitialized):
        paths.uninitialized.append(prefix + [i])
      case .full(.consumed(let c)):
        paths.consumed.append((subfield: prefix + [i], consumers: c))
      case .partial(let parts):
        for p in parts {
          p.gatherSubobjectPaths(prefixedBy: prefix + [i], into: &paths)
        }
      }
    }
  }

  /// The consumers of the object.
  fileprivate var consumers: State.Consumers {
    switch self {
    case .full(.initialized), .full(.uninitialized):
      return []
    case .full(.consumed(let c)):
      return c
    case .partial(let parts):
      return parts.reduce(into: [], { (s, p) in s.formUnion(p.consumers) })
    }
  }

  /// Returns `lhs` merged with `rhs`.
  fileprivate static func && (lhs: Self, rhs: Self) -> Self {
    switch (lhs.canonical, rhs.canonical) {
    case (.full(let lhs), .full(let rhs)):
      return .full(lhs && rhs)

    case (.partial(let lhs), .partial(let rhs)):
      assert(lhs.count == rhs.count)
      return .partial(zip(lhs, rhs).map(&&))

    case (.partial(let lhs), _):
      return .partial(lhs.map({ $0 && rhs }))

    case (_, .partial(let rhs)):
      return .partial(rhs.map({ lhs && $0 }))
    }
  }

  /// Returns the paths of the parts that are initialized in `l` and either uninitialized or
  /// consumed in `r`.
  ///
  /// - Requires: `l` and `r` are canonical and have the same layout
  static func - (l: Self, r: Self) -> [RecordPath] {
    switch (l, r) {
    case (_, .full(.initialized)):
      // No part of LHS is not initialized in RHS.
      return []

    case (let lhs, .full):
      // RHS is fully consumed or uninitialized.
      if let p = lhs.subfields {
        return p.initialized
      } else if lhs == .full(.initialized) {
        return [[]]
      } else {
        return []
      }

    case (.full(.initialized), let rhs):
      // RHS is partially initialized.
      let p = rhs.subfields!
      return p.uninitialized + p.consumed.map(\.subfield)

    case (.full, _):
      // LHS is fully consumed or uninitialized.
      return []

    case (.partial(let lhs), .partial(let rhs)):
      // LHS and RHS are partially initialized.
      assert(lhs.count == rhs.count)
      return (0 ..< lhs.count).reduce(into: []) { (result, i) in
        result.append(contentsOf: (lhs[i] - rhs[i]).map({ [i] + $0 }))
      }
    }
  }

}

extension Diagnostic {

  fileprivate static func illegalMove(at site: SourceRange) -> Diagnostic {
    .error("illegal move", at: site)
  }

  fileprivate static func illegalParameterEscape(
    consumedBy consumers: State.Consumers? = nil,
    in module: Module,
    at site: SourceRange
  ) -> Diagnostic {
    if let c = consumers {
      return .error(
        "parameter was consumed", at: site,
        notes: c.map({ Diagnostic.note("escape happens here", at: module[$0].site) }))
    } else {
      return .error("parameter was consumed", at: site)
    }
  }

  fileprivate static func uninitializedSetParameter(
    beforeReturningFrom f: Function.ID,
    in module: Module,
    at site: SourceRange
  ) -> Diagnostic {
    let e = module[f].isSubscript ? "subscript" : "function"
    return .error("set parameter not initialized before \(e) returns", at: site)
  }

  fileprivate static func error(
    cannotConsume k: AccessEffect, at site: SourceRange
  ) -> Diagnostic {
    .error("cannot consume '\(k)' projection", at: site)
  }

  fileprivate static func useOfConsumedObject(at site: SourceRange) -> Diagnostic {
    .error("use of consumed object", at: site)
  }

  fileprivate static func useOfPartiallyConsumedObject(at site: SourceRange) -> Diagnostic {
    .error("use of partially consumed object", at: site)
  }

  fileprivate static func useOfPartiallyInitializedObject(at site: SourceRange) -> Diagnostic {
    .error("use of partially initialized object", at: site)
  }

  fileprivate static func useOfUninitializedObject(at site: SourceRange) -> Diagnostic {
    .error("use of uninitialized object", at: site)
  }

}
