import Core
import DequeModule
import Utils

extension Module {

  /// Ensures that objects in `f` are initialized before use and deinitialized after last use,
  /// reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func normalizeObjectStates(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    var machine = AbstractInterpreter(analyzing: f, in: self, entryContext: entryContext(of: f))

    // Verify that object states are properly initialized/deinitialized in `b` given `context`,
    // updating `self` as necessary and reporting violations in `diagnostics`.
    machine.fixedPoint { (b, machine, context) in
      // We can safely iterate over the current indices of the block because instructions are
      // always inserted the currently visited address.
      let blockInstructions = self[f][b].instructions
      for i in blockInstructions.indices {
        let user = InstructionID(f, b, i.address)

        switch blockInstructions[i] {
        case is AllocStackInstruction:
          interpret(allocStack: user, in: &context)
        case is BorrowInstruction:
          interpret(borrow: user, in: &context)
        case is BranchInstruction:
          continue
        case is CondBranchInstruction:
          interpret(condBranch: user, in: &context)
        case is CallInstruction:
          interpret(call: user, in: &context)
        case is DeallocStackInstruction:
          interpret(deallocStack: user, in: &context)
        case is DeinitInstruction:
          interpret(deinit: user, in: &context)
        case is DestructureInstruction:
          interpret(destructure: user, in: &context)
        case is ElementAddrInstruction:
          interpret(elementAddr: user, in: &context)
        case is EndBorrowInstruction:
          continue
        case is LLVMInstruction:
          interpret(llvm: user, in: &context)
        case is LoadInstruction:
          interpret(load: user, in: &context)
        case is RecordInstruction:
          interpret(record: user, in: &context)
        case is ReturnInstruction:
          interpret(return: user, in: &context)
        case is StaticBranchInstruction:
          interpret(staticBranch: user, updating: &machine, in: &context)
        case is StoreInstruction:
          interpret(store: user, in: &context)
        case is UnrechableInstruction:
          continue
        case is WrapAddrInstruction:
          interpret(wrapAddr: user, in: &context)
        default:
          unreachable("unexpected instruction")
        }
      }
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(allocStack i: InstructionID, in context: inout Context) {
      // Create an abstract location denoting the newly allocated memory.
      let l = AbstractLocation.root(.register(i, 0))
      precondition(context.memory[l] == nil, "stack leak")

      // Update the context.
      context.memory[l] = .init(
        layout: AbstractTypeLayout(
          of: (self[i] as! AllocStackInstruction).allocatedType, definedIn: program),
        value: .full(.uninitialized))
      context.locals[.register(i, 0)] = .locations([l])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(borrow i: InstructionID, in context: inout Context) {
      let borrow = self[i] as! BorrowInstruction

      // Operand must a location.
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
          if o.value.paths!.consumed.isEmpty {
            diagnostics.insert(.useOfPartiallyInitializedObject(at: borrow.site))
          } else {
            diagnostics.insert(.useOfPartiallyConsumedObject(at: borrow.site))
          }
        }

      case .set:
        // `set` requires the borrowed object to be uninitialized.
        let p = o.value.initializedPaths
        if p.isEmpty { break }

        insertDeinitialization(of: borrow.location, at: p, before: i, anchoredAt: borrow.site)
        for l in locations {
          context.withObject(at: l, { $0.value = .full(.uninitialized) })
        }

      case .yielded, .sink:
        unreachable()
      }

      context.locals[.register(i, 0)] = .locations(locations)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(condBranch i: InstructionID, in context: inout Context) {
      let branch = self[i] as! CondBranchInstruction
      context.consume(branch.condition, with: i, at: branch.site, diagnostics: &diagnostics)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(call i: InstructionID, in context: inout Context) {
      let call = self[i] as! CallInstruction
      let calleeType = LambdaType(type(of: call.callee).astType)!

      if calleeType.receiverEffect == .sink {
        context.consume(call.callee, with: i, at: call.site, diagnostics: &diagnostics)
      } else {
        assert(isBorrowOrConstant(call.callee))
      }

      for (p, a) in zip(calleeType.inputs, call.arguments) {
        switch ParameterType(p.type)!.access {
        case .let, .inout:
          assert(isBorrowOrConstant(call.callee))

        case .set:
          context.forEachObject(at: a) { (o) in
            assert(o.value.initializedPaths.isEmpty || o.layout.type.base is BuiltinType)
            o.value = .full(.initialized)
          }

        case .sink:
          context.consume(a, with: i, at: call.site, diagnostics: &diagnostics)

        case .yielded:
          unreachable()
        }
      }

      initializeRegisters(createdBy: i, in: &context)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(deallocStack i: InstructionID, in context: inout Context) {
      let dealloc = self[i] as! DeallocStackInstruction
      let l = context.locals[dealloc.location]!.unwrapLocations()!.uniqueElement!

      // Make sure the memory at the deallocated location is consumed or uninitialized before
      // erasing the deallocated memory from the context.
      let p = context.withObject(at: l, \.value.initializedPaths)
      insertDeinitialization(of: dealloc.location, at: p, before: i, anchoredAt: dealloc.site)
      context.memory[l] = nil
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(deinit i: InstructionID, in context: inout Context) {
      let x = self[i] as! DeinitInstruction
      context.consume(x.object, with: i, at: x.site, diagnostics: &diagnostics)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(elementAddr i: InstructionID, in context: inout Context) {
      let addr = self[i] as! ElementAddrInstruction

      // Operand must a location.
      let locations: [AbstractLocation]
      if case .constant = addr.base {
        // Operand is a constant.
        fatalError("not implemented")
      } else {
        locations =
          context.locals[addr.base]!.unwrapLocations()!.map({ $0.appending(addr.elementPath) })
      }

      context.locals[.register(i, 0)] = .locations(Set(locations))
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(destructure i: InstructionID, in context: inout Context) {
      let x = self[i] as! DestructureInstruction
      context.consume(x.whole, with: i, at: x.site, diagnostics: &diagnostics)

      initializeRegisters(createdBy: i, in: &context)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(llvm i: InstructionID, in context: inout Context) {
      // TODO: Check that operands are initialized.
      initializeRegisters(createdBy: i, in: &context)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(load i: InstructionID, in context: inout Context) {
      let load = self[i] as! LoadInstruction

      // Operand must be a location.
      let locations: Set<AbstractLocation>
      if case .constant = load.source {
        // Operand is a constant.
        fatalError("not implemented")
      } else {
        locations = context.locals[load.source]!.unwrapLocations()!
      }

      // Object at target location must be initialized.
      for l in locations {
        context.withObject(at: l) { (o) in
          switch o.value {
          case .full(.initialized):
            o.value = .full(.consumed(by: [i]))
          case .full(.uninitialized):
            diagnostics.insert(.useOfUninitializedObject(at: load.site))
          case .full(.consumed):
            diagnostics.insert(.useOfConsumedObject(at: load.site))
          case .partial:
            let p = o.value.paths!
            if p.consumed.isEmpty {
              diagnostics.insert(.useOfPartiallyInitializedObject(at: load.site))
            } else {
              diagnostics.insert(.useOfPartiallyConsumedObject(at: load.site))
            }
          }
        }
      }

      initializeRegisters(createdBy: i, in: &context)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(record i: InstructionID, in context: inout Context) {
      let x = self[i] as! RecordInstruction
      for o in x.operands {
        context.consume(o, with: i, at: x.site, diagnostics: &diagnostics)
      }

      initializeRegisters(createdBy: i, in: &context)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(return i: InstructionID, in context: inout Context) {
      let x = self[i] as! ReturnInstruction
      context.consume(x.object, with: i, at: x.site, diagnostics: &diagnostics)
    }

    /// Interprets `i` in `context`, updating the state of `machine` and reporting violations into
    /// `diagnostics`.
    func interpret(
      staticBranch i: InstructionID,
      updating machine: inout AbstractInterpreter<State>,
      in context: inout Context
    ) {
      let s = self[i] as! StaticBranchInstruction
      if s.predicate != .initialized { fatalError("not implemented") }

      // Subject must be a location.
      let locations: Set<AbstractLocation>
      if case .constant = s.subject {
        // Operand is a constant.
        fatalError("not implemented")
      } else {
        locations = context.locals[s.subject]!.unwrapLocations()!
      }

      let v = context.withObject(at: locations.first!, \.value)
      assert(
        locations.allSatisfy({ (l) in context.withObject(at: l, { $0.value == v }) }),
        "bad context")

      switch v {
      case .full(.initialized):
        removeBlock(s.targetIfFalse)
        replace(i, by: makeBranch(to: s.targetIfTrue, anchoredAt: s.site))
        machine.removeWork(s.targetIfFalse.address)

      case .full(.uninitialized):
        removeBlock(s.targetIfTrue)
        replace(i, by: makeBranch(to: s.targetIfFalse, anchoredAt: s.site))
        machine.removeWork(s.targetIfTrue.address)

      default:
        fatalError("not implemented")
      }

      // Recompute the control flow graph and dominator tree.
      machine.recomputeControlFlow(self)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(store i: InstructionID, in context: inout Context) {
      let store = self[i] as! StoreInstruction
      context.consume(store.object, with: i, at: store.site, diagnostics: &diagnostics)
      context.forEachObject(at: store.target) { (o) in
        assert(o.value.initializedPaths.isEmpty || o.layout.type.base is BuiltinType)
        o.value = .full(.initialized)
      }
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(wrapAddr i: InstructionID, in context: inout Context) {
      let s = self[i] as! WrapAddrInstruction
      if case .constant = s.witness {
        // Operand is a constant.
        fatalError("not implemented")
      }

      context.locals[.register(i, 0)] = context.locals[s.witness]
    }

  }

  /// Returns the initial context in which `f` should be interpreted.
  private func entryContext(of f: Function.ID) -> Context {
    let function = self[f]
    var result = Context()

    let b = Block.ID(f, function.entry!)
    for i in function.inputs.indices {
      let (parameterConvention, parameterType) = function.inputs[i]
      let parameterLayout = AbstractTypeLayout(of: parameterType.astType, definedIn: program)

      switch parameterConvention {
      case .let, .inout:
        let l = AbstractLocation.root(.parameter(b, i))
        result.locals[.parameter(b, i)] = .locations([l])
        result.memory[l] = .init(layout: parameterLayout, value: .full(.initialized))

      case .set:
        let l = AbstractLocation.root(.parameter(b, i))
        result.locals[.parameter(b, i)] = .locations([l])
        result.memory[l] = .init(layout: parameterLayout, value: .full(.uninitialized))

      case .sink:
        result.locals[.parameter(b, i)] = .object(
          .init(layout: parameterLayout, value: .full(.initialized)))

      case .yielded:
        preconditionFailure("cannot represent instance of yielded type")
      }
    }

    return result
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
        .init(layout: .init(of: t.astType, definedIn: program), value: .full(.initialized)))
    }
  }

  /// Inserts IR for the deinitialization of `root` at given `initializedPaths` before
  /// instruction `i`, anchoring instructions at `anchor`
  private mutating func insertDeinitialization(
    of root: Operand,
    at initializedPaths: [PartPath],
    before i: InstructionID,
    anchoredAt anchor: SourceRange
  ) {
    for path in initializedPaths {
      let s = insert(
        makeElementAddr(root, at: path, anchoredAt: anchor),
        before: i)[0]
      let o = insert(
        makeLoad(s, anchoredAt: anchor),
        before: i)[0]
      insert(makeDeinit(o, anchoredAt: anchor), before: i)
    }
  }

}

/// An abstract interpretation context.
private typealias Context = AbstractContext<State>

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

/// The paths to the initialized, uninitialized, and consumed parts of an object.
private struct PartPaths {

  /// The paths to the initialized parts.
  var initialized: [PartPath]

  /// The paths to the uninitialized parts.
  var uninitialized: [PartPath]

  /// The paths to the consumed parts, along with the users that consumed them.
  var consumed: [(path: PartPath, consumers: State.Consumers)]

}

extension AbstractContext where Domain == State {

  /// Updates the state of the `o` to mark it consumed by `consumer` at `site`, or report a
  /// diagnostic in `diagnostics` explaining why `o` can't be consumed.
  fileprivate mutating func consume(
    _ o: Operand,
    with consumer: InstructionID,
    at site: SourceRange,
    diagnostics: inout DiagnosticSet
  ) {
    // Constants are never consumed.
    if case .constant = o { return }
    var v = locals[o]!.unwrapObject()!

    if v.value == .full(.initialized) {
      v.value = .full(.consumed(by: [consumer]))
      locals[o]! = .object(v)
    } else {
      diagnostics.insert(.illegalMove(at: site))
    }
  }

}

extension AbstractObject.Value where Domain == State {

  /// If `self` is `.partial`, the paths to `self`'s parts; otherwise, `nil`.
  fileprivate var paths: PartPaths? {
    if case .full = self { return nil }
    var paths = PartPaths(initialized: [], uninitialized: [], consumed: [])
    gatherSubobjectPaths(prefixedBy: [], into: &paths)
    return paths
  }

  /// The paths to `self`'s initialized parts.
  fileprivate var initializedPaths: [PartPath] {
    switch self {
    case .full(.initialized):
      return [[]]
    case .full(.uninitialized), .full(.consumed):
      return []
    case .partial:
      return paths!.initialized
    }
  }

  /// If `self` is `.partial`, inserts the paths to its parts into `paths`, prefixing each inserted
  /// element by `prefix`.
  ///
  /// - Requires: `self` is canonical.
  private func gatherSubobjectPaths(
    prefixedBy prefix: PartPath,
    into paths: inout PartPaths
  ) {
    guard case .partial(let subobjects) = self else { return }

    for i in 0 ..< subobjects.count {
      switch subobjects[i] {
      case .full(.initialized):
        paths.initialized.append(prefix + [i])
      case .full(.uninitialized):
        paths.uninitialized.append(prefix + [i])
      case .full(.consumed(let c)):
        paths.consumed.append((path: prefix + [i], consumers: c))
      case .partial(let parts):
        for p in parts {
          p.gatherSubobjectPaths(prefixedBy: prefix + [i], into: &paths)
        }
      }
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
  static func - (l: Self, r: Self) -> [PartPath] {
    switch (l, r) {
    case (_, .full(.initialized)):
      // No part of LHS is not initialized in RHS.
      return []

    case (let lhs, .full):
      // RHS is fully consumed or uninitialized.
      if let p = lhs.paths {
        return p.initialized
      } else if lhs == .full(.initialized) {
        return [[]]
      } else {
        return []
      }

    case (.full(.initialized), let rhs):
      // RHS is partially initialized.
      let p = rhs.paths!
      return p.uninitialized + p.consumed.map(\.path)

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
