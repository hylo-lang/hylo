import Core
import DequeModule
import Utils

extension Module {

  /// Ensures that objects in `f` are initialized before use and deinitialized after last use,
  /// reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func normalizeObjectStates(in f: Function.ID, diagnostics: inout DiagnosticSet) {

    /// The control flow graph of the function to analyze.
    var cfg = self[f].cfg()

    /// The dominator tree of the function to analyze.
    var dominatorTree = DominatorTree(function: f, cfg: cfg, in: self)

    /// A FILO list of blocks to visit.
    var work = Deque(dominatorTree.bfs)

    /// The set of blocks that no longer need to be visited.
    var done: Set<Function.Blocks.Address> = []

    /// The state of the abstract interpreter before and after the visited basic blocks.
    var contexts: Contexts = [:]

    // Interpret the function until we reach a fixed point.
    while let blockToProcess = work.popFirst() {
      guard isVisitable(blockToProcess) else {
        work.append(blockToProcess)
        continue
      }

      // The entry block is a special case.
      if blockToProcess == self[f].entry {
        let x = Context(entryOf: self[f], in: program)
        let y = afterContext(of: blockToProcess, in: x)
        contexts[blockToProcess] = (before: x, after: y)
        done.insert(blockToProcess)
        continue
      }

      let (newBefore, sources) = beforeContext(of: blockToProcess)
      let newAfter: Context
      if contexts[blockToProcess]?.before != newBefore {
        newAfter = afterContext(of: blockToProcess, in: newBefore)
      } else if sources.count != cfg.predecessors(of: blockToProcess).count {
        newAfter = contexts[blockToProcess]!.after
      } else {
        done.insert(blockToProcess)
        continue
      }

      // We're done with the current block if ...
      let isBlockDone: Bool = {
        // 1) we're done with all of the block's predecessors.
        let pending = cfg.predecessors(of: blockToProcess).filter({ !done.contains($0) })
        if pending.isEmpty { return true }

        // 2) the only predecessor left is the block itself, yet the after-context didn't change.
        return (pending.count == 1)
          && (pending[0] == blockToProcess)
          && (contexts[blockToProcess]?.after == newAfter)
      }()

      // Update the before/after-context pair for the current block and move to the next one.
      contexts[blockToProcess] = (before: newBefore, after: newAfter)
      if isBlockDone {
        done.insert(blockToProcess)
      } else {
        work.append(blockToProcess)
      }
    }

    /// Returns `true` if `b` has been visited.
    func visited(_ b: Function.Blocks.Address) -> Bool {
      contexts[b] != nil
    }

    /// Returns `true` if `b` is ready to be visited.
    ///
    /// Computing the before-context of `b` requires knowing the state of all uses in `b` that are
    /// defined its (transitive) predecessors. Because a definition must dominate all its uses, we
    /// can assume the predecessors dominated by `b` don't define variables used in `b`. Hence, `b`
    /// can be visited iff all its predecessors have been visited or are dominated by `b`.
    func isVisitable(_ b: Function.Blocks.Address) -> Bool {
      if let d = dominatorTree.immediateDominator(of: b) {
        return visited(d)
          && cfg.predecessors(of: b).allSatisfy({ (p) in
            visited(p) || dominatorTree.dominates(b, p)
          })
      } else {
        // No predecessor.
        return true
      }
    }

    /// Returns the before-context of `b` and the predecessors from which it's been computed.
    ///
    /// - Requires: `isVisitable(b)` is `true`
    func beforeContext(
      of b: Function.Blocks.Address
    ) -> (context: Context, sources: [Function.Blocks.Address]) {
      let p = cfg.predecessors(of: b)
      let sources = p.filter({ contexts[$0] != nil })
      return (.init(merging: sources.lazy.map({ contexts[$0]!.after })), sources)
    }

    /// Returns the after-context of `b` formed by interpreting it in `initialContext`, inserting
    /// instructions into `self` to deinitialize objects whose storage is reused or deallocated
    /// and reporting violations of definite initialization in `diagnostics`.
    ///
    /// This function implements an abstract interpreter that keeps track of the initialization
    /// state of the objects in registers and memory.
    ///
    /// - Note: This function is idempotent.
    func afterContext(
      of b: Function.Blocks.Address,
      in initialContext: Context
    ) -> Context {
      var newContext = initialContext

      // We can safely iterate over the current indices of the block because instructions are
      // always inserted the currently visited address.
      let blockInstructions = self[f][b].instructions
      for i in blockInstructions.indices {
        let user = InstructionID(f, b, i.address)

        switch blockInstructions[i] {
        case is AllocStackInstruction:
          interpret(allocStack: user, in: &newContext)
        case is BorrowInstruction:
          interpret(borrow: user, in: &newContext)
        case is BranchInstruction:
          continue
        case is CondBranchInstruction:
          interpret(condBranch: user, in: &newContext)
        case is CallInstruction:
          interpret(call: user, in: &newContext)
        case is DeallocStackInstruction:
          interpret(deallocStack: user, in: &newContext)
        case is DeinitInstruction:
          interpret(deinit: user, in: &newContext)
        case is DestructureInstruction:
          interpret(destructure: user, in: &newContext)
        case is ElementAddrInstruction:
          interpret(elementAddr: user, in: &newContext)
        case is EndBorrowInstruction:
          continue
        case is LLVMInstruction:
          interpret(llvm: user, in: &newContext)
        case is LoadInstruction:
          interpret(load: user, in: &newContext)
        case is RecordInstruction:
          interpret(record: user, in: &newContext)
        case is ReturnInstruction:
          interpret(return: user, in: &newContext)
        case is StaticBranchInstruction:
          interpret(staticBranch: user, in: &newContext)
        case is StoreInstruction:
          interpret(store: user, in: &newContext)
        case is UnrechableInstruction:
          continue
        default:
          unreachable("unexpected instruction")
        }
      }

      return newContext
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(allocStack i: InstructionID, in context: inout Context) {
      // Create an abstract location denoting the newly allocated memory.
      let location = AbstractLocation.instruction(block: i.block, address: i.address)
      precondition(context.memory[location] == nil, "stack leak")

      // Update the context.
      context.memory[location] = Object(
        layout: AbstractTypeLayout(
          of: (self[i] as! AllocStackInstruction).allocatedType, definedIn: program),
        value: .full(.uninitialized))
      context.locals[FunctionLocal(i, 0)] = .locations([location])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(borrow i: InstructionID, in context: inout Context) {
      let borrow = self[i] as! BorrowInstruction

      // Operand must a location.
      let locations: Set<AbstractLocation>
      if let k = FunctionLocal(operand: borrow.location) {
        locations = context.locals[k]!.unwrapLocations()!
      } else {
        // Operand is a constant.
        fatalError("not implemented")
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

      context.locals[FunctionLocal(i, 0)] = .locations(Set(locations))
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
          context.forEachObject(at: FunctionLocal(operand: a)!) { (o) in
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
      let k = FunctionLocal(dealloc.location.instruction!, 0)
      let l = context.locals[k]!.unwrapLocations()!.uniqueElement!

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
      if let k = FunctionLocal(operand: addr.base) {
        locations = context.locals[k]!.unwrapLocations()!.map({ $0.appending(addr.elementPath) })
      } else {
        // Operand is a constant.
        fatalError("not implemented")
      }

      context.locals[FunctionLocal(i, 0)] = .locations(Set(locations))
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
      if let k = FunctionLocal(operand: load.source) {
        locations = context.locals[k]!.unwrapLocations()!
      } else {
        // Operand is a constant.
        fatalError("not implemented")
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

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(staticBranch i: InstructionID, in context: inout Context) {
      let s = self[i] as! StaticBranchInstruction
      if s.predicate != .initialized { fatalError("not implemented") }

      // Subject must be a location.
      let locations: Set<AbstractLocation>
      if let k = FunctionLocal(operand: s.subject) {
        locations = context.locals[k]!.unwrapLocations()!
      } else {
        // Operand is a constant.
        fatalError("not implemented")
      }

      let v = context.memory[locations.first!]?.value
      assert(locations.allSatisfy({ context.memory[$0]?.value == v }), "bad context")

      switch v {
      case .full(.initialized):
        removeBlock(s.targetIfFalse)
        replace(i, by: makeBranch(to: s.targetIfTrue, anchoredAt: s.site))
        work.remove(at: work.firstIndex(of: s.targetIfFalse.address)!)

      case .full(.uninitialized):
        removeBlock(s.targetIfTrue)
        replace(i, by: makeBranch(to: s.targetIfFalse, anchoredAt: s.site))
        work.remove(at: work.firstIndex(of: s.targetIfTrue.address)!)

      default:
        fatalError("not implemented")
      }

      // Recompute the control flow graph and dominator tree.
      cfg = functions[f]!.cfg()
      dominatorTree = .init(function: f, cfg: cfg, in: self)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(store i: InstructionID, in context: inout Context) {
      let store = self[i] as! StoreInstruction
      context.consume(store.object, with: i, at: store.site, diagnostics: &diagnostics)
      context.forEachObject(at: FunctionLocal(operand: store.target)!) { (o) in
        assert(o.value.initializedPaths.isEmpty || o.layout.type.base is BuiltinType)
        o.value = .full(.initialized)
      }
    }

  }

  /// Returns `true` iff `o` is the result of a borrow instruction or a constant value.
  private func isBorrowOrConstant(_ o: Operand) -> Bool {
    switch o {
    case .constant:
      return true
    case .parameter:
      return false
    case .result(let i, _):
      return self[i] is BorrowInstruction
    }
  }

  /// Assigns in `context` a fully initialized object to each virtual register defined by `i`.
  private func initializeRegisters(createdBy i: InstructionID, in context: inout Context) {
    for (j, t) in self[i].types.enumerated() {
      context.locals[FunctionLocal(i, j)] = .init(
        object: .full(.initialized), ofType: t.astType, definedIn: program)
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

  /// An abstract object or aggregate of objects.
  fileprivate struct Object: Equatable {

    /// A set of consumers.
    typealias Consumers = Set<InstructionID>

    /// The initialization state of an object or sub-object.
    ///
    /// The values of this type form a lattice whose supremum is `.initialized` and infimum is
    /// `.consumed(by: s)` where `s` is the set of all instructions. The join of two elements in
    /// this lattice represent the conservative superposition of two initialization states.
    enum State: Equatable {

      /// Object is initialized.
      case initialized

      /// Object is uninitialized.
      case uninitialized

      /// Object was consumed the users in the payload.
      ///
      /// An object can be consumed by multiple users after merging after-contexts in which it's
      /// been consumed by different users.
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

    /// The value of an object, describing the initialization state of its parts.
    enum Value: Equatable {

      /// An object whose parts all have the same state.
      case full(State)

      /// An object whose parts may have different states.
      ///
      /// - Requires: The payload is not empty.
      case partial([Value])

      /// The canonical form of `self`.
      var canonical: Value {
        switch self {
        case .full:
          return self

        case .partial(var subobjects):
          var isUniform = true
          subobjects[0] = subobjects[0].canonical
          for i in 1 ..< subobjects.count {
            subobjects[i] = subobjects[i].canonical
            isUniform = isUniform && subobjects[i] == subobjects[0]
          }
          return isUniform ? subobjects[0] : .partial(subobjects)
        }
      }

      /// If `self` is `.partial`, the paths to `self`'s parts; otherwise, `nil`.
      var paths: PartPaths? {
        if case .full = self { return nil }
        var paths = PartPaths(initialized: [], uninitialized: [], consumed: [])
        gatherSubobjectPaths(prefixedBy: [], into: &paths)
        return paths
      }

      /// The paths to `self`'s initialized parts.
      var initializedPaths: [PartPath] {
        switch self {
        case .full(.initialized):
          return [[]]
        case .full(.uninitialized), .full(.consumed):
          return []
        case .partial:
          return paths!.initialized
        }
      }

      /// If `self` is `.partial`, inserts the paths to its parts into `paths`, prefixing each
      /// inserted elemebt by `prefix`.
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
      static func && (lhs: Self, rhs: Self) -> Self {
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
      /// - Requires: `lhs` and `rhs` are canonical and have the same layout
      static func - (l: Self, r: Self) -> [PartPath] {
        switch (l, r) {
        case (.full(.initialized), let rhs):
          if let p = rhs.paths {
            return p.uninitialized + p.consumed.map(\.path)
          } else {
            return []
          }

        case (.full, _):
          return []

        case (_, .full(.initialized)):
          return [[]]

        case (let lhs, .full):
          return lhs.paths!.initialized

        case (.partial(let lhs), .partial(let rhs)):
          assert(lhs.count == rhs.count)
          return (0 ..< lhs.count).reduce(
            into: [],
            { (result, i) in
              result.append(contentsOf: (lhs[i] - rhs[i]).map({ [i] + $0 }))
            })
        }
      }

    }

    /// The paths to the initialized, uninitialized, and consumed parts of an object.
    struct PartPaths {

      /// The paths to the initialized parts.
      var initialized: [PartPath]

      /// The paths to the uninitialized parts.
      var uninitialized: [PartPath]

      /// The paths to the consumed parts, along with the users that consumed them.
      var consumed: [(path: PartPath, consumers: Consumers)]

    }

    /// The abstract layout of the object.
    let layout: AbstractTypeLayout

    /// The value of the object.
    var value: Value

    /// Creates an instance with the given properties.
    init(layout: AbstractTypeLayout, value: Value) {
      self.layout = layout
      self.value = value.canonical
    }

    /// Returns the result of calling `action` with the sub-object at given `offset`.
    ///
    /// - Requires: `i` is a valid index in `layout`.
    mutating func withSubobject<T>(_ offset: Int, _ action: (inout Object) -> T) -> T {
      let n = layout.properties.count
      precondition(n != 0)

      var parts: [Value]
      if case .partial(let p) = value {
        parts = p
      } else {
        parts = Array(repeating: value, count: n)
      }

      var o = Object(layout: layout[offset], value: parts[offset])
      defer {
        parts[offset] = o.value
        value = .partial(parts).canonical
      }
      return action(&o)
    }

    /// Returns the result of calling `action` with the sub-object at given `path`.
    ///
    /// - Requires: `offsets` is a valid path in `self`.
    mutating func withSubobject<T, P: Collection>(
      at path: P,
      _ action: (inout Object) -> T
    ) -> T where P.Element == Int {
      guard let (i, t) = path.headAndTail else {
        defer { value = value.canonical }
        return action(&self)
      }

      if t.isEmpty {
        return withSubobject(i, action)
      } else {
        return withSubobject(at: t, action)
      }
    }

    /// Returns `l` merged with `r`.
    static func && (l: Self, r: Self) -> Self {
      precondition(l.layout == r.layout)
      return Object(layout: l.layout, value: l.value && r.value)
    }

  }

  /// The abstract value of a register.
  fileprivate enum AbstractValue: Equatable {

    /// A non-empty set of locations.
    case locations(Set<AbstractLocation>)

    /// An object.
    case object(Object)

    /// Creates a `.object` value with an object of given `value` and `type`, using `program` to
    /// compute its layout.
    init(object value: Object.Value, ofType type: AnyType, definedIn program: TypedProgram) {
      self = .object(.init(layout: AbstractTypeLayout(of: type, definedIn: program), value: value))
    }

    /// If `self` is `.locations(l)`, returns `l`; otherwise, returns `nil`.
    func unwrapLocations() -> Set<AbstractLocation>? {
      if case .locations(let ls) = self {
        return ls
      } else {
        return nil
      }
    }

    /// If `self`is `.object(o)`, returns `o`; otherwise, returns `nil`.
    func unwrapObject() -> Object? {
      if case .object(let o) = self {
        return o
      } else {
        return nil
      }
    }

    /// Returns `l` merged with `r`.
    static func && (l: Self, r: Self) -> Self {
      switch (l, r) {
      case (.locations(let a), .locations(let b)):
        return .locations(a.union(b))
      case (.object(let a), .object(let b)):
        return .object(a && b)
      default:
        unreachable()
      }
    }

  }

  /// A map fron function block to the context of the abstract interpreter before and after the
  /// evaluation of its instructions.
  fileprivate typealias Contexts = [Function.Blocks.Address: (before: Context, after: Context)]

  /// An abstract interpretation context.
  fileprivate struct Context: Equatable {

    /// The values of the locals.
    var locals: [FunctionLocal: AbstractValue] = [:]

    /// The state of the memory.
    var memory: [AbstractLocation: Object] = [:]

    /// Creates an empty context.
    init() {}

    /// Creates the before-context `function`'s entry in `module`.
    init(entryOf function: Function, in program: TypedProgram) {
      for i in 0 ..< function.inputs.count {
        let (parameterConvention, parameterType) = function.inputs[i]
        let parameterKey = FunctionLocal.parameter(block: function.entry!, index: i)
        let parameterLayout = AbstractTypeLayout(of: parameterType.astType, definedIn: program)

        switch parameterConvention {
        case .let, .inout:
          let l = AbstractLocation.argument(index: i)
          locals[parameterKey] = .locations([l])
          memory[l] = Object(layout: parameterLayout, value: .full(.initialized))

        case .set:
          let l = AbstractLocation.argument(index: i)
          locals[parameterKey] = .locations([l])
          memory[l] = Object(layout: parameterLayout, value: .full(.uninitialized))

        case .sink:
          locals[parameterKey] = .object(
            .init(layout: parameterLayout, value: .full(.initialized)))

        case .yielded:
          preconditionFailure("cannot represent instance of yielded type")
        }
      }

    }

    /// Forms a context by merging the contexts in `batch`.
    init<C: Collection<Self>>(merging batch: C) {
      if let (h, t) = batch.headAndTail {
        self = t.reduce(into: h, { $0.merge($1) })
      } else {
        self.init()
      }
    }

    /// Merges `other` into `self`.
    mutating func merge(_ other: Self) {
      // Merge the locals.
      for (key, lhs) in locals {
        // Ignore definitions that don't dominate the block.
        guard let rhs = other.locals[key] else {
          locals[key] = nil
          continue
        }

        // Merge both values conservatively.
        locals[key] = lhs && rhs
      }

      // Merge the state of the objects in memory.
      memory.merge(other.memory, uniquingKeysWith: &&)
    }

    /// Calls `action` with a projection of the objects at the locations assigned to `local`.
    ///
    /// - Requires: `locals[k]` is `.locations`.
    mutating func forEachObject(at k: FunctionLocal, _ action: (inout Object) -> Void) {
      for l in locals[k]!.unwrapLocations()! {
        withObject(at: l, action)
      }
    }

    /// Returns the result calling `action` with a projection of the object at `location`.
    mutating func withObject<T>(
      at location: AbstractLocation,
      _ action: (inout Object) -> T
    ) -> T {
      switch location {
      case .null:
        preconditionFailure("null location")

      case .argument, .instruction:
        return action(&memory[location]!)

      case .sublocation(let rootLocation, let path):
        if path.isEmpty {
          return action(&memory[location]!)
        } else {
          return modifying(&memory[rootLocation]!, { $0.withSubobject(at: path, action) })
        }
      }
    }

    /// Updates the state of the `o` to mark it consumed by `consumer` at `site`, or report a
    /// diagnostic in `diagnostics` explaining why `o` can't be consumed.
    mutating func consume(
      _ o: Operand,
      with consumer: InstructionID,
      at site: SourceRange,
      diagnostics: inout DiagnosticSet
    ) {
      // Constants are never consumed.
      guard let k = FunctionLocal(operand: o) else { return }
      var o = locals[k]!.unwrapObject()!

      if o.value == .full(.initialized) {
        o.value = .full(.consumed(by: [consumer]))
        locals[k]! = .object(o)
      } else {
        diagnostics.insert(.illegalMove(at: site))
      }
    }

  }

}

extension Module.Context: CustomStringConvertible {

  var description: String {
    """
    locals:
    \(locals.map({ "- \($0): \($1)" }).joined(separator: "\n"))
    memory:
    \(memory.map({ "- \($0): \($1)" }).joined(separator: "\n"))
    """
  }

}

extension Module.AbstractValue: CustomStringConvertible {

  var description: String {
    switch self {
    case .locations(let v):
      return String(describing: v)
    case .object(let v):
      return String(describing: v)
    }
  }

}


extension Module.Object: CustomStringConvertible {

  var description: String { "\(layout.type)(\(value))" }

}

extension Module.Object.Value: CustomStringConvertible {

  var description: String {
    switch self {
    case .full(let s):
      return String(describing: s)
    case .partial(let s):
      return "{\(list: s, joinedBy: ", ")}"
    }
  }

}

extension Module.Object.State: CustomStringConvertible {

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
