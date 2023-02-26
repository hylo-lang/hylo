import Core
import DequeModule
import Utils

/// The definite initialization pass.
///
/// Definite initialization is a mendatory IR pass ensuiring that objects are initialized before
/// use and deinitialized before their storage is reused or before they go and out scope.
public struct DefiniteInitializationPass {

  /// Creates an instance.
  public init() {}

  /// Reports any use-before-initialization errors in `f` into `diagnostics`, where `f` is in
  /// `module`.
  public func run(
    function f: Function.ID,
    module: inout Module,
    diagnostics: inout DiagnosticSet
  ) {

    /// The control flow graph of the function to analyze.
    let cfg = module[f].cfg

    /// The dominator tree of the function to analyze.
    let dominatorTree = DominatorTree(function: f, cfg: cfg, in: module)

    /// A FILO list of blocks to visit.
    var work = Deque(dominatorTree.bfs)

    /// The set of blocks that no longer need to be visited.
    var done: Set<Function.Blocks.Address> = []

    /// The state of the abstract interpreter before and after the visited basic blocks.
    var contexts: Contexts = [:]

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

    /// Returns the before-context of `b` and a Boolean that's `false` iff it was formed using the
    /// after-contexts of all `b`'s predecessors.
    ///
    /// - Requires: `isVisitable(b)` is `true`
    func beforeContext(of b: Function.Blocks.Address) -> (context: Context, isPartial: Bool) {
      let p = cfg.predecessors(of: b)
      let availableAfterContexts = p.compactMap({ (p) in contexts[p]?.after })
      let isPartial = p.count != availableAfterContexts.count

      if let (h, t) = availableAfterContexts.headAndTail {
        return (h.merging(t), isPartial)
      } else {
        return (Context(), isPartial)
      }
    }

    /// Returns the after-context of `b` formed by interpreting it in `initialContext`, inserting
    /// instructions into `module` to deinitialize objects whose storage is reused or deallocated
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

      let blockInstructions = module[f][b].instructions
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

    /// Assigns in `context` a fully initialized object to each virtual register defined by `i`.
    func assignObjectRegisters(createdBy i: InstructionID, in context: inout Context) {
      for (j, t) in module[i].types.enumerated() {
        context.locals[FunctionLocal(i, j)] = .init(
          object: .full(.initialized), ofType: t.astType, definedIn: module.program)
      }
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(allocStack i: InstructionID, in context: inout Context) {
      // Create an abstract location denoting the newly allocated memory.
      let location = MemoryLocation.instruction(block: i.block, address: i.address)
      precondition(context.memory[location] == nil, "stack leak")

      // Update the context.
      context.memory[location] = Object(
        layout: AbstractTypeLayout(
          of: (module[i] as! AllocStackInstruction).allocatedType, definedIn: module.program),
        value: .full(.uninitialized))
      context.locals[FunctionLocal(i, 0)] = .locations([location])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(borrow i: InstructionID, in context: inout Context) {
      let borrow = module[i] as! BorrowInstruction

      // Operand must a location.
      let locations: Set<MemoryLocation>
      if let k = FunctionLocal(operand: borrow.location) {
        locations = context.locals[k]!.unwrapLocations()!
      } else {
        // Operand is a constant.
        fatalError("not implemented")
      }

      // Objects at each location have the same state unless DI or LoE has been broken.
      let o = context.withObject(at: locations.first!, typedIn: module.program, { $0 })

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
          let p = o.value.paths!
          if p.consumed.isEmpty {
            diagnostics.insert(.useOfPartiallyInitializedObject(at: borrow.site))
          } else {
            diagnostics.insert(.useOfPartiallyConsumedObject(at: borrow.site))
          }
        }

      case .set:
        // `set` requires the borrowed object to be uninitialized.
        let initializedPaths: [PartPath]
        switch o.value {
        case .full(.initialized):
          initializedPaths = [[]]
        case .full(.uninitialized), .full(.consumed):
          initializedPaths = []
        case .partial:
          initializedPaths = o.value.paths!.initialized
        }

        // Nothing to do if the location is already uninitialized.
        if initializedPaths.isEmpty { break }

        // Deinitialize the object(s) at the location.
        for path in initializedPaths {
          let s = module.insert(
            module.makeElementAddr(borrow.location, at: path, anchoredAt: borrow.site),
            before: i)[0]
          let o = module.insert(
            module.makeLoad(s, anchoredAt: borrow.site),
            before: i)[0]
          module.insert(module.makeDeinit(o, anchoredAt: borrow.site), before: i)
        }

        // Apply the effects of the new instructions.
        for l in locations {
          context.withObject(at: l, typedIn: module.program, { $0.value = .full(.uninitialized) })
        }

      case .yielded, .sink:
        unreachable()
      }

      context.locals[FunctionLocal(i, 0)] = .locations(Set(locations))
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(condBranch i: InstructionID, in context: inout Context) {
      let branch = module[i] as! CondBranchInstruction
      context.consume(branch.condition, with: i, at: branch.site, diagnostics: &diagnostics)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(call i: InstructionID, in context: inout Context) {
      let call = module[i] as! CallInstruction
      let calleeType = LambdaType(module.type(of: call.callee).astType)!

      if calleeType.receiverEffect == .sink {
        context.consume(call.callee, with: i, at: call.site, diagnostics: &diagnostics)
      }

      for (p, a) in zip(calleeType.inputs, call.operands) {
        switch ParameterType(p.type)!.access {
        case .let, .inout, .set:
          continue
        case .sink:
          context.consume(a, with: i, at: call.site, diagnostics: &diagnostics)
        case .yielded:
          unreachable()
        }
      }

      assignObjectRegisters(createdBy: i, in: &context)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(deallocStack i: InstructionID, in context: inout Context) {
      // The location operand is the result an `alloc_stack` instruction.
      let dealloc = module[i] as! DeallocStackInstruction
      let alloc = module[dealloc.location.instruction!] as! AllocStackInstruction

      let k = FunctionLocal(dealloc.location.instruction!, 0)
      let l = context.locals[k]!.unwrapLocations()!.uniqueElement!

      // Make sure the memory at the deallocated location is consumed or uninitialized.
      let initializedPaths: [PartPath] = context.withObject(
        at: l, typedIn: module.program,
        { (o) in
          switch o.value {
          case .full(.initialized):
            return [[]]
          case .full(.uninitialized), .full(.consumed):
            return []
          case .partial:
            return o.value.paths!.initialized
          }
        })

      for p in initializedPaths {
        let s = module.insert(
          module.makeElementAddr(dealloc.location, at: p, anchoredAt: dealloc.site),
          before: i)[0]
        let o = module.insert(
          module.makeLoad(s, anchoredAt: dealloc.site),
          before: i)[0]
        module.insert(module.makeDeinit(o, anchoredAt: dealloc.site), before: i)

        // Apply the effects of the new instructions.
        let consumer = InstructionID(
          i.function, i.block,
          module[i.function][i.block].instructions.address(before: i.address)!)

        let l = AbstractTypeLayout(of: alloc.allocatedType, definedIn: module.program)[p]
        context.locals[FunctionLocal(i, 0)] = .object(
          Object(layout: l, value: .full(.consumed(by: [consumer]))))
      }

      // Erase the deallocated memory from the context.
      context.memory[l] = nil
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(deinit i: InstructionID, in context: inout Context) {
      let x = module[i] as! DeinitInstruction
      context.consume(x.object, with: i, at: x.site, diagnostics: &diagnostics)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(elementAddr i: InstructionID, in context: inout Context) {
      let addr = module[i] as! ElementAddrInstruction

      // Operand must a location.
      let locations: [MemoryLocation]
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
      let x = module[i] as! DestructureInstruction
      context.consume(x.whole, with: i, at: x.site, diagnostics: &diagnostics)

      assignObjectRegisters(createdBy: i, in: &context)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(llvm i: InstructionID, in context: inout Context) {
      // TODO: Check that operands are initialized.
      assignObjectRegisters(createdBy: i, in: &context)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(load i: InstructionID, in context: inout Context) {
      let load = module[i] as! LoadInstruction

      // Operand must be a location.
      let locations: Set<MemoryLocation>
      if let k = FunctionLocal(operand: load.source) {
        locations = context.locals[k]!.unwrapLocations()!
      } else {
        // Operand is a constant.
        fatalError("not implemented")
      }

      // Object at target location must be initialized.
      for l in locations {
        context.withObject(at: l, typedIn: module.program) { (o) in
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

      assignObjectRegisters(createdBy: i, in: &context)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(record i: InstructionID, in context: inout Context) {
      let x = module[i] as! RecordInstruction
      for o in x.operands {
        context.consume(o, with: i, at: x.site, diagnostics: &diagnostics)
      }

      assignObjectRegisters(createdBy: i, in: &context)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(return i: InstructionID, in context: inout Context) {
      let x = module[i] as! ReturnInstruction
      context.consume(x.object, with: i, at: x.site, diagnostics: &diagnostics)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(store i: InstructionID, in context: inout Context) {
      let store = module[i] as! StoreInstruction

      // Consume the object operand.
      context.consume(store.object, with: i, at: store.site, diagnostics: &diagnostics)

      // Target operand must be a location.
      let locations: Set<MemoryLocation>
      if let k = FunctionLocal(operand: store.target) {
        locations = context.locals[k]!.unwrapLocations()!
      } else {
        // Operand is a constant.
        fatalError("not implemented")
      }

      for l in locations {
        context.withObject(at: l, typedIn: module.program, { $0.value = .full(.initialized) })
      }
    }

    // Interpret the function until we reach a fixed point.
    while let block = work.popFirst() {
      // Pick another block if we can't process this one yet.
      guard isVisitable(block) else {
        work.append(block)
        continue
      }

      // The entry block is a special case.
      if block == module[f].blocks.firstAddress {
        let x = Context(entryOf: module[f], in: module.program)
        let y = afterContext(of: block, in: x)
        contexts[block] = (before: x, after: y)
        done.insert(block)
        continue
      }

      let (newBefore, isNewContextPartial) = beforeContext(of: block)
      let newAfter: Context
      if contexts[block]?.before != newBefore {
        newAfter = afterContext(of: block, in: newBefore)
      } else if isNewContextPartial {
        newAfter = contexts[block]!.after
      } else {
        done.insert(block)
        continue
      }

      // We're done with the current block if ...
      let isBlockDone: Bool = {
        // 1) we're done with all of the block's predecessors.
        let pending = cfg.predecessors(of: block).filter({ !done.contains($0) })
        if pending.isEmpty { return true }

        // 2) the only predecessor left is the block itself, yet the after-context didn't change.
        return (pending.count == 1)
          && (pending[0] == block)
          && (contexts[block]?.after == newAfter)
      }()

      // Update the before/after-context pair for the current block and move to the next one.
      contexts[block] = (before: newBefore, after: newAfter)
      if isBlockDone {
        done.insert(block)
      } else {
        work.append(block)
      }
    }
  }

}

extension DefiniteInitializationPass {

  /// An abstract memory location.
  fileprivate enum MemoryLocation: Hashable {

    /// The null location.
    case null

    /// The location of an argument to a `let`, `inout`, or `set` parameter.
    case argument(index: Int)

    /// A location produced by an instruction.
    case instruction(block: Function.Blocks.Address, address: Block.Instructions.Address)

    /// A sub-location rooted at an argument or instruction.
    ///
    /// `path[i]` denotes the index of a property in the abstract layout of the object stored at
    /// `.sublocation(root: r, path: path.prefix(upTo: i))`. For example, if `r` is the location
    /// identifying storage of type `{{A, B}, C}`, then `sublocation(root: r, path: [0, 1])` is a
    /// location identifying storage of type `B`.
    ///
    /// - Note: Use `appending(_:)` to create instances of this case.
    /// - Requires: `root` is `.argument` or `.instruction` and `path` is not empty.
    indirect case sublocation(root: MemoryLocation, path: PartPath)

    /// Returns a new locating created by appending `suffix` to this one.
    ///
    /// - Requires: `self` is not `.null`.
    func appending(_ suffix: PartPath) -> MemoryLocation {
      if suffix.isEmpty { return self }

      switch self {
      case .null:
        preconditionFailure("null location")
      case .argument, .instruction:
        return .sublocation(root: self, path: suffix)
      case .sublocation(let root, let prefix):
        return .sublocation(root: root, path: prefix + suffix)
      }
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
    case locations(Set<MemoryLocation>)

    /// An object.
    case object(Object)

    /// Creates a `.object` value with an object of given `value` and `type`, using `program` to
    /// compute its layout.
    init(object value: Object.Value, ofType type: AnyType, definedIn program: TypedProgram) {
      self = .object(.init(layout: AbstractTypeLayout(of: type, definedIn: program), value: value))
    }

    /// If `self` is `.locations(l)`, returns `l`; otherwise, returns `nil`.
    func unwrapLocations() -> Set<MemoryLocation>? {
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
    var memory: [MemoryLocation: Object] = [:]

    /// Creates an empty context.
    init() {}

    /// Creates the before-context `function`'s entry in `module`.
    init(entryOf function: Function, in program: TypedProgram) {
      let entryAddress = function.blocks.firstAddress!

      for i in 0 ..< function.inputs.count {
        let (parameterConvention, parameterType) = function.inputs[i]
        let parameterKey = FunctionLocal.parameter(block: entryAddress, index: i)
        let parameterLayout = AbstractTypeLayout(of: parameterType.astType, definedIn: program)

        switch parameterConvention {
        case .let, .inout:
          let l = MemoryLocation.argument(index: i)
          locals[parameterKey] = .locations([l])
          memory[l] = Object(layout: parameterLayout, value: .full(.initialized))

        case .set:
          let l = MemoryLocation.argument(index: i)
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

    /// Forms a context by merging the contexts in `others` into `self`.
    func merging<S: Sequence>(_ others: S) -> Self where S.Element == Self {
      others.reduce(into: self, { (l, r) in l.merge(r) })
    }

    /// Returns the result calling `action` with a projection of the object at `location`, using
    /// `program` to compute object layouts.
    mutating func withObject<T>(
      at location: MemoryLocation,
      typedIn program: TypedProgram,
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
