import Core
import DequeModule
import Utils

/// The definite initialization pass.
///
/// Definite initialization is a mendatory IR pass ensuiring that objects are initialized before
/// use and deinitialized before their storage is reused or before they go and out scope.
public struct DefiniteInitializationPass: TransformPass {

  /// The program from which the analyzed IR was lowered.
  private let program: TypedProgram

  public private(set) var diagnostics: [Diagnostic] = []

  public init(program: TypedProgram) {
    self.program = program
  }

  public mutating func run(function: Function.ID, module: inout Module) -> Bool {

    /// The control flow graph of the function to analyze.
    let cfg = module[function].cfg

    /// The dominator tree of the function to analyze.
    let dominatorTree = DominatorTree(function: function, cfg: cfg, in: module)

    /// A FILO list of blocks to visit.
    var work = Deque(dominatorTree.bfs)

    /// The set of blocks that no longer need to be visited.
    var done: Set<Function.Blocks.Address> = []

    /// The state of the abstract interpreter before and after the visited basic blocks.
    var contexts: [Function.Blocks.Address: (before: Context, after: Context)] = [:]

    /// The diagnostics reported by the pass.
    ///
    /// - TODO: This binding will should be removed once `TransformPass` is refactored.
    var diagnostics1 = Diagnostics()

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

      let blockInstructions = module[function][b].instructions
      for i in blockInstructions.indices {
        let user = InstructionID(function, b, i.address)

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

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(allocStack i: InstructionID, in context: inout Context) {
      // Create an abstract location denoting the newly allocated memory.
      let location = MemoryLocation.instruction(block: i.block, address: i.address)
      if context.memory[location] != nil {
        diagnostics1.report(.unboundedStackAllocation(at: module[i].site))
        return
      }

      // Update the context.
      context.memory[location] = Context.Cell(
        type: (module[i] as! AllocStackInstruction).allocatedType,
        object: .full(.uninitialized))
      context.locals[FunctionLocal(i, 0)] = .locations([location])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(borrow i: InstructionID, in context: inout Context) {
      let borrow = module[i] as! BorrowInstruction

      // Operand must a location.
      let locations: [MemoryLocation]
      if let k = FunctionLocal(operand: borrow.location) {
        locations = context.locals[k]!.unwrapLocations()!.map({ $0.appending(borrow.path) })
      } else {
        // Operand is a constant.
        fatalError("not implemented")
      }

      // Objects at each location have the same summary unless DI or LoE has been broken.
      let summary = context.withObject(at: locations[0], typedIn: program, \.summary)

      switch borrow.capability {
      case .let, .inout:
        // `let` and `inout` require the borrowed object to be initialized.
        switch summary {
        case .fullyInitialized:
          break
        case .fullyUninitialized:
          diagnostics1.report(.useOfUninitializedObject(at: borrow.site))
        case .fullyConsumed:
          diagnostics1.report(.useOfConsumedObject(at: borrow.site))
        case .partiallyInitialized:
          diagnostics1.report(.useOfPartiallyInitializedObject(at: borrow.site))
        case .partiallyConsumed:
          diagnostics1.report(.useOfPartiallyConsumedObject(at: borrow.site))
        }

      case .set:
        // `set` requires the borrowed object to be uninitialized.
        let initializedPaths: [[Int]]
        switch summary {
        case .fullyUninitialized, .fullyConsumed:
          initializedPaths = []
        case .fullyInitialized:
          initializedPaths = [borrow.path]
        case .partiallyInitialized(let paths):
          initializedPaths = paths.map({ borrow.path + $0 })
        case .partiallyConsumed(_, let paths):
          initializedPaths = paths.map({ borrow.path + $0 })
        }

        // Nothing to do if the location is already uninitialized.
        if initializedPaths.isEmpty { break }

        // Deinitialize the object(s) at the location.
        let rootType = module.type(of: borrow.location).astType
        for path in initializedPaths {
          let t = program.abstractLayout(of: rootType, at: path).type
          let o = module.insert(
            LoadInstruction(.object(t), from: borrow.location, at: path, site: borrow.site),
            before: i)[0]
          module.insert(
            DeinitInstruction(o, site: borrow.site),
            before: i)
        }

        // Apply the effects of the new instructions.
        for l in locations {
          context.withObject(at: l, typedIn: program, { $0 = .full(.uninitialized) })
        }

      case .yielded, .sink:
        unreachable()
      }

      context.locals[FunctionLocal(i, 0)] = .locations(Set(locations))
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(condBranch i: InstructionID, in context: inout Context) {
      let branch = module[i] as! CondBranchInstruction
      context.consume(branch.condition, with: i, at: branch.site, diagnostics: &diagnostics1)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(call i: InstructionID, in context: inout Context) {
      let call = module[i] as! CallInstruction
      for (c, o) in zip(call.conventions, call.operands) {
        switch c {
        case .let, .inout, .set:
          continue
        case .sink:
          context.consume(o, with: i, at: call.site, diagnostics: &diagnostics1)
        case .yielded:
          unreachable()
        }
      }
      context.locals[FunctionLocal(i, 0)] = .object(.full(.initialized))
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(deallocStack i: InstructionID, in context: inout Context) {
      // The location operand is the result an `alloc_stack` instruction.
      let dealloc = module[i] as! DeallocStackInstruction
      let alloc = module[dealloc.location.instruction!] as! AllocStackInstruction

      let k = FunctionLocal(dealloc.location.instruction!, 0)
      let l = context.locals[k]!.unwrapLocations()!.uniqueElement!

      // Make sure the memory at the deallocated location is consumed or uninitialized.
      let initializedPaths: [[Int]] = context.withObject(at: l, typedIn: program) { (o) in
        switch o.summary {
        case .fullyUninitialized, .fullyConsumed:
          return []
        case .fullyInitialized:
          return [[]]
        case .partiallyInitialized(let paths):
          return paths
        case .partiallyConsumed(_, let paths):
          return paths
        }
      }

      for p in initializedPaths {
        let objectType = program.abstractLayout(of: alloc.allocatedType, at: p).type
        let object = module.insert(
          LoadInstruction(.object(objectType), from: dealloc.location, at: p, site: dealloc.site),
          before: i)[0]
        module.insert(
          DeinitInstruction(object, site: dealloc.site), before: i)

        // Apply the effects of the new instructions.
        let consumer = InstructionID(
          i.function, i.block,
          module[i.function][i.block].instructions.address(before: i.address)!)
        context.locals[FunctionLocal(i, 0)] = .object(.full(.consumed(by: [consumer])))
      }

      // Erase the deallocated memory from the context.
      context.memory[l] = nil
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(deinit i: InstructionID, in context: inout Context) {
      let x = module[i] as! DeinitInstruction
      context.consume(x.object, with: i, at: x.site, diagnostics: &diagnostics1)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(destructure i: InstructionID, in context: inout Context) {
      let x = module[i] as! DestructureInstruction
      context.consume(x.object, with: i, at: x.site, diagnostics: &diagnostics1)

      for j in 0 ..< x.types.count {
        context.locals[FunctionLocal(i, j)] = .object(.full(.initialized))
      }
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(llvm i: InstructionID, in context: inout Context) {
      // TODO: Check that operands are initialized.
      context.locals[FunctionLocal(i, 0)] = .object(.full(.initialized))
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(load i: InstructionID, in context: inout Context) {
      let load = module[i] as! LoadInstruction

      // Operand must be a location.
      let locations: [MemoryLocation]
      if let k = FunctionLocal(operand: load.source) {
        locations = context.locals[k]!.unwrapLocations()!.map({ $0.appending(load.path) })
      } else {
        // Operand is a constant.
        fatalError("not implemented")
      }

      // Object at target location must be initialized.
      for l in locations {
        context.withObject(at: l, typedIn: program) { (o) in
          switch o.summary {
          case .fullyInitialized:
            o = .full(.consumed(by: [i]))
          case .fullyUninitialized:
            diagnostics1.report(.useOfUninitializedObject(at: load.site))
          case .fullyConsumed:
            diagnostics1.report(.useOfConsumedObject(at: load.site))
          case .partiallyInitialized:
            diagnostics1.report(.useOfPartiallyInitializedObject(at: load.site))
          case .partiallyConsumed:
            diagnostics1.report(.useOfPartiallyConsumedObject(at: load.site))
          }
        }
      }

      context.locals[FunctionLocal(i, 0)] = .object(.full(.initialized))
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(record i: InstructionID, in context: inout Context) {
      let x = module[i] as! RecordInstruction
      for o in x.operands {
        context.consume(o, with: i, at: x.site, diagnostics: &diagnostics1)
      }

      context.locals[FunctionLocal(i, 0)] = .object(.full(.initialized))
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(return i: InstructionID, in context: inout Context) {
      let x = module[i] as! ReturnInstruction
      context.consume(x.value, with: i, at: x.site, diagnostics: &diagnostics1)
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(store i: InstructionID, in context: inout Context) {
      let store = module[i] as! StoreInstruction

      // Consume the object operand.
      context.consume(store.object, with: i, at: store.site, diagnostics: &diagnostics1)

      // Target operand must be a location.
      let locations: Set<MemoryLocation>
      if let k = FunctionLocal(operand: store.target) {
        locations = context.locals[k]!.unwrapLocations()!
      } else {
        // Operand is a constant.
        fatalError("not implemented")
      }

      for l in locations {
        context.withObject(at: l, typedIn: program, { $0 = .full(.initialized) })
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
      if block == module[function].blocks.firstAddress {
        let x = Context(entryOf: module[function])
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

    diagnostics = Array(diagnostics1.log)
    return !diagnostics1.errorReported
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
    indirect case sublocation(root: MemoryLocation, path: [Int])

    /// Returns a new locating created by appending `suffix` to this one.
    ///
    /// - Requires: `self` is not `.null`.
    func appending(_ suffix: [Int]) -> MemoryLocation {
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
  fileprivate enum Object: Equatable {

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
      case consumed(by: Set<InstructionID>)

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

    /// The initialization state of an object or aggregate of objects.
    enum PartitionedState: Equatable {

      /// Object and all its parts are initialized.
      case fullyInitialized

      /// Object is fully uninitialized.
      case fullyUninitialized

      /// Object was consumed the users in the payload.
      ///
      /// - Requires: The payload is not empty.
      case fullyConsumed(by: Set<InstructionID>)

      /// Object has uninitialized parts, initialized parts, and no consumed parts; the payload
      /// contains the paths to all fully or partially initialized parts.
      case partiallyInitialized(initialized: [[Int]])

      /// Object has uninitialized parts, initialized parts; the payload contains the users of the
      /// consumed parts and the paths to all fully or partially initialized parts.
      case partiallyConsumed(by: Set<InstructionID>, initialized: [[Int]])

    }

    /// An object whose all parts have the same state.
    case full(State)

    /// An object whose part may have different states.
    ///
    /// - Requires: The payload must be non-empty.
    case partial([Object])

    /// Returns whether all parts have the same state.
    var isFull: Bool {
      if case .full = self { return true }
      if case .full = canonical { return true }
      return false
    }

    /// Given `self == .partial([obj_1, ..., obj_n])`, returns `obj_i`.
    subscript(i: Int) -> Object {
      get {
        guard case .partial(let subobjects) = self else {
          preconditionFailure("index out of range")
        }
        return subobjects[i]
      }
      _modify {
        guard case .partial(var subobjects) = self else {
          preconditionFailure("index out of range")
        }
        yield &subobjects[i]
        self = .partial(subobjects)
      }
    }

    /// Given `self == .full(s)`, assigns `self` to `.partial([obj_1, ..., obj_n])` where `obj_i`
    /// is `.full(s)` and `n` is the number of stored parts in `type`. Otherwise, does nothing.
    ///
    /// - Returns: The layout of `type`.
    ///
    /// - Requires: `type` must have a record layout and at least one stored property.
    mutating func disaggregate(type: AnyType, program: TypedProgram) -> AbstractTypeLayout {
      let layout = program.abstractLayout(of: type)
      guard case .full(let s) = self else { return layout }

      let n = layout.storedPropertiesTypes.count
      precondition(n != 0)
      self = .partial(Array(repeating: .full(s), count: n))
      return layout
    }

    /// A summary of the initialization of the object and its parts.
    var summary: PartitionedState {
      switch self {
      case .full(.initialized):
        return .fullyInitialized

      case .full(.uninitialized):
        return .fullyUninitialized

      case .full(.consumed(let consumers)):
        return .fullyConsumed(by: consumers)

      case .partial(let parts):
        var hasUninitializedPart = false
        var initializedPaths: [[Int]] = []
        var consumers: Set<InstructionID> = []

        for i in 0 ..< parts.count {
          switch parts[i].summary {
          case .fullyInitialized:
            initializedPaths.append([i])

          case .fullyUninitialized:
            hasUninitializedPart = true

          case .fullyConsumed(let users):
            consumers.formUnion(users)

          case .partiallyInitialized(let initialized):
            hasUninitializedPart = true
            initializedPaths.append(contentsOf: initialized.lazy.map({ [i] + $0 }))

          case .partiallyConsumed(let users, let initialized):
            consumers.formUnion(users)
            initializedPaths.append(contentsOf: initialized.lazy.map({ [i] + $0 }))
          }
        }

        if consumers.isEmpty {
          if initializedPaths.isEmpty {
            return .fullyUninitialized
          } else {
            return hasUninitializedPart
              ? .partiallyInitialized(initialized: initializedPaths)
              : .fullyInitialized
          }
        } else if initializedPaths.isEmpty {
          return .fullyConsumed(by: consumers)
        } else {
          return .partiallyConsumed(by: consumers, initialized: initializedPaths)
        }
      }
    }

    /// The canonical form of `self`.
    var canonical: Object {
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

    /// The paths of the parts in `self` that are fully initialized.
    var initializedPaths: [[Int]] {
      switch canonical {
      case .full(let state):
        return state == .initialized ? [[]] : []

      case .partial(let subojects):
        return (0 ..< subojects.count).reduce(
          into: [],
          { (result, i) in
            result.append(contentsOf: subojects[i].initializedPaths.map({ [i] + $0 }))
          })
      }
    }

    /// The paths of the parts in `self` that are uninitialized or consumed.
    var uninitializedOrConsumedPaths: [[Int]] {
      switch canonical {
      case .full(let state):
        return state == .initialized ? [] : [[]]

      case .partial(let subojects):
        return (0 ..< subojects.count).reduce(
          into: [],
          { (result, i) in
            result.append(contentsOf: subojects[i].uninitializedOrConsumedPaths.map({ [i] + $0 }))
          })
      }
    }

    /// Returns the paths of the parts that are initialized in `self` and uninitialized or consumed
    /// in `other`.
    func difference(_ other: Object) -> [[Int]] {
      switch (self.canonical, other.canonical) {
      case (.full(.initialized), let rhs):
        return rhs.uninitializedOrConsumedPaths

      case (.full, _):
        return []

      case (_, .full(.initialized)):
        return [[]]

      case (let lhs, .full):
        return lhs.initializedPaths

      case (.partial(let lhs), .partial(let rhs)):
        assert(lhs.count == rhs.count)
        return (0 ..< lhs.count).reduce(
          into: [],
          { (result, i) in
            result.append(contentsOf: lhs[i].difference(rhs[i]).map({ [i] + $0 }))
          })
      }
    }

    /// Returns `lhs` merged with `rhs`.
    static func && (lhs: Object, rhs: Object) -> Object {
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

  }

  /// An abstract value.
  fileprivate enum Value: Equatable {

    /// A non-empty set of locations.
    case locations(Set<MemoryLocation>)

    /// An object.
    case object(Object)

    /// Given `self = .locations(ls)`, returns `ls`; otherwise, returns `nil`.
    func unwrapLocations() -> Set<MemoryLocation>? {
      if case .locations(let ls) = self {
        return ls
      } else {
        return nil
      }
    }

    /// Given `self = .object(o)`, returns `o`; otherwise, returns `nil`.
    func unwrapObject() -> Object? {
      if case .object(let o) = self {
        return o
      } else {
        return nil
      }
    }

    /// Returns `lhs` merged with `rhs`.
    static func && (lhs: Value, rhs: Value) -> Value {
      switch (lhs, rhs) {
      case (.locations(let lhs), .locations(let rhs)):
        return .locations(lhs.union(rhs))
      case (.object(let lhs), .object(let rhs)):
        return .object(lhs && rhs)
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

    /// A memory cell.
    struct Cell: Equatable {

      /// The type of the object in the cell.
      var type: AnyType

      /// The object in the cell.
      var object: Object

    }

    /// The values of the locals.
    var locals: [FunctionLocal: Value] = [:]

    /// The state of the memory.
    var memory: [MemoryLocation: Cell] = [:]

    /// Creates an empty context.
    init() {}

    /// Creates the before-context `function`'s entry in `module`.
    init(entryOf function: Function) {
      let entryAddress = function.blocks.firstAddress!

      for i in 0 ..< function.inputs.count {
        let parameterKey = FunctionLocal.param(block: entryAddress, index: i)
        let (c, t) = function.inputs[i]
        switch c {
        case .let, .inout:
          let l = MemoryLocation.argument(index: i)
          locals[parameterKey] = .locations([l])
          memory[l] = Context.Cell(type: t.astType, object: .full(.initialized))

        case .set:
          let l = MemoryLocation.argument(index: i)
          locals[parameterKey] = .locations([l])
          memory[l] = Context.Cell(type: t.astType, object: .full(.uninitialized))

        case .sink:
          locals[parameterKey] = .object(.full(.initialized))

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
      memory.merge(other.memory) { (lhs, rhs) in
        assert(lhs.type == rhs.type)
        return Context.Cell(type: lhs.type, object: lhs.object && rhs.object)
      }
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
        return action(&memory[location]!.object)

      case .sublocation(let rootLocation, let path):
        if path.isEmpty {
          return action(&memory[location]!.object)
        } else {
          return modifying(&memory[rootLocation]!) { root in
            var derivedType = root.type
            var derivedPath = \Context.Cell.object
            for offset in path {
              // TODO: Handle tail-allocated objects.
              assert(offset >= 0, "not implemented")

              // Disaggregate the object if necessary.
              let layout = root[keyPath: derivedPath].disaggregate(
                type: derivedType, program: program)

              // Create a path to the sub-object.
              derivedType = layout.storedPropertiesTypes[offset]
              derivedPath = derivedPath.appending(path: \Object.[offset])
            }

            // Project the sub-object.
            return action(&root[keyPath: derivedPath])
          }
        }
      }
    }

    /// Updates the state of the `o` to mark it consumed by `consumer` at `site`, or report a
    /// diagnostic in `diagnostics` explaining why `o` can't be consumed.
    mutating func consume(
      _ o: Operand,
      with consumer: InstructionID,
      at site: SourceRange,
      diagnostics: inout Diagnostics
    ) {
      // Constants are never consumed.
      guard let k = FunctionLocal(operand: o) else { return }

      if locals[k]!.unwrapObject()!.summary == .fullyInitialized {
        locals[k]! = .object(.full(.consumed(by: [consumer])))
      } else {
        diagnostics.report(.illegalMove(at: site))
      }
    }

  }

}

extension Diagnostic {

  fileprivate static func illegalMove(at site: SourceRange) -> Diagnostic {
    .error("illegal move", at: site)
  }

  fileprivate static func unboundedStackAllocation(at site: SourceRange) -> Diagnostic {
    .error("unbounded stack allocation", at: site)
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
