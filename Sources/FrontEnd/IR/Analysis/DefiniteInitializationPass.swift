import DequeModule
import Utils

/// The definite initialization pass.
///
/// Definite initialization checks that objects are initialized before use and deinitialized
/// before their storage is reused or before they go and out scope.
public struct DefiniteInitializationPass: TransformPass {

  /// The pass is implemented as an abstract interpreter keeping track of the initialization state
  /// of the objects in registers and memory.
  ///
  /// The interpreter relies on the IR being well-formed.

  private typealias Contexts = [Function.Blocks.Address: (before: Context, after: Context)]

  public static let name = "Definite initialization"

  /// The program being lowered.
  public let program: TypedProgram

  /// The ID of the function being interpreted.
  private var functionID: Function.ID = -1

  /// The current evaluation context.
  private var currentContext = Context()

  public private(set) var diagnostics: [Diagnostic] = []

  public init(program: TypedProgram) {
    self.program = program
  }

  public mutating func run(function functionID: Function.ID, module: inout Module) -> Bool {
    /// The control flow graph of the function to analyze.
    let cfg = module[functionID].cfg
    /// The dominator tree of the function to analyize.
    let dominatorTree = DominatorTree(function: functionID, cfg: cfg, in: module)
    /// A FILO list of blocks to visit.
    var work: Deque<Function.Blocks.Address>
    /// The set of blocks that no longer need to be visited.
    var done: Set<Function.Blocks.Address> = []
    /// The state of the abstract interpreter before and after the visited basic blocks.
    var contexts: [Function.Blocks.Address: (before: Context, after: Context)] = [:]
    /// Indicates whether the pass succeeded.
    var success = true

    /// Returns the before-context that merges the after-contexts of `predecessors`, inserting
    /// deinitialization at the end of these blocks if necessary.
    func mergeAfterContexts(of predecessors: [Function.Blocks.Address]) -> Context {
      /// The set of predecessors that have been visited already.
      var visitedPredecessors: [Function.Blocks.Address] = []
      /// The sources of the after contexts used for this merge.
      var afterContextSources: Set<Function.Blocks.Address> = []

      // Determine the after-context that will be used in the merge for each predecessor: if the
      // predecessor has already been visited, use its after-context; otherwise, use the
      // after-context of its immediate dominator.
      for predecessor in predecessors {
        if contexts[predecessor] != nil {
          visitedPredecessors.append(predecessor)
          afterContextSources.insert(predecessor)
        } else {
          afterContextSources.insert(dominatorTree.immediateDominator(of: predecessor)!)
        }
      }

      switch afterContextSources.count {
      case 0:
        // Unreachable block.
        return Context()

      case 1:
        // Trivial merge.
        return contexts[afterContextSources.first!]!.after

      default:
        var result = contexts[afterContextSources.first!]!.after
        for predecessor in afterContextSources.dropFirst() {
          let afterContext = contexts[predecessor]!.after

          // Merge the locals.
          for (key, lhs) in result.locals {
            // Ignore definitions that don't dominate the block.
            guard let rhs = afterContext.locals[key] else {
              result.locals[key] = nil
              continue
            }

            // Merge both values conservatively.
            result.locals[key] = lhs && rhs
          }

          // Merge the state of the objects in memory.
          result.memory.merge(
            afterContext.memory,
            uniquingKeysWith: { (lhs, rhs) in
              assert(lhs.type == rhs.type)
              return Context.Cell(type: lhs.type, object: lhs.object && rhs.object)
            })
        }

        // Make sure the after-contexts are consistent with the computed before-context.
        for predecessor in visitedPredecessors {
          let afterContext = contexts[predecessor]!.after
          var didChange = false

          for key in result.locals.keys {
            switch result.locals[key]! {
            case .object(let lhs):
              // Operands have consistent types.
              let rhs = afterContext.locals[key]!.unwrapObject()!
              assert(lhs.isFull && rhs.isFull, "bad object operand state")

              // Find situations where an operand is initialized at the end of a predecessor but
              // deinitialized in the before context. In these cases, the operand is deinitialized
              // at the end of the predecessor.
              if lhs != rhs {
                // Deinitialize the object at the end of the predecessor.
                module.insert(
                  DeinitInstruction(key.operand(in: functionID)),
                  before: module.terminator(
                    of: Block.ID(function: functionID, address: predecessor))!)
                didChange = true
              }

            case .locations(let lhs):
              // Operands have consistent types.
              let rhs = afterContext.locals[key]!.unwrapLocations()!

              // Assume the objects at each location have the same state. That assertion holds
              // for the locations referred to by the before-state locals, unless DI or LoW has
              // been broken somewhere.
              let stateAtEntry = withObject(at: lhs.first!, { $0 })
              let stateAtExit = withObject(at: rhs.first!, { $0 })

              // Find situations where the initialization state of an object in memory at the end
              // of a predecessor is different in the before context. In those these, the object
              // is deinitialized at the end of the predecessor.
              let difference = stateAtExit.difference(stateAtEntry)
              if !difference.isEmpty {
                // Deinitialize the object at the end of the predecessor.
                let terminator = module.terminator(
                  of: Block.ID(function: functionID, address: predecessor))!
                let operand = key.operand(in: functionID)
                let rootType = module.type(of: operand).astType

                for path in difference {
                  let objectType = program.abstractLayout(of: rootType, at: path).type
                  let object = module.insert(
                    LoadInstruction(.object(objectType), from: operand, at: path),
                    before: terminator)[0]
                  module.insert(DeinitInstruction(object), before: terminator)
                }
                didChange = true
              }
            }
          }

          // Reinsert the predecessor in the work list if necessary.
          if didChange && (done.remove(predecessor) != nil) {
            var blocksToRevisit = [predecessor]
            while let b = blocksToRevisit.popLast() {
              done.remove(b)
              blocksToRevisit.append(contentsOf: cfg.successors(of: b).filter(done.contains(_:)))
            }
          }
        }

        return result
      }
    }

    // Reinitialize the internal state of the pass.
    self.functionID = functionID
    self.diagnostics.removeAll()

    // Establish the initial visitation order.
    work = Deque(dominatorTree.bfs)

    // Interpret the function until we reach a fixed point.
    while let block = work.popFirst() {
      // Handle the entry as a special case.
      if block == module[functionID].blocks.firstAddress {
        let beforeContext = entryContext(in: module)
        currentContext = beforeContext
        success = eval(block: block, in: &module) && success
        contexts[block] = (before: beforeContext, after: currentContext)
        done.insert(block)
        continue
      }

      // Make sure the block's immediate dominator and all non-dominated predecessors have been
      // visited, or pick another node.
      let predecessors = cfg.predecessors(of: block)
      if let dominator = dominatorTree.immediateDominator(of: block) {
        if contexts[dominator] == nil
          || predecessors.contains(where: { p in
            contexts[p] == nil && !dominatorTree.dominates(block, p)
          })
        {
          work.append(block)
          continue
        }
      } else {
        preconditionFailure("function has unreachable block")
      }

      // Merge the after-contexts of the predecessors.
      let beforeContext = mergeAfterContexts(of: predecessors)

      // If the before-context didn't change, we're done with the current block.
      if contexts[block]?.before == beforeContext {
        done.insert(block)
        continue
      }

      currentContext = beforeContext
      success = eval(block: block, in: &module) && success

      // We're done with the current block if ...
      let isBlockDone: Bool = {
        // 1) we found an error.
        if !success { return true }

        // 2) we're done with all of the block's predecessors.
        let pending = predecessors.filter({ !done.contains($0) })
        if pending.isEmpty { return true }

        // 3) the only predecessor left is the block itself, yet the after-context didn't change.
        return (pending.count == 1)
          && (pending[0] == block)
          && (contexts[block]?.after == currentContext)
      }()

      // Update the before/after-context pair for the current block and move to the next one.
      contexts[block] = (before: beforeContext, after: currentContext)
      if isBlockDone {
        done.insert(block)
      } else {
        work.append(block)
      }
    }

    return success
  }

  /// Returns the before-context of the function's entry block.
  private func entryContext(in module: Module) -> Context {
    let function = module[functionID]
    let entryAddress = function.blocks.firstAddress!
    var entryContext = Context()

    for i in 0 ..< function.inputs.count {
      let key = FunctionLocal.param(block: entryAddress, index: i)
      let (convention, type) = function.inputs[i]
      switch convention {
      case .let, .inout:
        let location = MemoryLocation.arg(index: i)
        entryContext.locals[key] = .locations([location])
        entryContext.memory[location] = Context.Cell(
          type: type.astType, object: .full(.initialized))

      case .set:
        let location = MemoryLocation.arg(index: i)
        entryContext.locals[key] = .locations([location])
        entryContext.memory[location] = Context.Cell(
          type: type.astType, object: .full(.uninitialized))

      case .sink:
        entryContext.locals[key] = .object(.full(.initialized))

      case .yielded:
        preconditionFailure("cannot represent instance of yielded type")
      }
    }

    return entryContext
  }

  private mutating func eval(block: Function.Blocks.Address, in module: inout Module) -> Bool {
    let instructions = module[functionID][block].instructions
    for i in instructions.indices {
      let id = InstructionID(functionID, block, i.address)
      switch instructions[i.address] {
      case let instruction as AllocStackInstruction:
        if !eval(allocStack: instruction, id: id, module: &module) { return false }
      case let instruction as BorrowInstruction:
        if !eval(borrow: instruction, id: id, module: &module) { return false }
      case let instruction as CondBranchInstruction:
        if !eval(condBranch: instruction, id: id, module: &module) { return false }
      case let instruction as CallInstruction:
        if !eval(call: instruction, id: id, module: &module) { return false }
      case let instruction as DeallocStackInstruction:
        if !eval(deallocStack: instruction, id: id, module: &module) { return false }
      case let instruction as DeinitInstruction:
        if !eval(deinit: instruction, id: id, module: &module) { return false }
      case let instruction as DestructureInstruction:
        if !eval(destructure: instruction, id: id, module: &module) { return false }
      case let instruction as LoadInstruction:
        if !eval(load: instruction, id: id, module: &module) { return false }
      case let instruction as RecordInstruction:
        if !eval(record: instruction, id: id, module: &module) { return false }
      case let instruction as ReturnInstruction:
        if !eval(return: instruction, id: id, module: &module) { return false }
      case let instruction as StoreInstruction:
        if !eval(store: instruction, id: id, module: &module) { return false }
      case is BranchInstruction, is EndBorrowInstruction, is UnrechableInstruction:
        continue
      default:
        unreachable("unexpected instruction")
      }
    }
    return true
  }

  private mutating func eval(
    allocStack instruction: AllocStackInstruction, id: InstructionID, module: inout Module
  ) -> Bool {
    // Create an abstract location denoting the newly allocated memory.
    let location = MemoryLocation.instruction(block: id.block, address: id.address)
    if currentContext.memory[location] != nil {
      diagnostics.append(.unboundedStackAllocation(at: instruction.range))
      return false
    }

    // Update the context.
    currentContext.memory[location] = Context.Cell(
      type: instruction.allocatedType, object: .full(.uninitialized))
    currentContext.locals[FunctionLocal(id, 0)] = .locations([location])
    return true
  }

  private mutating func eval(
    borrow instruction: BorrowInstruction, id: InstructionID, module: inout Module
  ) -> Bool {
    // Operand must a location.
    let locations: [MemoryLocation]
    if let key = FunctionLocal(operand: instruction.location) {
      locations = currentContext.locals[key]!.unwrapLocations()!.map({
        $0.appending(instruction.path)
      })
    } else {
      // The operand is a constant.
      fatalError("not implemented")
    }

    // Assume the objects at each location have the same summary. That assertion holds unless DI
    // or LoE has been broken somewhere else.
    let summary = withObject(at: locations[0], { $0.summary })

    switch instruction.capability {
    case .let, .inout:
      // `let` and `inout` require the borrowed object to be initialized.
      switch summary {
      case .fullyInitialized:
        break

      case .fullyUninitialized:
        diagnostics.append(.useOfUninitializedObject(at: instruction.range))
        return false

      case .fullyConsumed:
        diagnostics.append(.useOfConsumedObject(at: instruction.range))
        return false

      case .partiallyInitialized:
        diagnostics.append(.useOfPartiallyInitializedObject(at: instruction.range))
        return false

      case .partiallyConsumed:
        diagnostics.append(.useOfPartiallyConsumedObject(at: instruction.range))
        return false
      }

    case .set:
      // `set` requires the borrowed object to be uninitialized.
      let initializedPaths: [[Int]]
      switch summary {
      case .fullyUninitialized, .fullyConsumed:
        initializedPaths = []
      case .fullyInitialized:
        initializedPaths = [instruction.path]
      case .partiallyInitialized(let paths):
        initializedPaths = paths.map({ instruction.path + $0 })
      case .partiallyConsumed(_, let paths):
        initializedPaths = paths.map({ instruction.path + $0 })
      }

      // Nothing to do if the location is already uninitialized.
      if initializedPaths.isEmpty { break }

      // Deinitialize the object(s) at the location.
      let rootType = module.type(of: instruction.location).astType

      for path in initializedPaths {
        let objectType = program.abstractLayout(of: rootType, at: path).type
        let object = module.insert(
          LoadInstruction(
            .object(objectType), from: instruction.location, at: path, range: instruction.range),
          before: id)[0]
        module.insert(DeinitInstruction(object, range: instruction.range), before: id)
      }

      // We can skip the visit of the instructions that were just inserted and update the context
      // with their result directly.
      for l in locations {
        withObject(at: l, { object in object = .full(.uninitialized) })
      }

    case .yielded, .sink:
      unreachable()
    }

    currentContext.locals[FunctionLocal(id, 0)] = .locations(Set(locations))
    return true
  }

  private mutating func eval(
    condBranch instruction: CondBranchInstruction, id: InstructionID, module: inout Module
  ) -> Bool {
    // Consume the condition operand.
    let key = FunctionLocal(operand: instruction.condition)!
    return consume(
      localForKey: key, with: id,
      or: { (this, _) in
        this.diagnostics.append(.illegalMove(at: instruction.range))
      })
  }

  private mutating func eval(
    call instruction: CallInstruction, id: InstructionID, module: inout Module
  ) -> Bool {
    // Process the operands.
    for i in 0 ..< instruction.operands.count {
      switch instruction.conventions[i] {
      case .let, .inout, .set:
        // Nothing to do here.
        continue

      case .sink:
        // Consumes the operand unless it's a constant.
        if let key = FunctionLocal(operand: instruction.operands[i]) {
          if !consume(
            localForKey: key, with: id,
            or: { (this, _) in
              this.diagnostics.append(.illegalMove(at: instruction.range))
            })
          {
            return false
          }
        }

      case .yielded:
        unreachable()
      }
    }

    // Result is initialized.
    currentContext.locals[FunctionLocal(id, 0)] = .object(.full(.initialized))
    return true
  }

  private mutating func eval(
    deallocStack instruction: DeallocStackInstruction, id: InstructionID, module: inout Module
  ) -> Bool {
    // The location operand is the result an `alloc_stack` instruction.
    let alloc = module[instruction.location.instruction!] as! AllocStackInstruction

    let key = FunctionLocal(instruction.location.instruction!, 0)
    let locations = currentContext.locals[key]!.unwrapLocations()!
    assert(locations.count == 1)

    // Make sure the memory at the deallocated location is consumed or uninitialized.
    let initializedPaths: [[Int]] = withObject(
      at: locations.first!,
      { object in
        switch object.summary {
        case .fullyUninitialized, .fullyConsumed:
          return []
        case .fullyInitialized:
          return [[]]
        case .partiallyInitialized(let paths):
          return paths
        case .partiallyConsumed(_, let paths):
          return paths
        }
      })

    for path in initializedPaths {
      let object = module.insert(
        LoadInstruction(
          .object(program.abstractLayout(of: alloc.allocatedType, at: path).type),
          from: instruction.location,
          at: path,
          range: instruction.range),
        before: id)[0]
      module.insert(DeinitInstruction(object, range: instruction.range), before: id)

      // Apply the effect of the inserted instructions on the context directly.
      let consumer = InstructionID(
        id.function,
        id.block,
        module[id.function][id.block].instructions.address(before: id.address)!)
      currentContext.locals[FunctionLocal(id, 0)] = .object(.full(.consumed(by: [consumer])))
    }

    // Erase the deallocated memory from the context.
    currentContext.memory[locations.first!] = nil
    return true
  }

  private mutating func eval(
    deinit instruction: DeinitInstruction, id: InstructionID, module: inout Module
  ) -> Bool {
    // Consume the object operand.
    let key = FunctionLocal(operand: instruction.object)!
    return consume(
      localForKey: key, with: id,
      or: { (this, _) in
        this.diagnostics.append(.illegalMove(at: instruction.range))
      })
  }

  private mutating func eval(
    destructure instruction: DestructureInstruction, id: InstructionID, module: inout Module
  ) -> Bool {
    // Consume the object operand.
    if let key = FunctionLocal(operand: instruction.object) {
      if !consume(
        localForKey: key, with: id,
        or: { (this, _) in
          this.diagnostics.append(.illegalMove(at: instruction.range))
        })
      {
        return false
      }
    }

    // Result are initialized.
    for i in 0 ..< instruction.types.count {
      currentContext.locals[FunctionLocal(id, i)] = .object(.full(.initialized))
    }
    return true
  }

  private mutating func eval(
    load instruction: LoadInstruction, id: InstructionID, module: inout Module
  ) -> Bool {
    // Operand must be a location.
    let locations: [MemoryLocation]
    if let key = FunctionLocal(operand: instruction.source) {
      locations = currentContext.locals[key]!.unwrapLocations()!.map({
        $0.appending(instruction.path)
      })
    } else {
      // The operand is a constant.
      fatalError("not implemented")
    }

    // Object at target location must be initialized.
    for location in locations {
      if let diagnostic = withObject(
        at: location,
        { (object) -> Diagnostic? in
          switch object.summary {
          case .fullyInitialized:
            object = .full(.consumed(by: [id]))
            return nil
          case .fullyUninitialized:
            return .useOfUninitializedObject(at: instruction.range)
          case .fullyConsumed:
            return .useOfConsumedObject(at: instruction.range)
          case .partiallyInitialized:
            return .useOfPartiallyInitializedObject(at: instruction.range)
          case .partiallyConsumed:
            return .useOfPartiallyConsumedObject(at: instruction.range)
          }
        })
      {
        diagnostics.append(diagnostic)
        return false
      }
    }

    // Result is initialized.
    currentContext.locals[FunctionLocal(id, 0)] = .object(.full(.initialized))
    return true
  }

  private mutating func eval(
    record instruction: RecordInstruction, id: InstructionID, module: inout Module
  ) -> Bool {
    // Consumes the non-constant operand.
    for operand in instruction.operands {
      if let key = FunctionLocal(operand: operand) {
        if !consume(
          localForKey: key, with: id,
          or: { (this, _) in
            this.diagnostics.append(.illegalMove(at: instruction.range))
          })
        {
          return false
        }
      }
    }

    // Result is initialized.
    currentContext.locals[FunctionLocal(id, 0)] = .object(.full(.initialized))
    return true
  }

  private mutating func eval(
    return instruction: ReturnInstruction, id: InstructionID, module: inout Module
  ) -> Bool {
    // Consume the object operand.
    if let key = FunctionLocal(operand: instruction.value) {
      if !consume(
        localForKey: key, with: id,
        or: { (this, _) in
          this.diagnostics.append(.illegalMove(at: instruction.range))
        })
      {
        return false
      }
    }

    return true
  }

  private mutating func eval(
    store instruction: StoreInstruction, id: InstructionID, module: inout Module
  ) -> Bool {
    // Consume the object operand.
    if let key = FunctionLocal(operand: instruction.object) {
      if !consume(
        localForKey: key, with: id,
        or: { (this, _) in
          this.diagnostics.append(.illegalMove(at: instruction.range))
        })
      {
        return false
      }
    }

    // Target operand must be a location.
    let locations: Set<MemoryLocation>
    if let key = FunctionLocal(operand: instruction.target) {
      locations = currentContext.locals[key]!.unwrapLocations()!
    } else {
      // The operand is a constant.
      fatalError("not implemented")
    }

    // Update the context.
    for location in locations {
      withObject(at: location, { object in object = .full(.initialized) })
    }
    return true
  }

  /// Returns the result of a call to `action` with a projection of the object at `location`.
  private mutating func withObject<T>(
    at location: MemoryLocation, _ action: (inout Object) -> T
  ) -> T {
    switch location {
    case .null:
      preconditionFailure("null location")

    case .arg, .instruction:
      return action(&currentContext.memory[location]!.object)

    case .sublocation(let rootLocation, let path):
      if path.isEmpty {
        return action(&currentContext.memory[location]!.object)
      } else {
        return modifying(
          &currentContext.memory[rootLocation]!,
          { root in
            var derivedType = root.type
            var derivedPath = \Context.Cell.object
            for offset in path {
              // TODO: Handle tail-allocated objects.
              assert(offset >= 0, "not implemented")

              // Disaggregate the object if necessary.
              let layout = root[keyPath: derivedPath]
                .disaggregate(type: derivedType, program: program)

              // Create a path to the sub-object.
              derivedType = layout.storedPropertiesTypes[offset]
              derivedPath = derivedPath.appending(path: \Object.[offset])
            }

            // Project the sub-object.
            return action(&root[keyPath: derivedPath])
          })
      }
    }
  }

  /// Consumes the object in the specified local register.
  ///
  /// The method returns `true` if it succeeded. Otherwise, it or calls `handleFailure` with a
  /// with a projection of `self` and the state summary of the object before returning `false`.
  private mutating func consume(
    localForKey key: FunctionLocal,
    with consumer: InstructionID,
    or handleFailure: (inout Self, Object.StateSummary) -> Void
  ) -> Bool {
    let summary = currentContext.locals[key]!.unwrapObject()!.summary
    if summary == .fullyInitialized {
      currentContext.locals[key]! = .object(.full(.consumed(by: [consumer])))
      return true
    } else {
      handleFailure(&self, summary)
      return false
    }
  }

}

extension DefiniteInitializationPass {

  /// An abstract memory location.
  fileprivate enum MemoryLocation: Hashable {

    /// The null location.
    case null

    /// The location of a an argument to a `let`, `inout`, or `set` parameter.
    case arg(index: Int)

    /// A location produced by an instruction.
    case instruction(block: Function.Blocks.Address, address: Block.Instructions.Address)

    /// A sub-location rooted at an argument or an instruction.
    indirect case sublocation(root: MemoryLocation, path: [Int])

    /// The canonical form of `self`.
    var canonical: MemoryLocation {
      switch self {
      case .sublocation(let root, let path) where path.isEmpty:
        return root
      default:
        return self
      }
    }

    /// Returns a new locating created by appending the given path to this one.
    func appending(_ suffix: [Int]) -> MemoryLocation {
      if suffix.isEmpty { return self }

      switch self {
      case .null:
        preconditionFailure("null location")
      case .arg, .instruction:
        return .sublocation(root: self, path: suffix)
      case .sublocation(let root, let prefix):
        return .sublocation(root: root, path: prefix + suffix)
      }
    }

    func hash(into hasher: inout Hasher) {
      switch self {
      case .null:
        hasher.combine(-1)
      case .arg(let i):
        hasher.combine(i)
      case .instruction(let b, let a):
        hasher.combine(b)
        hasher.combine(a)
      case .sublocation(let r, let p):
        hasher.combine(r)
        for i in p { hasher.combine(i) }
      }
    }

    static func == (l: MemoryLocation, r: MemoryLocation) -> Bool {
      switch (l, r) {
      case (.null, .null):
        return true
      case (.arg(let a), .arg(let b)):
        return a == b
      case (.instruction(let a0, let a1), .instruction(let b0, let b1)):
        return (a0 == b0) && (a1 == b1)
      case (.sublocation(let a0, let a1), .sublocation(let b0, let b1)):
        return (a0 == b0) && (a1 == b1)
      case (.sublocation(let a0, let a1), _) where a1.isEmpty:
        return a0 == r
      case (_, .sublocation(let b0, let b1)) where b1.isEmpty:
        return l == b0
      default:
        return false
      }
    }

  }

  /// An abstract object.
  fileprivate enum Object: Equatable {

    /// The initialization state of an object or sub-object.
    enum State: Equatable {

      case initialized

      case uninitialized

      case consumed(by: Set<InstructionID>)

      /// Returns `lhs` merged with `rhs`.
      static func && (lhs: State, rhs: State) -> State {
        switch lhs {
        case .initialized:
          return rhs

        case .uninitialized:
          return rhs == .initialized ? lhs : rhs

        case .consumed(var a):
          if case .consumed(let b) = rhs {
            a.formUnion(b)
          }
          return .consumed(by: a)
        }
      }

    }

    /// The summary of an the initialization state of an object and its parts.
    enum StateSummary: Equatable {

      /// The object and all its parts are initialized.
      case fullyInitialized

      /// The object is fully uninitialized.
      case fullyUninitialized

      /// The object is fully consumed.
      ///
      /// The payload contains the set of instructions that consumed the object.
      case fullyConsumed(consumers: Set<InstructionID>)

      /// The object has at least one uninitialized part, at least one initialized part, and no
      /// consumed part.
      ///
      /// The payload contains the paths to the (partially) initialized parts of the object.
      case partiallyInitialized(initialized: [[Int]])

      /// The object has at least one consumed part and one initialized part.
      ///
      /// The payload contains the set of instructions that consumed the object or some of its
      /// parts, and the paths to the (partially) initialized parts.
      case partiallyConsumed(consumers: Set<InstructionID>, initialized: [[Int]])

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
    var summary: StateSummary {
      switch self {
      case .full(.initialized):
        return .fullyInitialized

      case .full(.uninitialized):
        return .fullyUninitialized

      case .full(.consumed(let consumers)):
        return .fullyConsumed(consumers: consumers)

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
          return .fullyConsumed(consumers: consumers)
        } else {
          return .partiallyConsumed(consumers: consumers, initialized: initializedPaths)
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

  }

}

extension Diagnostic {

  fileprivate static func illegalMove(at range: SourceRange?) -> Diagnostic {
    .error("illegal move", range: range)
  }

  fileprivate static func unboundedStackAllocation(at range: SourceRange?) -> Diagnostic {
    .error("unbounded stack allocation", range: range)
  }

  fileprivate static func useOfConsumedObject(at range: SourceRange?) -> Diagnostic {
    .error("use of consumed object", range: range)
  }

  fileprivate static func useOfPartiallyConsumedObject(at range: SourceRange?) -> Diagnostic {
    .error("use of partially consumed object", range: range)
  }

  fileprivate static func useOfPartiallyInitializedObject(at range: SourceRange?) -> Diagnostic {
    .error("use of partially initialized object", range: range)
  }

  fileprivate static func useOfUninitializedObject(at range: SourceRange?) -> Diagnostic {
    .error("use of uninitialized object", range: range)
  }

}
