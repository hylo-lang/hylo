import DequeModule
import Utils

/// The lifetime pass of IR analysis.
public struct DefiniteInitializationPass {

  /// The pass is implemented as an abstract interpreter keeping track of the initialization state
  /// of the objects in registers and memory.
  ///
  /// The interpreter relies on the IR being well-formed.

  /// An abstract memory location.
  private enum MemoryLocation: Hashable {

    /// The null location.
    case null

    /// The location of a an argument to a `let`, `inout`, or `set` parameter.
    case arg(index: Int)

    /// A location produced by an instruction.
    case inst(block: Function.BlockAddress, address: Block.InstAddress)

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
      case .arg, .inst:
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
      case .inst(let b, let a):
        hasher.combine(b)
        hasher.combine(a)
      case.sublocation(let r, let p):
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
      case (.inst(let a0, let a1), .inst(let b0, let b1)):
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
  private enum Object: Equatable {

    /// The initialization state of an object or sub-object.
    enum State: Equatable {

      case initialized

      case uninitialized

      case consumed(by: InstID)

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
      case fullyConsumed(consumers: Set<InstID>)

      /// The object has at least one uninitialized part, at least one initialized part, and no
      /// consumed part.
      ///
      /// The payload contains the paths to the (partially) initialized parts of the object.
      case partiallyInitialized(initialized: [[Int]])

      /// The object has at least one consumed part and one initialized part.
      ///
      /// The payload contains the set of instructions that consumed the object or some of its
      /// parts, and the paths to the (partially) initialized parts.
      case partiallyConsumed(consumers: Set<InstID>, initialized: [[Int]])

    }

    /// An object whose all parts have the same state.
    case full(State)

    /// An object whose part may have different states.
    ///
    /// - Requires: The payload must be non-empty.
    case partial([Object])

    /// A summary of the initialization of the object and its parts.
    var summary: StateSummary {
      switch self {
      case .full(.initialized):
        return .fullyInitialized

      case .full(.uninitialized):
        return .fullyUninitialized

      case .full(.consumed(let consumer)):
        return .fullyConsumed(consumers: [consumer])

      case .partial(let parts):
        var hasUninitializedPart = false
        var initializedPaths: [[Int]] = []
        var consumers: Set<InstID> = []

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

    /// Given `self == .full(s)`, assigns `self` to `.partial([obj_1, ..., obj_n])` where `obj_i`
    /// is `.full(s)` and `n` is the number of stored parts in `type`. Otherwise, does nothing.
    ///
    /// - Returns: The layout of `type`.
    ///
    /// - Requires: `type` must have a record layout and at least one stored property.
    mutating func disaggregate(type: Type, program: TypedProgram) -> TypeLayout {
      let layout = TypeLayout(type, in: program)
      guard case .full(let s) = self else { return layout }

      let n = layout.storedPropertiesTypes.count
      precondition(n != 0)
      self = .partial(Array(repeating: .full(s), count: n))
      return layout
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

  }

  /// An abstract value.
  private enum Value: Equatable {

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

  }

  /// A key in the local store of an abstract interpretation context.
  private enum LocalKey: Hashable {

    /// An instruction.
    case inst(block: Function.BlockAddress, address: Block.InstAddress)

    /// A block parameter.
    case param(block: Function.BlockAddress, index: Int)

    /// Given `operand` is an instruction ID or a parameter, creates the corresponding key.
    /// Otherwise, returns `nil`.
    init?(operand: Operand) {
      switch operand {
      case .inst(let i):
        self = .inst(block: i.block, address: i.address)
      case .parameter(let block, let index):
        self = .param(block: block.address, index: index)
      case .constant:
        return nil
      }
    }

    /// Creates an instruction key from an instruction ID.
    init(_ instID: InstID) {
      self = .inst(block: instID.block, address: instID.address)
    }

  }

  /// An abstract interpretation context.
  private struct Context: Equatable {

    /// A memory cell.
    struct Cell: Equatable {

      /// The type of the object in the cell.
      var type: Type

      /// The object in the cell.
      var object: Object

    }

    /// The values of the locals.
    var locals: [LocalKey: Value] = [:]

    /// The state of the memory.
    var memory: [MemoryLocation: Cell] = [:]

    mutating func merge(_ other: Context) {

    }

  }

  /// The program being lowered.
  public let program: TypedProgram

  /// The ID of the function being interpreted.
  private var functionID: Function.ID = -1

  /// The current evaluation context.
  private var context = Context()

  /// The diagnostics collected during the pass.
  private var diagnostics: [Diagnostic] = []

  public init(program: TypedProgram) {
    self.program = program
  }

  /// Runs the pass and returns whether it succeeded without any error.
  public mutating func run(function functionID: Function.ID, module: inout Module) -> Bool {
    /// The control flow graph of the function to analyze.
    let cfg = module[functionID].cfg
    /// A FILO list of blocks to visit.
    var work: Deque<Function.BlockAddress>
    /// The set of blocks that no longer need to be visited.
    var done: Set<Function.BlockAddress> = []
    /// The state of the abstract interpreter before and after the visited basic blocks.
    var contexts: [Function.BlockAddress: (before: Context, after: Context)] = [:]
    /// Indicates whether the pass succeeded.
    var success = true

    // Reset the internal state of the pass.
    self.functionID = functionID
    self.diagnostics.removeAll()

    // Establish the initial visitation order.
    let dominatorTree = DominatorTree(function: functionID, cfg: cfg, in: module)
    work = Deque(dominatorTree.bfs)

    // Interpret the function until we reach a fixed point.
    while let block = work.popFirst() {
      // Make sure the block's predecessors have been visited, or pick another one.
      let predecessors = cfg.predecessors(of: block)
      if !predecessors.allSatisfy({ contexts[$0] != nil }) {
        work.append(block)
        continue
      }

      // Compute the before-context of the block.
      var beforeContext: Context
      if block == module[functionID].blocks.firstAddress {
        // The block is the entry.
        beforeContext = entryContext(in: module)
      } else {
        // Merge the after-contexts of the predecessors.
        beforeContext = predecessors.reduce(into: Context(), { $0.merge(contexts[$1]!.after) })
      }

      // If the before-context didn't change, we're done with the current block.
      if contexts[block]?.before == beforeContext {
        done.insert(block)
        continue
      }

      context = beforeContext
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
            && (contexts[block]?.after == context)
      }()

      // Update the before/after-context pair for the current block and move to the next one.
      contexts[block] = (before: beforeContext, after: context)
      if isBlockDone {
        done.insert(block)
      } else {
        work.append(block)
      }
    }

    return success
  }

  /// Creates the before-context of the function's entry block.
  private func entryContext(in module: Module) -> Context {
    let function = module[functionID]
    let entryAddress = function.blocks.firstAddress!
    var entryContext = Context()

    for i in 0 ..< function.inputs.count {
      let key = LocalKey.param(block: entryAddress, index: i)
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

  private mutating func eval(block: Function.BlockAddress, in module: inout Module) -> Bool {
    let instructions = module[functionID][block].instructions
    for i in instructions.indices {
      let id = InstID(function: functionID, block: block, address: i.address)
      switch instructions[i] {
      case let inst as AllocStackInst:
        if !eval(allocStack: inst, id: id, module: &module) { return false }
      case let inst as BorrowInst:
        if !eval(borrow: inst, id: id, module: &module) { return false }
      case let inst as CallInst:
        if !eval(call: inst, id: id, module: &module) { return false }
      case let inst as EndBorrowInst:
        if !eval(endBorrow: inst, id: id, module: &module) { return false }
      case let inst as LoadInst:
        if !eval(load: inst, id: id, module: &module) { return false }
      case let inst as RecordInst:
        if !eval(record: inst, id: id, module: &module) { return false }
      case let inst as StoreInst:
        if !eval(store: inst, id: id, module: &module) { return false }
      case let inst as TakeMemberInst:
        if !eval(takeMember: inst, id: id, module: &module) { return false }
      default:
        unreachable("unexpected instruction")
      }
    }
    return true
  }

  private mutating func eval(
    allocStack inst: AllocStackInst, id: InstID, module: inout Module
  ) -> Bool {
    // Create an abstract location denoting the newly allocated memory.
    let location = MemoryLocation.inst(block: id.block, address: id.address)
    if context.memory[location] != nil {
      diagnostics.append(.unboundedStackAllocation(range: inst.range))
      return false
    }

    // Update the context.
    context.memory[location] = Context.Cell(type: inst.objectType, object: .full(.uninitialized))
    context.locals[LocalKey(id)] = .locations([location])
    return true
  }

  private mutating func eval(
    borrow inst: BorrowInst, id: InstID, module: inout Module
  ) -> Bool {
    // Operand must a location.
    let locations: [MemoryLocation]
    if let key = LocalKey(operand: inst.value) {
      locations = context.locals[key]!.unwrapLocations()!.map({ $0.appending(inst.path) })
    } else {
      // The operand is a constant.
      fatalError("not implemented")
    }

    switch inst.capability {
    case .let, .inout:
      // `let` and `inout` borrows require the borrowed object to be initialized.
      for location in locations {
        if let diagnostic = withObject(at: location, { (object) -> Diagnostic? in
          switch object.summary {
          case .fullyInitialized:
            return nil
          case .fullyUninitialized:
            return .useOfUninitializedObject(range: inst.range)
          case .fullyConsumed:
            return .useOfConsumedObject(range: inst.range)
          case .partiallyInitialized:
            return .useOfPartiallyInitializedObject(range: inst.range)
          case .partiallyConsumed:
            return .useOfPartiallyConsumedObject(range: inst.range)
          }
        }) {
          diagnostics.append(diagnostic)
          return false
        }
      }

    case .set:
      // `set` borrowes require the borrowed object to be uninitialized.
      for location in locations {
        if !withObject(at: location, { (object) -> Bool in
          switch object.summary {
          case .fullyUninitialized, .fullyConsumed:
            return true
          default:
            return false
          }
        }) {
          // TODO: Destroy initialized parts
        }
      }

    case .yielded:
      unreachable()
    }

    context.locals[LocalKey(id)] = .locations(Set(locations))
    return true
  }

  private mutating func eval(
    call inst: CallInst, id: InstID, module: inout Module
  ) -> Bool {
    // Process the operands.
    for i in 0 ..< inst.operands.count {
      switch inst.conventions[i] {
      case .let, .inout, .set:
        // Nothing to do here.
        continue

      case .sink:
        // Consumes the operand unless it's a constant.
        if let key = LocalKey(operand: inst.operands[i]) {
          if !consume(localForKey: key, with: id, or: { (this, _) in
            this.diagnostics.append(.illegalMove(range: inst.range))
          }) {
            return false
          }
        }

      case .yielded:
        unreachable()
      }
    }

    // Result is initialized.
    context.locals[LocalKey(id)] = .object(.full(.initialized))
    return true
  }

  private mutating func eval(
    endBorrow inst: EndBorrowInst, id: InstID, module: inout Module
  ) -> Bool {
    // Nothing to do.
    return true
  }

  private mutating func eval(
    load inst: LoadInst, id: InstID, module: inout Module
  ) -> Bool {
    // Operand must be a location.
    let locations: Set<MemoryLocation>
    if let key = LocalKey(operand: inst.source) {
      locations = context.locals[key]!.unwrapLocations()!
    } else {
      // The operand is a constant.
      fatalError("not implemented")
    }

    // Object at target location must be initialized.
    for location in locations {
      if let diagnostic = withObject(at: location, { (object) -> Diagnostic? in
        switch object.summary {
        case .fullyInitialized:
          object = .full(.consumed(by: id))
          return nil
        case .fullyUninitialized:
          return .useOfUninitializedObject(range: inst.range)
        case .fullyConsumed:
          return .useOfConsumedObject(range: inst.range)
        case .partiallyInitialized:
          return .useOfPartiallyInitializedObject(range: inst.range)
        case .partiallyConsumed:
          return .useOfPartiallyConsumedObject(range: inst.range)
        }
      }) {
        diagnostics.append(diagnostic)
        return false
      }
    }

    return true
  }

  private mutating func eval(
    record inst: RecordInst, id: InstID, module: inout Module
  ) -> Bool {
    // Consumes the non-constant operand.
    for operand in inst.operands {
      if let key = LocalKey(operand: operand) {
        if !consume(localForKey: key, with: id, or: { (this, _) in
          this.diagnostics.append(.illegalMove(range: inst.range))
        }) {
          return false
        }
      }
    }

    // Result is initialized.
    context.locals[LocalKey(id)] = .object(.full(.initialized))
    return true
  }

  private mutating func eval(
    store inst: StoreInst, id: InstID, module: inout Module
  ) -> Bool {
    // Consume the object operand.
    if let key = LocalKey(operand: inst.object) {
      if !consume(localForKey: key, with: id, or: { (this, _) in
        this.diagnostics.append(.illegalMove(range: inst.range))
      }) {
        return false
      }
    }

    // Target operand must be a location.
    let locations: Set<MemoryLocation>
    if let key = LocalKey(operand: inst.target) {
      locations = context.locals[key]!.unwrapLocations()!
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

  private mutating func eval(
    takeMember inst: TakeMemberInst, id: InstID, module: inout Module
  ) -> Bool {
    // TODO
    return true
  }

  /// Returns the result of a call to `action` with a projection of the object at `location`.
  private mutating func withObject<T>(
    at location: MemoryLocation, _ action: (inout Object) -> T
  ) -> T {
    switch location {
    case .null:
      preconditionFailure("null location")

    case .arg, .inst:
      return action(&context.memory[location]!.object)

    case .sublocation(let rootLocation, let path):
      if path.isEmpty {
        return action(&context.memory[location]!.object)
      } else {
        return modifying(&context.memory[rootLocation]!, { root in
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
    localForKey key: LocalKey,
    with consumer: InstID,
    or handleFailure: (inout Self, Object.StateSummary) -> ()
  ) -> Bool {
    let summary = context.locals[key]!.unwrapObject()!.summary
    if summary == .fullyInitialized {
      context.locals[key]! = .object(.full(.consumed(by: consumer)))
      return true
    } else {
      handleFailure(&self, summary)
      return false
    }
  }

}

extension Diagnostic {

  fileprivate static func illegalMove(range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "illegal move",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  fileprivate static func unboundedStackAllocation(range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "unbounded stack allocation",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  fileprivate static func useOfConsumedObject(range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "use of consumed object",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  fileprivate static func useOfPartiallyConsumedObject(range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "use of partially consumed object",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  fileprivate static func useOfPartiallyInitializedObject(range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "use of partially initialized object",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  fileprivate static func useOfUninitializedObject(range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "use of uninitialized object",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

}
