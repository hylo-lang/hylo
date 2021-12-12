/// This file implements Val's mendatory "typestate" analysis, verifying memory safety properties.
/// The pass typically operates on raw VIL and transforms the latter into checked VIL.
///
/// The analysis is implemented as an abstract interpretation of VIL functions that keeps track of
/// an abastract representation (or typestate) of every value. The state of the whole interpreter
/// consists of two partial functions. One maps VIL registers to typestates or addresses. The other
/// maps addresses to typestates.

import AST
import Basic
import struct DequeModule.Deque

/// The abstract representation of an address.
fileprivate enum AbstractAddress: Hashable {

  /// A base address.
  case base(Operand)

  /// The address of an inline stored property.
  indirect case property(parent: AbstractAddress, property: String)

  /// The root of an existential package, in an existential container.
  indirect case existential(parent: AbstractAddress)

  /// A Boolean value that indicates whether this address is a base address.
  var isBase: Bool {
    if case .base = self {
      return true
    } else {
      return false
    }
  }

  /// The base of this address.
  var base: AbstractAddress {
    switch self {
    case .base:
      return self
    case .property(let parent, property: _):
      return parent.base
    case .existential(let parent):
      return parent.base
    }
  }

  /// The operand representing the provenance of the base of this address.
  var provenance: Operand {
    if case .base(let operand) = self {
      return operand
    } else {
      return base.provenance
    }
  }

}

/// The ownership state of an abstract value.
///
/// Ownership states form a lattice-like data structure that's used to merge flow-sensitive typing
/// contexts at confluence points. The structure does not have an infimum. Failure to compute the
/// "meet" of two states denotes a typing error.
fileprivate enum OwnershipState: Equatable {

  /// A merge error.
  struct MergeError: Error {

    let lhs: OwnershipState

    let rhs: OwnershipState

  }

  /// The value is independent and owned by the current context.
  case owned

  /// The value is borrowed from an existing location.
  case borrowed(origin: AbstractAddress)

  /// The value is owned, but it is being projected (i.e., borrowed immutably).
  case projected(borrowers: [InstIndex])

  /// The value is owned, but is being inouted (i.e., borrowed mutably).
  case inouted(borrower: InstIndex)

  /// The value has been consumed and is currently uninitialized.
  case consumed(consumer: InstIndex)

  /// The value is uninitialzed.
  ///
  /// Uninitialized values are implicitly "owned" by the context that knows about them, since they
  /// can't be borrowed, passed as argument, or returned.
  case uninitialized

  /// The value is a partially initialized record.
  ///
  /// This state represents a partially initialized `self` record in a constructor before all parts
  /// have been properly initialized. The associated value lists associates the record's parts to
  /// their individual state.
  case partial([String: OwnershipState])

  /// The value is owned, consumed or uninitialized depending on a condition evaluated at runtime.
  case dynamic(consumer: InstIndex?)

  /// Returns the "meet" (i.e., greatest lower bound) of two states.
  static func && (_ lhs: OwnershipState, _ rhs: OwnershipState) throws -> OwnershipState {
    switch (lhs, rhs) {
    case (.owned, .owned):
      return .owned
    case (.owned, .borrowed):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.owned, .projected):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.owned, .inouted):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.owned, .consumed(let consumer)):
      return .dynamic(consumer: consumer)
    case (.owned, .uninitialized):
      return .dynamic(consumer: nil)
    case (.owned, .partial(let props)):
      return try .partial(props.mapValues({ try lhs && $0 }))
    case (.owned, .dynamic):
      return rhs

    case (.borrowed(let a), .borrowed(let b)):
      guard a == b else { throw MergeError(lhs: lhs, rhs: rhs) }
      return lhs
    case (.borrowed, .projected):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.borrowed, .inouted):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.borrowed, .consumed):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.borrowed, .uninitialized):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.borrowed, .partial):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.borrowed, .dynamic):
      throw MergeError(lhs: lhs, rhs: rhs)

    case (.projected(let a), .projected(let b)):
      return .projected(borrowers: a + b)
    case (.projected, .inouted):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.projected, .consumed):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.projected, .uninitialized):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.projected, .partial):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.projected, .dynamic):
      throw MergeError(lhs: lhs, rhs: rhs)

    case (.inouted(let a), .inouted(let b)):
      guard a == b else { throw MergeError(lhs: lhs, rhs: rhs) }
      return lhs
    case (.inouted, .consumed):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.inouted, .uninitialized):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.inouted, .partial):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.inouted, .dynamic):
      throw MergeError(lhs: lhs, rhs: rhs)

    case (.consumed, .consumed):
      return lhs
    case (.consumed, .uninitialized):
      return lhs
    case (.consumed, .partial(let props)):
      return try .partial(props.mapValues({ try lhs && $0 }))
    case (.consumed, .dynamic):
      return rhs

    case (.uninitialized, .uninitialized):
      return .uninitialized
    case (.uninitialized, .partial):
      return rhs
    case (.uninitialized, .dynamic):
      return rhs

    case (.partial(let a), .partial(let b)):
      return try .partial(a.merging(b, uniquingKeysWith: &&))
    case (.partial(let a), .dynamic):
      return try .partial(a.mapValues({ try $0 && rhs }))

    case (.dynamic(let a), .dynamic(let b)):
      return .dynamic(consumer: a ?? b)

    default:
      return try rhs && lhs
    }
  }

}

/// An abstract value stored in memory or register.
fileprivate enum AbstractValue: Equatable {

  /// The address of a value stored in memory.
  case address(AbstractAddress)

  /// A value.
  case value(OwnershipState)

  static func && (_ lhs: AbstractValue, _ rhs: AbstractValue) throws -> AbstractValue {
    switch (lhs, rhs) {
    case (.address(let a), .address(let b)):
      precondition(a == b, "bad VIL")
      return lhs
    case (.value(let a), .value(let b)):
      return .value(try a && b)
    default:
      fatalError("bad VIL")
    }
  }

}

/// A context mapping register and memory locations to abstract values.
fileprivate struct AbstractContext: Equatable {

  var locals: [Operand: AbstractValue] = [:]

  var memory: [AbstractAddress: AbstractValue] = [:]

  /// Returns the address assigned to the specified register.
  func address(assignedTo register: Operand) -> AbstractAddress {
    guard case .address(let a) = locals[register] else { illegalOperand() }
    return a
  }

  /// Returns the ownership state of the value assigned to the specified register.
  func state(ofValueAssignedTo register: Operand) -> OwnershipState {
    guard case .value(let s) = locals[register] else { illegalOperand() }
    return s
  }

  /// Returns the ownership state of the value stored at the specified address.
  func state(ofValueAt address: AbstractAddress) -> OwnershipState {
    guard case .value(let s) = memory[address] else { illegalOperand() }
    return s
  }

  mutating func merge(_ other: AbstractContext) throws {
    try locals.merge(other.locals, uniquingKeysWith: &&)
    try memory.merge(other.memory, uniquingKeysWith: &&)
  }

}


/// A typestate error.
enum TypestateError: Error {

  case moveOfBorrowedValue

  case moveOfInoutedValue

  case moveOfProjectedValue

  case overlappingAccesses

  case useOfConsumedValue

  case useOfPartialValue

  case useOfUninitializedValue

}

/// The VIL pass implementing Val's typestate anaylsis.
public struct TypestateAnalysis {

  private var context = AbstractContext()

  private var module: Module!

  public init() {}

  public mutating func run(_ funName: String, on module: inout Module) -> Bool {
    withUnsafeMutablePointer(to: &module, { self.module = $0.move() })
    defer {
      withUnsafeMutablePointer(to: &module, { orig in
        withUnsafeMutablePointer(to: &self.module, { this in
          orig.initialize(to: this.move()!)
          this.initialize(to: nil)
        })
      })
    }

    let fun = module.functions[funName] ?< fatalError("function does not exist")
    guard let entry = fun.entry else { return true }

    // Build the function's dominator tree to establish the initial visiting order.
    let tree = DominatorTree(from: fun)
    var work = Deque(tree.breadthFirstBlocks)

    // Start by processing the entry. We can assume that it never needs to be processed more than
    // once, because it can't be the successor of any other block in well-formed VIL.
    assert(work.first == entry)
    assert(fun.cfg.edges(from: entry).allSatisfy({ $0.label == .forward }))
    var done: Set = [work.removeFirst()]

    var pre = AbstractContext()
    for (actual, formal) in zip(module.blocks[entry].params, fun.type.params!) {
      let arg = Operand(actual)

      switch formal.policy! {
      case .consuming, .consumingMutable:
        pre.locals[arg] = .value(.owned)

      case .local:
        pre.locals[arg] = .value(.borrowed(origin: .base(arg)))

      case .inout:
        let a: AbstractAddress = .base(arg)
        pre.locals[arg] = .address(a)
        pre.memory[a] = .value(.owned)
      }
    }

    var post = pre
    var success = process(block: entry, fun: fun, context: &post)
    if work.isEmpty { return success }

    // Go through the work list, (re)visiting each basic block until we reach a fixed point.
    var store: [BasicBlockIndex: (pre: AbstractContext, post: AbstractContext)] = [
      entry: (pre: pre, post: post)
    ]

    while let block = work.popFirst() {
      // Make sure we visited all of the block's predecessors, or pick another one.
      let predecessors = fun.cfg.edges(from: block).filter({ $0.label != .forward })
      guard predecessors.allSatisfy({ store[$0.target] != nil }) else {
        work.append(block)
        continue
      }

      // Compute the pre-context of the block, based on its own arguments and the post-context of
      // its predecessors.
      pre = predecessors.reduce(into: AbstractContext(), { (context, pred) in
        guard let post = store[pred.target]?.post else { return }
        do {
          try context.merge(post)
        } catch {
          success = false
        }
      })
      for arg in module.blocks[block].params where arg.type.isObject {
        pre.locals[Operand(arg)] = .value(.owned)
      }

      // If the pre-context didn't change, then we don't need to re-compute the post-context and
      // we're done with the current block.
      if store[block]?.pre == pre {
        done.insert(block)
        continue
      }

      post = pre
      success = process(block: block, fun: fun, context: &post)

      // We're done with the current block if ...
      let isBlockDone: Bool = {
        // 1) we found an error somewhere in the function.
        if !success { return true }

        // 2) we're done with all of the block's predecessors, or
        let pending = predecessors.filter({ !done.contains($0.target) })
        if pending.isEmpty { return true }

        // 3) the only predecessor left is the block itself, yet the post-context didn't change.
        return (pending.count == 1)
          && (pending[0].target == block)
          && (store[block]?.post == post)
      }()

      // Either way, store the pre/post context pair and move to the next block.
      store[block] = (pre: pre, post: post)
      if isBlockDone {
        done.insert(block)
      } else {
        work.append(block)
      }
    }

    return success
  }

  private func process(
    block: BasicBlockIndex,
    fun: VILFun,
    context: inout AbstractContext
  ) -> Bool {
//    var success = true
//    for index in fun.blocks[blockID]!.instructions.indices {
//      let path = InstPath(funName: fun.name, blockID: blockID, instIndex: index)
//      switch fun.blocks[blockID]!.instructions[index] {
//      case let inst as AllocStackInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as ApplyInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as CopyInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as CopyAddrInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as CheckedCastBranchInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as CopyExistentialInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as DeallocStackInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as DeleteInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as DeleteAddrInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as EqualAddrInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as InitExistentialAddrInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as LoadInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as RecordInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as RecordMemberInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as RecordMemberAddrInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as RetInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as StoreInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as WitnessMethodInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//
//      case
//        is BranchInst,
//        is CondBranchInst,
//        is HaltInst,
//        is ThinToThickInst:
//        continue
//
//      case
//        is CheckedCastAddrInst,
//        is PartialApplyInst,
//        is UnsafeCastAddrInst:
//        fatalError("Not implemented")
//
//      case let inst:
//        fatalError("unsupported instruction '\(inst)'")
//      }
//    }
//
//    return success
    return true
  }

//  private func visit(
//    inst: AllocStackInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    // Create an abstract address denoting the newly allocated memory. The address is tagged by the
//    // `alloc_stack` instruction to detect unbounded stack allocation in loops.
//    let addr: AbstractAddress = .base(ReferenceBox(inst))
//    assert(context.memory[addr] == nil, "bad VIL: stack allocation in loop")
//    context.locals[inst] = .address(addr)
//
//    if inst.isSelf {
//      // Allocation of `self` requires that the cell be marked as `partial` to track definitive
//      // assignment of all stored properties.
//      let ty = inst.allocType.valType as! ProductType
//      let props: [String: OwnershipState] = Dictionary(
//        uniqueKeysWithValues: ty.decl.storedVars.map({ ($0.name, .uninitialized) }))
//      context.memory[addr] = .value(.partial(props))
//    } else {
//      context.memory[addr] = .value(.uninitialized)
//    }
//
//    return true
//  }

//  private func visit(
//    inst: ApplyInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    // The result of a function call is always owned.
//    context.locals[inst] = .value(.owned)
//
//    // Verify and update the ownership state of each parameter.
//    var stateBeforeLoan: [AbstractAddress: AbstractValue] = [:]
//    var localOwnedArgs: [Value] = []
//    var success = true
//
//    for (actual, formal) in zip(inst.args,  inst.callee.type.params!) {
//      switch formal.policy {
//      case .local:
//        // `local` creates an immutable borrow.
//        let addr = context.address(assignedTo: actual)
//        success = borrow(addr, borrower: path, in: &<#T##AbstractContext#>)
//        success = consume(value: actual, consumer: path, in: &context) && success
//        localOwnedArgs.append(actual)
//
//      case .consuming, .consumingMutable:
//        // `consuming` and `consumingMutable` consumes the argument.
//        success = consume(value: actual, consumer: path, in: &context) && success
//
//      case .inout:
//        // `inout` requires uniqueness: the argument has to refer to an owned memory location.
//        let addr = loadAsAddr(actual, in: context)
//        stateBeforeLoan[addr] = context.memory[addr]
//
//        switch stage(at: addr, in: context) {
//        case .owned:
//          context.memory[addr] = .lent(borrower: path)
//        case .partial:
//          builder.context.report(.useOfPartialValue(location: actual))
//          success = false
//        case .lent:
//          builder.context.report(.overlappingAccess(location: actual))
//          success = false
//        case .moved(let cp):
//          builder.context.report(.useAfterMove(location: actual, consumer: builder.module[cp]))
//          success = false
//        case .uninitialized:
//          builder.context.report(.useBeforeInit(location: actual, anchor: nil))
//          success = false
//        default:
//          fatalError("bad VIL: illegal operand")
//        }
//      }
//    }
//
//    // Ends the loans and reclaim ownership for `@local_owned` arguments.
//    context.memory.merge(stateBeforeLoan, uniquingKeysWith: { _, rhs in rhs })
//    for arg in localOwnedArgs {
//      context.locals[arg] = .owned
//    }
//
//    return success
//  }
//
//  private func visit(
//    inst: CopyInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    switch context.locals[inst.value] {
//    case .owned:
//      context.locals[inst] = .owned
//      return true
//    case .partial:
//      builder.context.report(.useOfPartialValue(location: inst.value))
//    case .lent:
//      builder.context.report(.overlappingAccess(location: inst.value))
//    case .moved(let cp):
//      builder.context.report(.useAfterMove(location: inst, consumer: builder.module[cp]))
//    case .uninitialized:
//      builder.context.report(.useBeforeInit(location: inst, anchor: nil))
//    default:
//      fatalError("bad VIL: illegal operand")
//    }
//    return false
//  }
//
//  private func visit(
//    inst: CheckedCastBranchInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    return consume(value: inst.value, consumer: path, in: &context)
//  }
//
//  private func visit(
//    inst: CopyAddrInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    // The source location must be usable.
//    var success = assertUsable(
//      location: inst.source,
//      context: context,
//      builder: builder,
//      anchor: nil)
//
//    // The target location must be ready to receive ownership of a new value.
//    let target = loadAsAddr(inst.target, in: context)
//    switch context.memory[target] {
//    case .uninitialized, .moved:
//      // The target is uninitialized; we're good.
//      break
//
//    case .partial, .owned:
//      // The target holds an owned value; we must delete it first. If the value is only partially
//      // initialized, the visitor for `delete_addr` will handle the error.
//      builder.insertionPointer = InsertionPointer(before: path)
//      let i = builder.buildDeleteAddr(target: inst.target)
//      let p = builder.module.path(before: path)
//      success = visit(inst: i, path: p, context: &context, builder: &builder) && success
//
//    default:
//      fatalError("bad VIL: illegal operand")
//    }
//
//    context.memory[target] = .owned
//    if case .existential(let parent) = target {
//      context.memory[parent] = .owned
//    }
//
//    return success
//  }
//
//  private func visit(
//    inst: CopyExistentialInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    switch context.locals[inst.container] {
//    case .owned:
//      context.locals[inst] = .owned
//      return true
//
//    default:
//      fatalError("bad VIL: illegal operand")
//    }
//  }
//
//  private func visit(
//    inst: DeallocStackInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    // Make sure the deallocated address is removed from the abstract context.
//    let addr = loadAsAddr(inst.alloc, in: context)
//    assert(
//      addr.isBase && addr.provenance is AllocStackInst,
//      "bad VIL: operand of dealloc_stack is not a stack allocation")
//
//    switch context.memory[addr] {
//    case .uninitialized, .moved:
//      // The memory is uninitialized; we're good.
//      context.memory[addr] = nil
//      return true
//
//    case .partial, .owned:
//      // The memory holds an owned value; we must delete it first. If the value is only partially
//      // initialized, the visitor for `delete_addr` will handle the error.
//      builder.insertionPointer = InsertionPointer(before: path)
//      let i = builder.buildDeleteAddr(target: inst.alloc)
//      let p = builder.module.path(before: path)
//      let success = visit(inst: i, path: p, context: &context, builder: &builder)
//
//      context.memory[addr] = nil
//      return success
//
//    default:
//      fatalError("bad VIL: illegal operand")
//    }
//  }
//
//  private func visit(
//    inst: DeleteInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    return consume(value: inst.value, consumer: path, in: &context)
//  }
//
//  private func visit(
//    inst: DeleteAddrInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    let addr = loadAsAddr(inst.target, in: context)
//    switch stage(at: addr, in: context) {
//    case .owned:
//      context.memory[addr] = .uninitialized
//      return true
//
//    default:
//      // FIXME: Handle deallocation of unowned values gracefully.
//      fatalError("deinitialization of unowned value")
//    }
//  }
//
//  private func visit(
//    inst: EqualAddrInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    guard
//      case .address = context.locals[inst.lhs],
//      case .address = context.locals[inst.rhs]
//    else {
//      fatalError("bad VIL: illegal operand")
//    }
//    return true
//  }
//
//  private func visit(
//    inst: InitExistentialAddrInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    var success = true
//
//    let addr = loadAsAddr(inst.container, in: context)
//    switch context.memory[addr] {
//    case .uninitialized, .moved:
//      // The container is uninitialized; we're good.
//      break
//
//    case .partial, .owned:
//      // The container is fully initialized; we must delete it first.
//      builder.insertionPointer = InsertionPointer(before: path)
//      let i = builder.buildDeleteAddr(target: inst.container)
//      let p = builder.module.path(before: path)
//      success = visit(inst: i, path: p, context: &context, builder: &builder)
//
//    default:
//      fatalError("bad VIL: illegal operand")
//    }
//
//    context.memory[addr] = .owned
//    return consume(value: inst.value, consumer: path, in: &context) && success
//  }
//
//  private func visit(
//    inst: LoadInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    // The source location must be usable.
//    let success = assertUsable(
//      location: inst.location,
//      context: context,
//      builder: builder,
//      anchor: nil)
//
//    // Assume the instruction is correct, so that the analysis can carry on no matter what.
//    context.locals[inst] = .owned
//
//    // Moving consumes ownership.
//    let addr = loadAsAddr(inst.location, in: context)
//    if case .lent = context.memory[addr] {
//      fatalError("not implemented")
//    }
//
//    context.memory[addr] = .moved(consumer: path)
//    return success
//  }
//
//  private func visit(
//    inst: RecordInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    if inst.typeDecl.storedVars.isEmpty {
//      context.locals[inst] = .owned
//    } else {
//      context.locals[inst] = .partial(properties: inst.typeDecl.storedVars.map({ $0.name }))
//    }
//    return true
//  }
//
//  private func visit(
//    inst: RecordMemberInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    switch context.locals[inst.record] {
//    case .owned:
//      context.locals[inst] = .owned
//
//    default:
//      fatalError("bad VIL: illegal operand")
//    }
//
//    return true
//  }
//
//  private func visit(
//    inst: RecordMemberAddrInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    let addr = loadAsAddr(inst.record, in: context)
//    let propAddr = AbstractAddress.property(parent: addr, property: inst.memberDecl.name)
//    context.locals[inst] = .address(propAddr)
//
//    switch context.memory[addr] {
//    case .owned:
//      // If the whole record is owned, then so are its members.
//      context.memory[propAddr] = .owned
//      return true
//
//    case .partial:
//      // If the record is partially initialized, the state of the requested member depends on what
//      // parts have already been initialized. This information should be stored in the context.
//      if context.memory[propAddr] == nil {
//        context.memory[propAddr] = .uninitialized
//      }
//      return true
//
//    case .lent:
//      // FIXME: Emit a diganostic, don't fail.
//      fatalError("violation of uniqueness")
//
//    case .uninitialized:
//      builder.context.report(.useBeforeInit(location: inst, anchor: nil))
//      return false
//
//    case .moved(let consumer):
//      builder.context.report(
//        .useAfterMove(location: inst, consumer: builder.module[consumer]))
//      return false
//
//    default:
//      fatalError("bad VIL: illegal operand")
//    }
//  }
//
//  private func visit(
//    inst: RetInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    // We currently only support return with ownership.
//    return consume(value: inst.value, consumer: path, in: &context)
//  }
//
//  private func visit(
//    inst: StoreInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    var success = true
//
//    let addr = loadAsAddr(inst.target, in: context)
//    switch context.memory[addr] {
//    case .uninitialized, .moved:
//      // The target is uninitialized; we can store a new value to it.
//      break
//
//    case .owned:
//      // The target holds an owned value; we must delete it first. If the value is only partially
//      // initialized, the visitor for `delete_addr` will handle the error.
//      builder.insertionPointer = InsertionPointer(before: path)
//      let i = builder.buildDeleteAddr(target: inst.target)
//      let p = builder.module.path(before: path)
//      success = visit(inst: i, path: p, context: &context, builder: &builder)
//
//    case .partial:
//      // The target holds a partially initialized `self`; we must delete all initialized properties
//      // first. Note: We don't need to handle partial initialization recursively since only `self`
//      // can be partially initialized; its properties cannot.
//      fatalError("not implemented")
//
//    default:
//      fatalError("bad VIL: illegal operand")
//    }
//
//    // The target takes ownership of the source, unless it's a non-linear literal value.
//    success = consume(value: inst.value, consumer: path, in: &context) && success
//    context.memory[addr] = .owned
//
//    // Update the state of the address' owner, if any.
//    switch addr {
//    case .base:
//      break
//
//    case .property(let parent, _):
//      // If the address belongs to a record in state `partial`, we have to check for definitive
//      // assignment of all other properties and update the record's state if necessary.
//      if case .partial(let props) = context.memory[parent] {
//        if props.allSatisfy({
//          context.memory[.property(parent: parent, property: $0)] == .owned
//        }) {
//          context.memory[parent] = .owned
//        }
//      }
//
//    case .existential(let parent):
//      context.memory[parent] = .owned
//    }
//
//    return success
//  }
//
//  private func visit(
//    inst: WitnessMethodInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    switch context.locals[inst.container] {
//    case .owned:
//      context.locals[inst] = .owned
//      return true
//
//    default:
//      fatalError("bad VIL: illegal operand")
//    }
//  }
//
//  /// Returns the address assigned to the given register, or fails.
//  private func loadAsAddr(_ register: Value, in context: AbstractContext) -> AbstractAddress {
//    guard case .address(let addr) = context.locals[register] else {
//      fatalError("bad VIL: illegal operand")
//    }
//    return addr
//  }
//
//  /// Returns the stage of the value referenced by the given address.
//  private func stage(at addr: AbstractAddress, in context: AbstractContext) -> AbstractValue? {
//    switch addr {
//    case .base:
//      return context.memory[addr]
//    case .existential(let parent):
//      return stage(at: parent, in: context)
//    case .property(let parent, let property):
//      // FIXME
//      return stage(at: parent, in: context)
//    }
//  }
//
//  /// Updates the ownership stage of the value stored at the given address.
//  private func updateStage(
//    newValue: AbstractValue,
//    at addr: AbstractAddress,
//    in context: inout AbstractContext
//  ) {
//    switch addr {
//    case .base:
//      context.memory[addr] = newValue
//
//    case .existential(let parent):
//      updateStage(newValue: newValue, at: parent, in: &context)
//
//    case .property(let parent, _):
//      // The update relates to a stored property.
//      switch stage(at: parent, in: context) {
//      case .owned:
//        if newValue == .owned { return }
//        updateStage(newValue: newValue, at: parent, in: &context)
//
//      case .partial:
//        // I'm not sure what should happen here. Can this situation even occur?
//        fatalError("Not implemented")
//
//      default:
//        fatalError("unreachable")
//      }
//    }
//  }

  /// Borrows the value at the specified address.
  private func borrow(
    _ addr: AbstractAddress,
    borrower: InstIndex,
    in context: inout AbstractContext
  ) throws {
    guard case .value(let state) = context.memory[addr] else { illegalOperand() }
    switch state {
    case .owned:
      context.memory[addr] = .value(.projected(borrowers: [borrower]))

    case .borrowed(let origin):
      // FIXME: Probably something to do if the origin is an mutating argument.
      try borrow(origin, borrower: borrower, in: &context)

    case .projected(let borrowers):
      context.memory[addr] = .value(.projected(borrowers: borrowers + [borrower]))

    case .inouted:
      throw TypestateError.overlappingAccesses

    case .consumed:
      throw TypestateError.useOfConsumedValue

    case .uninitialized:
      throw TypestateError.useOfUninitializedValue

    case .partial(_):
      throw TypestateError.useOfPartialValue

    case .dynamic:
      fatalError("not implemented: use of dynamic")
    }
  }

  /// Consumes the ownership of the given value.
  private func consume(
    operand: Operand,
    consumer: InstIndex,
    in context: inout AbstractContext
  ) throws {
    // Constant VIL values are non-linear.
    if operand.constant != nil { return }

    guard case .value(let state) = context.locals[operand] else { illegalOperand() }
    switch state {
    case .owned:
      context.locals[operand] = .value(.consumed(consumer: consumer))

    case .borrowed:
      throw TypestateError.moveOfBorrowedValue

    case .projected:
      throw TypestateError.moveOfProjectedValue

    case .inouted:
      throw TypestateError.moveOfInoutedValue

    case .consumed:
      throw TypestateError.useOfConsumedValue

    case .uninitialized:
      throw TypestateError.useOfUninitializedValue

    case .partial:
      throw TypestateError.useOfPartialValue

    case .dynamic:
      fatalError("not implemented: use of dynamic")
    }
  }

//  /// Checks that the value at the specified location is ready to be used for copy or move and
//  /// emits an appropriate error message if it is not.
//  ///
//  /// - Parameters:
//  ///   - location: A VIL value representing a source address.
//  ///   - context: The context of the abstract interpreter.
//  ///   - builder: A VIL builder.
//  ///   - anchor: A source range related to the use.
//  private func assertUsable(
//    location: Value,
//    context: AbstractContext,
//    builder: Builder,
//    anchor: SourceRange?
//  ) -> Bool {
//    let addr = loadAsAddr(location, in: context)
//    switch stage(at: addr, in: context) {
//    case .uninitialized:
//      // The source location is not initialized; that's a use before initialization.
//      builder.context.report(.useBeforeInit(location: location, anchor: anchor))
//      return false
//
//    case .moved(let consumer):
//      // The source location has been moved; that's a use after move.
//      builder.context.report(
//        .useAfterMove(location: location, consumer: builder.module[consumer]))
//      return false
//
//    case .partial:
//      // Can this happen? Should be use before init?
//      fatalError("Not implemented")
//
//    case .owned, .lent:
//      // The source location is initialized; we're good.
//      return true
//
//    default:
//      fatalError("bad VIL: illegal operand")
//    }
//  }

}
fileprivate func illegalOperand() -> Never {
  fatalError("bad VIL: illegal operand")
}
