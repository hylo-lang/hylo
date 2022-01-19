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
  case base(ReferenceBox<Value>)

  /// The address of an inline stored property.
  indirect case property(parent: AbstractAddress, property: String)

  /// The root of an existential package, in an existential container.
  indirect case existential(parent: AbstractAddress)

  /// A flag that indicates whether this address is a base address.
  var isBase: Bool {
    if case .base = self {
      return true
    } else {
      return false
    }
  }

  /// The base of the address.
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

  /// The instruction representing the allocation of the base of this address.
  var provenance: Value {
    if case .base(let box) = self {
      return box.value
    } else {
      return base.provenance
    }
  }

}


/// The typestate of an abstract value stored in memory or register.
fileprivate enum AbstractValue: Equatable {

  /// A value that is owned, moved, or uninitialized, depending on a dynamic condition.
  case dynamic(consumer: InstPath?)

  /// An uninitialized value.
  ///
  /// Uninitialized values are implicitly "owned" by the context that knows about them, since they
  /// can't be shared, or passed as an argument.
  case uninitialized

  /// A value that has moved (or consumed) by an instruction.
  case moved(consumer: InstPath)

  /// A value that has been borrowed by an instruction.
  case lent(borrower: InstPath?)

  /// A partially initialized `self` record.
  ///
  /// This state represents a partially initialized `self` record in a constructor before all parts
  /// have been properly initialized. The associated value lists the record's stored properties and
  /// is used to determine when it becomes fully initialized.
  case partial(properties: [String])

  /// An independent value that's "owned" by the context.
  case owned

  /// The address of a value stored in memory.
  case address(AbstractAddress)

  /// Returns the "meet" (i.e., greatest lower bound) of two abstract values.
  static func meet(_ lhs: AbstractValue, _ rhs: AbstractValue) -> AbstractValue {
    switch lhs {
    case .uninitialized:
      switch rhs {
      case .uninitialized:
        // uninitialized ∧ uninitialized = uninitialized
        return rhs
      case .moved, .dynamic:
        // uninitialized ∧ moved = moved
        // uninitialized ∧ dynamic = dynamic
        return rhs
      case .owned:
        // uninitialized ∧ owned = dynamic
        return .dynamic(consumer: nil)
      case .lent:
        // uninitialized ∧ lent = X
        fatalError("bad VIL: incompatible abstract values")
      default: break
      }

    case .moved(let consumer):
      switch rhs {
      case .uninitialized, .moved:
        // moved ∧ uninitialized = moved
        // moved ∧ moved = moved
        return .moved(consumer: consumer)
      case .owned, .dynamic:
        // moved ∧ owned = dynamic
        // moved ∧ dynamic = dynamic
        return .dynamic(consumer: consumer)
      case .lent:
        // moved ∧ lent = X
        fatalError("bad VIL: incompatible abstract values")
      default:
        break
      }

    case .lent(let borrower):
      switch rhs {
      case .lent(let other):
        // lent ∧ lent = lent, if and only if the borrower is the same.
        if borrower == other {
          return lhs
        } else {
          fatalError("bad VIL: incompatible loans")
        }
      case .owned:
        // lent ∧ owned = lent
        return lhs
      case .uninitialized, .moved, .dynamic:
        // lent ∧ uninitialized = X
        // lent ∧ moved = X
        // lent ∧ dynamic = X
        fatalError("bad VIL: incompatible abstract values")
      default:
        break
      }

    case .owned:
      switch rhs {
      case .uninitialized:
        // owned ∧ uninitialized = dynamic
        return .dynamic(consumer: nil)
      case .moved(let consumer):
        // owned ∧ moved = dynamic
        return .dynamic(consumer: consumer)
      case .lent(let borrower):
        // owned ∧ lent = dynamic
        return .lent(borrower: borrower)
      case .owned:
        // owned ∧ owned = owned
        return .owned
      case .dynamic:
        // owned ∧ dynamic = dynamic
        return rhs
      default:
        break
      }

    case .dynamic:
      switch rhs {
      case .uninitialized, .moved, .owned, .dynamic:
        // dynamic ∧ uninitialized = dynamic
        // dynamic ∧ moved = dynamic
        // dynamic ∧ owned = dynamic
        // dynamic ∧ dynamic = dynamic
        return lhs
      case .lent:
        // dynamic ∧ lent = X
        fatalError("bad VIL: incompatible abstract values")
      default: break
      }

    default:
      break
    }

    if lhs == rhs {
      return lhs
    } else {
      fatalError("bad VIL: incompatible abstract values")
    }
  }

}

/// A context mapping register and memory locations to abstract values.
fileprivate struct AbstractContext: Equatable {

  var locals: ReferenceTable<Value, AbstractValue> = [:]

  var memory: [AbstractAddress: AbstractValue] = [:]

  mutating func merge(_ other: AbstractContext) {
    locals.merge(other.locals, uniquingKeysWith: AbstractValue.meet(_:_:))
    memory.merge(other.memory, uniquingKeysWith: AbstractValue.meet(_:_:))
  }

}

/// The VIL pass implementing Val's typestate anaylsis.
public struct TypestateAnalysis {

  public init() {}

  public func run(_ funName: VILName, with builder: inout Builder) -> Bool {
    let fun = builder.module.functions[funName] ?< fatalError("function does not exist")
    guard let entryID = fun.entryID else { return true }

    // Build the function's dominator tree to establish the initial visiting order.
    let tree = DominatorTree(fun: fun)
    var work = Deque(tree.breadthFirstBlocks)

    // Start by processing the entry. We can assume that it never needs to be processed more than
    // once, because it can't be the successor of any other block in well-formed VIL.
    assert(work.first == entryID)
    assert(fun.cfg.edges(from: entryID).allSatisfy({ $0.label == .forward }))
    var done: Set = [work.removeFirst()]

    var pre = AbstractContext()
    for (param, conv) in zip(fun.entry!.params, fun.type.paramConvs) {
      switch conv {
      case .owned, .localOwned:
        pre.locals[param] = .owned

      case .mutating:
        let a: AbstractAddress = .base(ReferenceBox(param))
        pre.locals[param] = .address(a)
        pre.memory[a] = .owned
      }
    }

    var post = pre
    var success = process(blockID: entryID, fun: fun, context: &post, builder: &builder)
    if work.isEmpty { return success }

    // Go through the work list, (re)visiting each basic block until we reach a fixed point.
    var store: [BasicBlock.ID: (pre: AbstractContext, post: AbstractContext)] = [
      entryID: (pre: pre, post: post)
    ]

    while let blockID = work.popFirst() {
      // Make sure we visited all of the block's predecessors, or pick another one.
      let predecessors = fun.cfg.edges(from: blockID).filter({ $0.label != .forward })
      guard predecessors.allSatisfy({ store[$0.target] != nil }) else {
        work.append(blockID)
        continue
      }

      // Compute the pre-context of the block, based on its own arguments and the post-context of
      // its predecessors.
      pre = predecessors.reduce(into: AbstractContext(), { (context, pred) in
        guard let post = store[pred.target]?.post else { return }
        context.merge(post)
      })
      for param in fun.blocks[blockID]!.params {
        pre.locals[param] = .owned
      }

      // If the pre-context didn't change, then we don't need to re-compute the post-context and
      // we're done with the current block.
      if store[blockID]?.pre == pre {
        done.insert(blockID)
        continue
      }

      post = pre
      success = process(blockID: blockID, fun: fun, context: &post, builder: &builder)

      // We're done with the current block if ...
      let isBlockDone: Bool = {
        // 1) we found an error somewhere in the function.
        if !success { return true }

        // 2) we're done with all of the block's predecessors, or
        let pending = predecessors.filter({ !done.contains($0.target) })
        if pending.isEmpty { return true }

        // 3) the only predecessor left is the block itself, yet the post-context didn't change.
        return (pending.count == 1)
          && (pending[0].target == blockID)
          && (store[blockID]?.post == post)
      }()

      // Either way, store the pre/post context pair and move to the next block.
      store[blockID] = (pre: pre, post: post)
      if isBlockDone {
        done.insert(blockID)
      } else {
        work.append(blockID)
      }
    }

    return success
  }

  private func process(
    blockID: BasicBlock.ID,
    fun: VILFun,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    var success = true
    for index in fun.blocks[blockID]!.instructions.indices {
      let path = InstPath(funName: fun.name, blockID: blockID, instIndex: index)
      switch fun.blocks[blockID]!.instructions[index] {
      case let inst as AllocStackInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as ApplyInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as CopyInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as CopyAddrInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as CheckedCastBranchInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as CopyExistentialInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as DeallocStackInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as DeleteInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as DeleteAddrInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as EqualAddrInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as InitExistentialAddrInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as LoadInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as RecordInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as RecordMemberInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as RecordMemberAddrInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as RetInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as StoreInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as WitnessMethodInst:
        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success

      case
        is BranchInst,
        is CondBranchInst,
        is HaltInst,
        is ThinToThickInst:
        continue

      case
        is CheckedCastAddrInst,
        is PartialApplyInst,
        is UnsafeCastAddrInst:
        fatalError("Not implemented")

      case let inst:
        fatalError("unsupported instruction '\(inst)'")
      }
    }

    return success
  }

  private func visit(
    inst: AllocStackInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    // Create an abstract address denoting the newly allocated memory. The address is tagged by the
    // `alloc_stack` instruction to detect unbounded stack allocation in loops.
    let addr: AbstractAddress = .base(ReferenceBox(inst))
    assert(context.memory[addr] == nil, "bad VIL: stack allocation in loop")
    context.locals[inst] = .address(addr)

    if inst.isSelf {
      // Allocation of `self` requires that the cell be marked as `partial` to track definitive
      // assignment of all stored properties.
      let ty = inst.allocatedType.valType as! ProductType
      context.memory[addr] = .partial(properties: ty.decl.storedVars.map({ $0.name }))
    } else {
      context.memory[addr] = .uninitialized
    }

    return true
  }

  private func visit(
    inst: ApplyInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    let funType = inst.callee.type as! VILFunType

    // `@owned` is currently the only legal return convention.
    assert(funType.retConv == .owned)
    context.locals[inst] = .owned

    // Verify and update the ownership stage of each parameter.
    var stageBeforeLoan: [AbstractAddress: AbstractValue] = [:]
    var localOwnedArgs: [Value] = []
    var success = true

    for (arg, conv) in zip(inst.args, funType.paramConvs) {
      switch conv {
      case .owned:
        // `@owned` consumes the argument.
        success = consume(value: arg, consumer: path, in: &context, with: &builder) && success

      case .localOwned:
        // `@local_owned` consumes the argument too, but since it can't escape, it "comes back" at
        // the end of the call.
        success = consume(value: arg, consumer: path, in: &context, with: &builder) && success
        localOwnedArgs.append(arg)

      case .mutating:
        // `@mutating` requires uniqueness: the argument has to refer to an owned memory location.
        let addr = loadAsAddr(arg, in: context)
        stageBeforeLoan[addr] = context.memory[addr]

        switch stage(at: addr, in: context) {
        case .owned:
          context.memory[addr] = .lent(borrower: path)
        case .partial:
          builder.context.report(.useOfPartialValue(location: arg))
          success = false
        case .lent:
          builder.context.report(.overlappingAccess(location: arg))
          success = false
        case .moved(let cp):
          builder.context.report(.useAfterMove(location: arg, consumer: builder.module[cp]))
          success = false
        case .uninitialized:
          builder.context.report(.useBeforeInit(location: arg, anchor: nil))
          success = false
        default:
          fatalError("bad VIL: illegal operand")
        }
      }
    }

    // Ends the loans and reclaim ownership for `@local_owned` arguments.
    context.memory.merge(stageBeforeLoan, uniquingKeysWith: { _, rhs in rhs })
    for arg in localOwnedArgs {
      context.locals[arg] = .owned
    }

    return success
  }

  private func visit(
    inst: CopyInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    switch context.locals[inst.value] {
    case .owned:
      context.locals[inst] = .owned
      return true
    case .partial:
      builder.context.report(.useOfPartialValue(location: inst.value))
    case .lent:
      builder.context.report(.overlappingAccess(location: inst.value))
    case .moved(let cp):
      builder.context.report(.useAfterMove(location: inst, consumer: builder.module[cp]))
    case .uninitialized:
      builder.context.report(.useBeforeInit(location: inst, anchor: nil))
    default:
      fatalError("bad VIL: illegal operand")
    }
    return false
  }

  private func visit(
    inst: CheckedCastBranchInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    return consume(value: inst.value, consumer: path, in: &context, with: &builder)
  }

  private func visit(
    inst: CopyAddrInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    // The source location must be usable.
    var success = assertUsable(
      location: inst.source,
      context: context,
      builder: builder,
      anchor: nil)

    // The target location must be ready to receive ownership of a new value.
    let target = loadAsAddr(inst.target, in: context)
    switch context.memory[target] {
    case .uninitialized, .moved:
      // The target is uninitialized; we're good.
      break

    case .partial, .owned:
      // The target holds an owned value; we must delete it first. If the value is only partially
      // initialized, the visitor for `delete_addr` will handle the error.
      builder.insertionPointer = InsertionPointer(before: path)
      let i = builder.buildDeleteAddr(target: inst.target)
      let p = builder.module.path(before: path)
      success = visit(inst: i, path: p, context: &context, builder: &builder) && success

    default:
      fatalError("bad VIL: illegal operand")
    }

    context.memory[target] = .owned
    if case .existential(let parent) = target {
      context.memory[parent] = .owned
    }

    return success
  }

  private func visit(
    inst: CopyExistentialInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    switch context.locals[inst.container] {
    case .owned:
      context.locals[inst] = .owned
      return true

    default:
      fatalError("bad VIL: illegal operand")
    }
  }

  private func visit(
    inst: DeallocStackInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    // Make sure the deallocated address is removed from the abstract context.
    let addr = loadAsAddr(inst.alloc, in: context)
    assert(
      addr.isBase && addr.provenance is AllocStackInst,
      "bad VIL: operand of dealloc_stack is not a stack allocation")

    switch context.memory[addr] {
    case .uninitialized, .moved:
      // The memory is uninitialized; we're good.
      context.memory[addr] = nil
      return true

    case .partial, .owned:
      // The memory holds an owned value; we must delete it first. If the value is only partially
      // initialized, the visitor for `delete_addr` will handle the error.
      builder.insertionPointer = InsertionPointer(before: path)
      let i = builder.buildDeleteAddr(target: inst.alloc)
      let p = builder.module.path(before: path)
      let success = visit(inst: i, path: p, context: &context, builder: &builder)

      context.memory[addr] = nil
      return success

    default:
      fatalError("bad VIL: illegal operand")
    }
  }

  private func visit(
    inst: DeleteInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    return consume(value: inst.value, consumer: path, in: &context, with: &builder)
  }

  private func visit(
    inst: DeleteAddrInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    let addr = loadAsAddr(inst.target, in: context)
    switch stage(at: addr, in: context) {
    case .owned:
      context.memory[addr] = .uninitialized
      return true

    default:
      // FIXME: Handle deallocation of unowned values gracefully.
      fatalError("deinitialization of unowned value")
    }
  }

  private func visit(
    inst: EqualAddrInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    guard
      case .address = context.locals[inst.lhs],
      case .address = context.locals[inst.rhs]
    else {
      fatalError("bad VIL: illegal operand")
    }
    return true
  }

  private func visit(
    inst: InitExistentialAddrInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    var success = true

    let addr = loadAsAddr(inst.container, in: context)
    switch context.memory[addr] {
    case .uninitialized, .moved:
      // The container is uninitialized; we're good.
      break

    case .partial, .owned:
      // The container is fully initialized; we must delete it first.
      builder.insertionPointer = InsertionPointer(before: path)
      let i = builder.buildDeleteAddr(target: inst.container)
      let p = builder.module.path(before: path)
      success = visit(inst: i, path: p, context: &context, builder: &builder)

    default:
      fatalError("bad VIL: illegal operand")
    }

    context.memory[addr] = .owned
    return consume(value: inst.value, consumer: path, in: &context, with: &builder) && success
  }

  private func visit(
    inst: LoadInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    // The source location must be usable.
    let success = assertUsable(
      location: inst.location,
      context: context,
      builder: builder,
      anchor: nil)

    // Assume the instruction is correct, so that the analysis can carry on no matter what.
    context.locals[inst] = .owned

    switch inst.semantics {
    case .move:
      // Moving consumes ownership.
      let addr = loadAsAddr(inst.location, in: context)
      if case .lent = context.memory[addr] {
        fatalError("not implemented")
      }

      context.memory[addr] = .moved(consumer: path)
      return success

    case .copy:
      // Copying produces a new value with ownership.
      return success
    }
  }

  private func visit(
    inst: RecordInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    if inst.typeDecl.storedVars.isEmpty {
      context.locals[inst] = .owned
    } else {
      context.locals[inst] = .partial(properties: inst.typeDecl.storedVars.map({ $0.name }))
    }
    return true
  }

  private func visit(
    inst: RecordMemberInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    switch context.locals[inst.record] {
    case .owned:
      context.locals[inst] = .owned

    default:
      fatalError("bad VIL: illegal operand")
    }

    return true
  }

  private func visit(
    inst: RecordMemberAddrInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    let addr = loadAsAddr(inst.record, in: context)
    let propAddr = AbstractAddress.property(parent: addr, property: inst.memberDecl.name)
    context.locals[inst] = .address(propAddr)

    switch context.memory[addr] {
    case .owned:
      // If the whole record is owned, then so are its members.
      context.memory[propAddr] = .owned
      return true

    case .partial:
      // If the record is partially initialized, the state of the requested member depends on what
      // parts have already been initialized. This information should be stored in the context.
      if context.memory[propAddr] == nil {
        context.memory[propAddr] = .uninitialized
      }
      return true

    case .lent:
      // FIXME: Emit a diganostic, don't fail.
      fatalError("violation of uniqueness")

    case .uninitialized:
      builder.context.report(.useBeforeInit(location: inst, anchor: nil))
      return false

    case .moved(let consumer):
      builder.context.report(
        .useAfterMove(location: inst, consumer: builder.module[consumer]))
      return false

    default:
      fatalError("bad VIL: illegal operand")
    }
  }

  private func visit(
    inst: RetInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    // We currently only support return with ownership.
    return consume(value: inst.value, consumer: path, in: &context, with: &builder)
  }

  private func visit(
    inst: StoreInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    var success = true

    let addr = loadAsAddr(inst.target, in: context)
    switch context.memory[addr] {
    case .uninitialized, .moved:
      // The target is uninitialized; we can store a new value to it.
      break

    case .owned:
      // The target holds an owned value; we must delete it first. If the value is only partially
      // initialized, the visitor for `delete_addr` will handle the error.
      builder.insertionPointer = InsertionPointer(before: path)
      let i = builder.buildDeleteAddr(target: inst.target)
      let p = builder.module.path(before: path)
      success = visit(inst: i, path: p, context: &context, builder: &builder)

    case .partial:
      // The target holds a partially initialized `self`; we must delete all initialized properties
      // first. Note: We don't need to handle partial initialization recursively since only `self`
      // can be partially initialized; its properties cannot.
      fatalError("not implemented")

    default:
      fatalError("bad VIL: illegal operand")
    }

    // The target takes ownership of the source, unless it's a non-linear literal value.
    success = consume(value: inst.value, consumer: path, in: &context, with: &builder) && success
    context.memory[addr] = .owned

    // Update the state of the address' owner, if any.
    switch addr {
    case .base:
      break

    case .property(let parent, _):
      // If the address belongs to a record in state `partial`, we have to check for definitive
      // assignment of all other properties and update the record's state if necessary.
      if case .partial(let props) = context.memory[parent] {
        if props.allSatisfy({
          context.memory[.property(parent: parent, property: $0)] == .owned
        }) {
          context.memory[parent] = .owned
        }
      }

    case .existential(let parent):
      context.memory[parent] = .owned
    }

    return success
  }

  private func visit(
    inst: WitnessMethodInst,
    path: InstPath,
    context: inout AbstractContext,
    builder: inout Builder
  ) -> Bool {
    switch context.locals[inst.container] {
    case .owned:
      context.locals[inst] = .owned
      return true

    default:
      fatalError("bad VIL: illegal operand")
    }
  }

  /// Returns the address assigned to the given register, or fails.
  private func loadAsAddr(_ register: Value, in context: AbstractContext) -> AbstractAddress {
    guard case .address(let addr) = context.locals[register] else {
      fatalError("bad VIL: illegal operand")
    }
    return addr
  }

  /// Returns the stage of the value referenced by the given address.
  private func stage(at addr: AbstractAddress, in context: AbstractContext) -> AbstractValue? {
    switch addr {
    case .base:
      return context.memory[addr]
    case .existential(let parent):
      return stage(at: parent, in: context)
    case .property(let parent, let property):
      // FIXME
      return stage(at: parent, in: context)
    }
  }

  /// Updates the ownership stage of the value stored at the given address.
  private func updateStage(
    newValue: AbstractValue,
    at addr: AbstractAddress,
    in context: inout AbstractContext
  ) {
    switch addr {
    case .base:
      context.memory[addr] = newValue

    case .existential(let parent):
      updateStage(newValue: newValue, at: parent, in: &context)

    case .property(let parent, _):
      // The update relates to a stored property.
      switch stage(at: parent, in: context) {
      case .owned:
        if newValue == .owned { return }
        updateStage(newValue: newValue, at: parent, in: &context)

      case .partial:
        // I'm not sure what should happen here. Can this situation even occur?
        fatalError("Not implemented")

      default:
        fatalError("unreachable")
      }
    }
  }

  /// Consumes the ownership of the given value.
  ///
  /// If `value` is a literal value, the method always succeeds and does not update the context.
  ///
  /// - Returns: If the method succeeds, it returns `true` and updates `context.locals[value]` to
  ///   to reflect the ownership transfer. Otherwise, it returns `false` and reports the cause of
  ///   the failure.
  private func consume(
    value: Value,
    consumer: InstPath,
    in context: inout AbstractContext,
    with builder: inout Builder
  ) -> Bool {
    // Literal values are non-linear.
    if value is LiteralValue { return true }

    // Attempt to consume the value.
    switch context.locals[value] {
    case .owned:
      context.locals[value] = .moved(consumer: consumer)
      return true
    case .partial:
      builder.context.report(.useOfPartialValue(location: value))
    case .lent:
      builder.context.report(.overlappingAccess(location: value))
    case .moved(let cp):
      builder.context.report(.useAfterMove(location: value, consumer: builder.module[cp]))
    case .uninitialized:
      builder.context.report(.useBeforeInit(location: value, anchor: nil))
    default:
      fatalError("bad VIL: illegal operand")
    }
    return false
  }

  /// Checks that the value at the specified location is ready to be used for copy or move and
  /// emits an appropriate error message if it is not.
  ///
  /// - Parameters:
  ///   - location: A VIL value representing a source address.
  ///   - context: The context of the abstract interpreter.
  ///   - builder: A VIL builder.
  ///   - anchor: A source range related to the use.
  private func assertUsable(
    location: Value,
    context: AbstractContext,
    builder: Builder,
    anchor: SourceRange?
  ) -> Bool {
    let addr = loadAsAddr(location, in: context)
    switch stage(at: addr, in: context) {
    case .uninitialized:
      // The source location is not initialized; that's a use before initialization.
      builder.context.report(.useBeforeInit(location: location, anchor: anchor))
      return false

    case .moved(let consumer):
      // The source location has been moved; that's a use after move.
      builder.context.report(
        .useAfterMove(location: location, consumer: builder.module[consumer]))
      return false

    case .partial:
      // Can this happen? Should be use before init?
      fatalError("Not implemented")

    case .owned, .lent:
      // The source location is initialized; we're good.
      return true

    default:
      fatalError("bad VIL: illegal operand")
    }
  }

}
