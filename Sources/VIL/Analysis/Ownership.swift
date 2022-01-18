/// This file implements Val's mendatory ownership analysis, which verifies lifetime safety and
/// uniqueness of mutable access.
///
/// The analysis is implemented as an intraprocedural abstract interpretation that tracks the
/// ownership state of every object. It starts from a function's entry and follows the edges of
/// that function's CFG until it reaches a fixed point.
///
/// Errors are reported as they are encountered but do not stop the interpreter. Instead, the
/// analysis "pretends" all instructions are legal so that it can explore the entire function.

import AST
import Basic
import struct DequeModule.Deque

/// An abstract context mapping register and memory locations to abstract values, representing the
/// state of the abstract interpreter.
fileprivate struct AbstractContext: Equatable {

  private var locals: [Operand: AbstractValue] = [:]

  private var memory: [AbstractAddress: AbstractObject] = [:]

  /// Accesses the value in the specified register for reading and writing.
  subscript(in register: Operand) -> AbstractValue? {
    get {
      return locals[register]
    }
    set {
      precondition(newValue != nil, "cannot unassign register")
      if let newObject = newValue?.asObject?.canonical {
        assert(newObject.verify())
        locals[register] = .object(newObject)
      } else {
        locals[register] = newValue
      }
    }
  }

  /// Accesses the object stored at the specified address for reading and writing.
  ///
  /// - Returns: If there is an object at `address.base`, returns an object whose ownership state
  ///   is at most as high as that of the object at `address.base`. Otherwise, returns `nil`.
  subscript(at address: AbstractAddress) -> AbstractObject? {
    get {
      if case .part(let parent, let property) = address {
        if let container = self[at: parent] {
          return container.parts[property] ?? AbstractObject(state: container.state, parts: [:])
        } else {
          return nil
        }
      } else {
        return memory[address]
      }
    }
    set(newObject) {
      if case .part(let parent, let property) = address {
        if var container = self[at: parent] {
          container.parts[property] = newObject
          self[at: parent] = container
        } else if let newObject = newObject {
          self[at: parent] = AbstractObject(state: newObject.state, parts: [property: newObject])
        }
      } else if let newObject = newObject?.canonical {
        assert(newObject.verify())
        memory[address] = newObject
      } else {
        memory[address] = nil
      }
    }
  }

  /// Consumes the object at the specified address.
  mutating func consume(
    _ address: AbstractAddress,
    with consumer: InstIndex
  ) throws -> AbstractObject {
    guard let pre = self[at: address] else { illegalOperand() }
    var post = pre
    try post.consume(with: consumer)
    self[at: address] = post
    return pre
  }

  /// Consumes the specified operand.
  mutating func consume(
    _ operand: Operand,
    with consumer: InstIndex
  ) throws -> AbstractObject {
    // Constant values are non-linear.
    guard operand.constant == nil else { return .owned }

    guard let pre = self[in: operand]!.asObject else { illegalOperand() }
    var post = pre
    try post.consume(with: consumer)
    self[in: operand] = .object(post)
    return pre
  }

  /// Lends the object at the specified address.
  mutating func lend(_ address: AbstractAddress, mutably: Bool, to borrower: InstIndex) throws {
    guard var object = self[at: address] else { illegalOperand() }
    try object.lend(mutably: mutably, to: borrower)
    self[at: address] = object
  }

  /// Ends a loan on the object at the specified address.
  ///
  /// The method silently ignores cases where `borrower` doesn't belong to the object's borrower
  /// set. That situation occurs when the abstract interpreter is ending an invalid loan. Hence,
  /// we can assume that the error was reported when the loan started.
  mutating func endLoan(at address: AbstractAddress, to borrower: InstIndex) {
    guard var object = self[at: address] else { illegalOperand() }
    object.endLoan(to: borrower)
    self[at: address] = object
  }

  mutating func merge(_ other: AbstractContext) throws {
    try locals.merge(other.locals, uniquingKeysWith: &&)
    try memory.merge(other.memory, uniquingKeysWith: &&)
  }

}

/// An abstract value stored in memory or register.
fileprivate enum AbstractValue: Equatable {

  /// The address of a value stored in memory.
  case address(AbstractAddress)

  /// An object.
  case object(AbstractObject)

  /// Safely unwraps the address represented by the value.
  var asAddress: AbstractAddress? {
    guard case .address(let a) = self else { return nil }
    return a
  }

  /// Safely unwraps the object represented by the value.
  var asObject: AbstractObject? {
    guard case .object(let o) = self else { return nil }
    return o
  }

  static func && (_ lhs: AbstractValue, _ rhs: AbstractValue) throws -> AbstractValue {
    switch (lhs, rhs) {
    case (.address(let a), .address(let b)):
      precondition(a == b, "bad VIL: addresses have different origins")
      return lhs

    case (.object(let a), .object(let b)):
      return .object(try a && b)

    default:
      fatalError("bad VIL: cannot merge objects and addresses")
    }
  }

}

/// The abstract representation of an address.
fileprivate enum AbstractAddress: Hashable {

  /// A base address.
  case base(Operand)

  /// The abstract address of a part from a value.
  indirect case part(parent: AbstractAddress, property: String)

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
    case .part(let parent, _):
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

/// An abstract object.
///
/// This type represent objects in register or memory. More formally, an abtract object is a triple
/// `(s, ls, ps)` where `s` is the ownership state of the object, `ls` lists its loans and and `ps`
/// maps property identifiers to abstract objects. These elements are used by the analysis to track
/// the state of the object and its parts individually.
///
/// Borrowing information is propagated through all parts so that querying the state of a given
/// part returns a state that implicitly encode the state of the container object. For instance,
/// given an instance of a type `Pair` with two parts `fst` and `snd`, borrowing the pair
/// automatically borrows `fst` and `snd`.
///
/// The "derived" state of the object is defined as the most conservative state covering `s` and
/// `ps(x)` for all properties `x`. An abstract object is "well-formed" all pairs `(s, drv(a))` for
/// all properties `x` belong to a relation `R` denoting well-formedness invariants. These
/// invariants are implemented in the method `verify()`.
fileprivate struct AbstractObject: Equatable {

  /// The ownership state of the object itself, without considering the state of its parts.
  var state: OwnershipState

  /// The loans contracted by this object on other objects.
  var loans: Set<Loan>

  /// The parts of the object.
  var parts: [String: AbstractObject]

  /// Creates a new abstract object.
  init(state: OwnershipState, loans: Set<Loan> = [], parts: [String: AbstractObject] = [:]) {
    self.state = state
    self.loans = loans
    self.parts = parts
  }

  /// The canonical form of that object.
  var canonical: AbstractObject {
    return AbstractObject(
      state: state,
      loans: loans,
      parts: parts.filter({ (_, part) in
        !part.loans.isEmpty || (part.derivedState != state)
      }))
  }

  /// The derived state of the object.
  ///
  /// This property denotes the most conservative assumption we can make about that object, given
  /// its own ownership state and that of its parts.
  var derivedState: OwnershipState {
    return parts.values.reduce(state, { (current, part) in
      switch current {
      case .owned:
        return part.derivedState

      case .lent(let a):
        if case .lent(let b) = part.derivedState {
          return .lent(borrowers: a.union(b))
        } else {
          return current
        }

      case .projected, .consumed, .uninitialized, .dynamic:
        return current
      }
    })
  }

  /// The derived loans of the object.
  var derivedLoans: Set<Loan> {
    return parts.values.reduce(into: loans, { $0.formUnion($1.loans) })
  }

  mutating func consume(with consumer: InstIndex) throws {
    switch derivedState {
    case .owned:
      state = .consumed(consumer: consumer)
      parts = [:]

    case .lent:
      throw OwnershipError.moveOfProjectedValue
    case .projected:
      throw OwnershipError.moveOfInoutedValue
    case .consumed, .dynamic:
      throw OwnershipError.useOfConsumedValue
    case .uninitialized:
      throw OwnershipError.useOfUninitializedValue
    }
  }

  mutating func lend(mutably: Bool, to borrower: InstIndex) throws {
    var this = self

    switch this.derivedState {
    case .owned:
      this.state = mutably
        ? .projected(borrower: borrower)
        : .lent(borrowers: [borrower])

      // Propagate the loan.
      for k in this.parts.keys {
        try this.parts[k]!.lend(mutably: mutably, to: borrower)
      }

    case .lent:
      if mutably { throw OwnershipError.overlappingMutableAccesses }

      // Add the new borrower to the current set of borrowers.
      if case .lent(let borrowers) = this.state {
        this.state = .lent(borrowers: borrowers.union([borrower]))
      } else {
        assert(this.state == .owned)
        this.state = .lent(borrowers: [borrower])
      }

      // Propagate the loan.
      for k in this.parts.keys {
        try this.parts[k]!.lend(mutably: false, to: borrower)
      }

    case .projected:
      throw OwnershipError.overlappingMutableAccesses
    case .consumed, .dynamic:
      throw OwnershipError.useOfConsumedValue
    case .uninitialized:
      throw OwnershipError.useOfUninitializedValue
    }

    swap(&self, &this)
  }

  mutating func endLoan(to borrower: InstIndex) {
    switch state {
    case .lent(var borrowers):
      borrowers.remove(borrower)
      state = borrowers.isEmpty
        ? .owned
        : .lent(borrowers: borrowers)

    case .projected(borrower):
      state = .owned

    default:
      break
    }

    for k in parts.keys {
      parts[k]!.endLoan(to: borrower)
    }
  }

  /// Verifies the invariants on the state of the object's parts w.r.t. to its own state.
  func verify() -> Bool {
    for part in parts.values {
      let rhs = part.derivedState
      switch (state, rhs) {
      case (.owned, .owned), (.owned, .lent), (.owned, .projected):
        continue
      case (.owned, _):
        preconditionFailure("owned parent must have owned, lent, or projected parts")

      case (.lent, .lent):
        continue
      case (.lent, _):
        preconditionFailure("lent parent must have lent parts")

      case (.projected(let a), .projected(let b)):
        precondition(a == b, "parts of projected parent must have the same borrower")
      case (.projected, _):
        preconditionFailure("projected parent must have projected parts")

      case (.consumed, _):
        preconditionFailure("consumed parent must have no parts")

      case (.uninitialized, _):
        preconditionFailure("uninitialized parent must have no parts")

      case (.dynamic, _):
        preconditionFailure("dynamic parent must have no parts")
      }
    }

    return true
  }

  static var owned: AbstractObject { AbstractObject(state: .owned) }

  static var uninitialized: AbstractObject { AbstractObject(state: .uninitialized) }

  static func && (_ lhs: AbstractObject, _ rhs: AbstractObject) throws -> AbstractObject {
    return try AbstractObject(
      state: lhs.derivedState && rhs.derivedState,
      loans: lhs.loans.union(rhs.loans),
      parts: lhs.parts.merging(rhs.parts, uniquingKeysWith: &&))
  }

}

/// A loan contracted by an object.
fileprivate struct Loan: Hashable {

  /// The address of the loan.
  let address: AbstractAddress

  /// The instruction that created the loan.
  let borrower: InstIndex

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

  /// The value is owned, but it is being lent immutably.
  case lent(borrowers: Set<InstIndex>)

  /// The value is owned, but is being projected (i.e., lent mutably).
  case projected(borrower: InstIndex)

  /// The value has been consumed and is currently uninitialized.
  case consumed(consumer: InstIndex)

  /// The value is uninitialzed.
  ///
  /// Uninitialized values are implicitly "owned" by the context that knows about them, since they
  /// can't be borrowed, passed as argument, or returned.
  case uninitialized

  /// The value is owned or consumed depending on a condition evaluated at runtime.
  case dynamic(consumer: InstIndex)

  /// Returns the "meet" (i.e., greatest lower bound) of two ownership states.
  static func && (lhs: OwnershipState, rhs: OwnershipState) throws -> OwnershipState {
    switch (lhs, rhs) {
    case (.owned, .owned):
      return .owned
    case (.owned, .lent):
      return rhs
    case (.owned, .projected):
      return rhs
    case (.owned, .consumed(let consumer)):
      return .dynamic(consumer: consumer)
    case (.owned, .uninitialized):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.owned, .dynamic):
      return rhs

    case (.lent, .owned):
      return lhs
    case (.lent(let a), .lent(let b)):
      return .lent(borrowers: a.union(b))
    case (.lent(let a), .projected(let b)):
      return .lent(borrowers: a.union([b]))
    case (.lent, _):
      throw MergeError(lhs: lhs, rhs: rhs)

    case (.projected, .owned):
      return lhs
    case (.projected(let a), .lent(let b)):
      return .lent(borrowers: b.union([a]))
    case (.projected(let a), .projected(let b)):
      return a == b ? lhs : .lent(borrowers: [a, b])
    case (.projected, _):
      throw MergeError(lhs: lhs, rhs: rhs)

    case (.consumed, .consumed):
      return lhs
    case (.consumed, .uninitialized):
      return lhs
    case (.consumed, .dynamic):
      return rhs

    case (.uninitialized, .uninitialized):
      return .uninitialized
    case (.uninitialized, .dynamic):
      return rhs

    case (.dynamic, .dynamic):
      return lhs

    default:
      return try rhs && lhs
    }
  }

}

/// The ownership analysis pass.
public struct OwnershipAnalysis {

  private var module: Module

  public init(context: Context) {
    self.module = Module(id: "_", context: context)
  }

  public mutating func run(on funName: String, in module: inout Module) -> Bool {
    // Borrow the module argument.
    swap(&self.module, &module)
    defer { swap(&self.module, &module) }

    let fun = self.module.functions[funName] ?< fatalError("function does not exist")
    guard !fun.stage.contains(.didPassOwnership) else { return true }
    guard let entry = fun.entry else { return true }

    // Build the function's dominator tree to establish the initial visiting order.
    let tree = DominatorTree(of: fun)
    var work = Deque(tree.breadthFirstBlocks)
    var done: Set<BasicBlockIndex> = []
    assert(work.first == entry)
    assert(fun.cfg.edges(from: entry).allSatisfy({ $0.label == .forward }))

    // Go through the work list, (re)visiting each basic block until we reach a fixed point.
    var store: [BasicBlockIndex: (pre: AbstractContext, post: AbstractContext)] = [:]
    var success = true
    while let block = work.popFirst() {
      // Make sure we visited all of the block's predecessors, or pick another one.
      let predecessors = fun.cfg.edges(from: block).filter({ $0.label != .forward })
      guard predecessors.allSatisfy({ store[$0.target] != nil }) else {
        work.append(block)
        continue
      }

      // Compute the pre-context of the block.
      var pre = AbstractContext()
      if block == entry {
        for (actual, formal) in zip(self.module.blocks[entry].params, fun.type.params!) {
          switch formal.policy! {
          case .local, .inout:
            let a: AbstractAddress = .base(Operand(actual))
            pre[in: Operand(actual)] = .address(a)
            pre[at: a] = .owned

          case .consuming:
            pre[in: Operand(actual)] = .object(.owned)
          }
        }
      } else {
        for pred in predecessors {
          do {
            try pre.merge(store[pred.target]!.post)
          } catch {
            report(error: error, range: nil)
            success = false
          }
        }
      }

      // If the pre-context didn't change, then we don't need to re-compute the post-context and
      // we're done with the current block.
      if store[block]?.pre == pre {
        done.insert(block)
        continue
      }

      var post = pre
      success = process(block: block, context: &post) && success

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

    if success { self.module.functions[fun.name]!.stage.insert(.didPassOwnership) }
    return success
  }

  private mutating func process(
    block: BasicBlockIndex,
    context: inout AbstractContext
  ) -> Bool {
    var success = true
    for index in module.blocks[block].instructions {
      switch module.instructions[index] {
      case let inst as AllocStackInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as ApplyInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as AsyncInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as AwaitInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as BorrowAddrInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as BorrowExistAddrInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as BranchInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as CheckedCastAddrInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as CondBranchInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as DeallocStackInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as DeleteAddrInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as EndBorrowInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as InitExistInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as IsCastableAddrInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as LoadInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as OpenExistAddrInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as PartialApplyInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as RecordInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as RecordMemberAddrInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as RetInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as StoreInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as ThinToThickInst:
        success = visit(inst: inst, index: index, context: &context) && success

      case let inst:
        fatalError("unsupported instruction '\(inst)'")
      }
    }

    return success
  }

  private func visit(
    inst: AllocStackInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    // Create an abstract address denoting the newly allocated memory. The address is tagged by the
    // `alloc_stack` instruction to detect unbounded stack allocation in loops.
    let address: AbstractAddress = .base(Operand(index))
    assert(context[at: address] == nil, "bad VIL: stack allocation in loop")

    // Add the new allocation to the context.
    context[in: Operand(index)] = .address(address)
    context[at: address] = .uninitialized
    return true
  }

  private func visit(
    inst: ApplyInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    // The result of a function call is always owned.
    context[in: Operand(index)] = .object(.owned)

    // Verify and update the ownership state of each parameter.
    var success = true
    let params = module.type(of: inst.callee).params!
    for i in 0 ..< inst.args.count {
      switch params[i].policy {
      case .local, .inout:
        // Local and mutating parameters are borrowed.
        let inst = module.instructions[inst.args[i].inst!]
        assert((inst is BorrowAddrInst) || (inst is BorrowExistAddrInst))

      case .consuming:
        // Consuming parameters are consumed.
        do {
          _ = try context.consume(inst.args[i], with: index)
        } catch {
          report(error: error, range: inst.argsRanges[i])
          success = false
        }

      case nil:
        // The parameter passing is unknown. That can only happen if the type of the function we
        // are analyzing is ill-formed.
        fatalError("ill-formed function type")
      }
    }

    return success
  }

  private func visit(
    inst: AsyncInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    // The result of an async instruction is always owned.
    var result = AbstractObject.owned

    // Verify and update the ownership state of each capture.
    var success = true
    let params = inst.ref.type.params!
    for i in 0 ..< inst.captures.count {
      switch params[i].policy {
      case .local, .inout:
        // Local and mutating captures are borrowed. Assuming the operand results from a borrowing
        // instruction, transfer the loan to the async instruction.
        let capture = inst.captures[i]
        guard let address = context[in: capture]?.asAddress else { illegalOperand() }
        context.endLoan(at: address, to: capture.inst!)
        do {
          try context.lend(address, mutably: params[i].policy == .inout, to: index)
          result.loans.insert(Loan(address: address, borrower: index))
        } catch {
          report(error: error, range: inst.captureRanges[i])
          success = false
        }

      case .consuming:
        // Consuming captures are consumed.
        do {
          _ = try context.consume(inst.captures[i], with: index)
        } catch {
          report(error: error, range: inst.captureRanges[i])
          success = false
        }

      case nil:
        // The parameter passing is unknown. That can only happen if the type of the function we
        // are analyzing is ill-formed.
        fatalError("ill-formed function type")
      }
    }

    context[in: Operand(index)] = .object(result)
    return success
  }

  private func visit(
    inst: AwaitInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    // The result of an await instruction is always owned.
    context[in: Operand(index)] = .object(.owned)

    // End the loans of the awaited operand.
    if var object = context[in: inst.value]?.asObject {
      for loan in object.loans {
        context.endLoan(at: loan.address, to: loan.borrower)
      }
      object.loans.removeAll()
      context[in: inst.value] = .object(object)
    }

    return true
  }

  private mutating func visit(
    inst: BorrowAddrInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    // Copy the address of the borrowed object.
    guard let source = context[in: inst.source]?.asAddress else { illegalOperand() }
    context[in: Operand(index)] = .address(source)

    // Borrow the source's object.
    do {
      try context.lend(source, mutably: inst.isMutable, to: index)
      return true
    } catch {
      report(error: error, range: inst.range)
      return false
    }
  }

  private mutating func visit(
    inst: BorrowExistAddrInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    // Copy the address of the borrowed object.
    guard let source = context[in: inst.source]?.asAddress else { illegalOperand() }
    context[in: Operand(index)] = .address(source)

    // Borrow the source's object.
    do {
      try context.lend(source, mutably: false, to: index)
      return true
    } catch {
      report(error: error, range: inst.range)
      return false
    }
  }

  private mutating func visit(
    inst: BranchInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    return true
  }

  private mutating func visit(
    inst: CheckedCastAddrInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    guard let source = context[in: inst.source]?.asAddress else { illegalOperand() }
    context[in: Operand(index)] = .address(source)
    return true
  }

  private mutating func visit(
    inst: CondBranchInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    var success = true

    do {
      _ = try context.consume(inst.cond, with: index)
    } catch {
      report(error: error, range: inst.range)
      success = false
    }

    // Consume object arguments.
    func consume(_ operands: [Operand]) {
      for operand in operands where module.type(of: operand).isObject {
        do {
          _ = try context.consume(operand, with: index)
        } catch {
          report(error: error, range: inst.range)
          success = false
        }
      }
    }
    consume(inst.succArgs)
    consume(inst.failArgs)

    return success
  }

  private mutating func visit(
    inst: DeallocStackInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    guard let address = context[in: inst.alloc]?.asAddress else { illegalOperand() }
    assert(
      address.isBase && module.instructions[address.provenance.inst!] is AllocStackInst,
      "bad VIL: operand of dealloc_stack is not alloc_stack")

    // Make sure the deallocated is uninitialized, inserting delete_addr if necessary.
    if ensureDeinit(target: inst.alloc, before: index, context: &context) {
      context[at: address] = nil
      return true
    } else {
      return false
    }
  }

  private func visit(
    inst: DeleteAddrInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    guard let target = context[in: inst.target]?.asAddress else { illegalOperand() }

    // End the loans contracted by the object at the specified address.
    if let object = context[at: target] {
      for loan in object.loans {
        context.endLoan(at: loan.address, to: loan.borrower)
      }
    }

    // Consume the object at the specified address.
    do {
      _ = try context.consume(target, with: index)
      return true
    } catch {
      // Assume the error has been reported already.
      context[at: target] = AbstractObject(state: .consumed(consumer: index))
      return false
    }
  }

  private func visit(
    inst: EndBorrowInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    guard let source = context[in: inst.source]?.asAddress else { illegalOperand() }
    if let borrower = inst.source.inst {
      context.endLoan(at: source, to: borrower)
    }
    return true
  }

  private mutating func visit(
    inst: InitExistInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    return visitInit(object: inst.object, target: inst.container, index: index, context: &context)
  }

  private mutating func visit(
    inst: IsCastableAddrInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    context[in: Operand(index)] = .object(.owned)

    guard let source = context[in: inst.source]?.asAddress else { illegalOperand() }
    switch context[at: source]!.state {
    case .uninitialized:
      report(error: OwnershipError.useOfUninitializedValue, range: inst.range)
      return false

    case .consumed:
      report(error: OwnershipError.useOfConsumedValue, range: inst.range)
      return false

    default:
      return true
    }
  }

  private func visit(
    inst: LoadInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    guard let source = context[in: inst.source]?.asAddress else { illegalOperand() }
    do {
      context[in: Operand(index)] = .object(try context.consume(source, with: index))
      return true
    } catch {
      context[in: Operand(index)] = .object(.owned)
      report(error: error, range: inst.range)
      return false
    }
  }

  private func visit(
    inst: OpenExistAddrInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    guard let source = context[in: inst.container]?.asAddress else { illegalOperand() }
    context[in: Operand(index)] = .address(source)
    return true
  }

  private func visit(
    inst: PartialApplyInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    // The result of a partial_apply instruction is always owned.
    var result = AbstractObject.owned

    // Verify and update the ownership state of each capture.
    var success = true
    let params = inst.delegator.type.params!
    for i in 0 ..< inst.args.count {
      switch params[i].policy {
      case .local, .inout:
        // Local and mutating captures are borrowed; we must transfer the loan to the instruction.
        let capture = inst.args[i]
        guard let address = context[in: capture]?.asAddress else { illegalOperand() }
        if let borrower = capture.inst {
          context.endLoan(at: address, to: borrower)
        }
        do {
          try context.lend(address, mutably: params[i].policy == .inout, to: index)
          result.loans.insert(Loan(address: address, borrower: index))
        } catch {
          report(error: error, range: inst.argsRanges[i])
          success = false
        }

      case .consuming:
        // Consuming captures are consumed.
        do {
          _ = try context.consume(inst.args[i], with: index)
        } catch {
          report(error: error, range: inst.argsRanges[i])
          success = false
        }

      case nil:
        // The parameter passing is unknown. That can only happen if the type of the function we
        // are analyzing is ill-formed.
        fatalError("ill-formed function type")
      }
    }

    context[in: Operand(index)] = .object(result)
    return success
  }

  private func visit(
    inst: RecordInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    var success = true
    for i in 0 ..< inst.operands.count {
      do {
        _ = try context.consume(inst.operands[i], with: index)
      } catch {
        report(error: error, range: inst.range)
        success = false
      }
    }

    context[in: Operand(index)] = .object(.owned)
    return success
  }

  private func visit(
    inst: RecordMemberAddrInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    guard let parent = context[in: inst.record]?.asAddress else { illegalOperand() }
    let part = AbstractAddress.part(parent: parent, property: inst.memberDecl.name)
    context[in: Operand(index)] = .address(part)
    return true
  }

  private mutating func visit(
    inst: RetInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    do {
      // Returned values are consumed.
      _ = try context.consume(inst.value, with: index)
      return true
    } catch {
      report(error: error, range: inst.range)
      return false
    }
  }

  private mutating func visit(
    inst: StoreInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    return visitInit(object: inst.value, target: inst.target, index: index, context: &context)
  }

  private func visit(
    inst: ThinToThickInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    context[in: Operand(index)] = .object(.owned)
    return true
  }

  private mutating func visitInit(
    object: Operand,
    target: Operand,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    let success = ensureDeinit(target: target, before: index, context: &context)

    // Transfer the object from register to memory.
    guard let address = context[in: target]?.asAddress else { illegalOperand() }
    do {
      context[at: address] = try context.consume(object, with: index)
      return success
    } catch {
      context[at: address] = .owned
      report(error: error, range: module.instructions[index].range)
      return false
    }
  }

  // Ensures that the memory at `target` is uninitialized before the instruction at `index`,
  // inserting `delete_addr` if necessary.
  private mutating func ensureDeinit(
    target: Operand,
    before index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    guard let address = context[in: target]?.asAddress else { illegalOperand() }
    switch context[at: address]!.derivedState {
    case .owned:
      // The memory at the target address holds an owned object; delete it first.
      let i = module.insertDeleteAddr(target: target, at: .before(inst: index))
      return visit(
        inst: module.instructions[i] as! DeleteAddrInst, index: i, context: &context)

    case .lent:
      report(
        error: OwnershipError.writeAccessToProjectedValue,
        range: module.instructions[index].range)
      return false

    case .projected:
      report(
        error: OwnershipError.writeAccessToInoutedValue,
        range: module.instructions[index].range)
      return false

    case .consumed, .uninitialized:
      // The memory at the target address is uninitialized; we're good.
      return true

    case .dynamic:
      fatalError("not implemented")
    }
  }

  private func report(error: Error, range: SourceRange?) {
    if let error = error as? OwnershipError {
      module.context.report(error.diag(anchor: range))
    } else {
      module.context.report(Diag(error.localizedDescription, anchor: range))
    }
  }

}

fileprivate func illegalOperand(
  _ message: @autoclosure () -> String = "illegal operand"
) -> Never {
  fatalError("bad VIL: \(message())")
}

fileprivate func illegalOwnershipState() -> Never {
  fatalError("bad VIL: illegal ownership state")
}
