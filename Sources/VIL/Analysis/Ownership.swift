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

//  /// The root of an existential package, in an existential container.
//  indirect case existential(parent: AbstractAddress)

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
//    case .existential(let parent):
//      return parent.base
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

  /// The loans of the object.
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

/// A loan transferred to an object.
fileprivate struct Loan: Hashable {

  let address: AbstractAddress

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
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.owned, .projected):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.owned, .consumed(let consumer)):
      return .dynamic(consumer: consumer)
    case (.owned, .uninitialized):
      throw MergeError(lhs: lhs, rhs: rhs)
    case (.owned, .dynamic):
      return rhs

    case (.lent(let a), .lent(let b)):
      return .lent(borrowers: a.union(b))
    case (.lent, _):
      throw MergeError(lhs: lhs, rhs: rhs)

    case (.projected(let a), .projected(let b)):
      guard a == b else { throw MergeError(lhs: lhs, rhs: rhs) }
      return lhs
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

  private var module: Module?

  public init() {}

  public mutating func run(_ funName: String, on module: inout Module) -> Bool {
    // Borrow the module argument.
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
    guard !fun.stage.contains(.didPassOwnership) else { return true }
    guard let entry = fun.entry else { return true }

    // Build the function's dominator tree to establish the initial visiting order.
    let tree = DominatorTree(of: fun)
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
      case .local, .inout:
        let a: AbstractAddress = .base(arg)
        pre[in: arg] = .address(a)
        pre[at: a] = .owned

      case .consuming:
        pre[in: arg] = .object(.owned)
      }
    }

    var post = pre
    var success = process(block: entry, context: &post)
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
        pre[in: Operand(arg)] = .object(.owned)
      }

      // If the pre-context didn't change, then we don't need to re-compute the post-context and
      // we're done with the current block.
      if store[block]?.pre == pre {
        done.insert(block)
        continue
      }

      post = pre
      success = process(block: block, context: &post)

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

    if success { module.functions[fun.name]!.stage.insert(.didPassOwnership) }
    return success
  }

  private mutating func process(
    block: BasicBlockIndex,
    context: inout AbstractContext
  ) -> Bool {
    var success = true
    for index in module!.blocks[block].instructions {
      switch module!.instructions[index] {
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
      case let inst as DeallocStackInst:
        success = visit(inst: inst, index: index, context: &context) && success
//      case let inst as DeleteInst:
//        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as DeleteAddrInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as EndBorrowInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as LoadInst:
        success = visit(inst: inst, index: index, context: &context) && success
//      case let inst as RecordInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
//      case let inst as RecordMemberInst:
//        success = visit(inst: inst, path: path, context: &context, builder: &builder) && success
      case let inst as RecordMemberAddrInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as RetInst:
        success = visit(inst: inst, index: index, context: &context) && success
      case let inst as StoreInst:
        success = visit(inst: inst, index: index, context: &context) && success
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
    let params = module!.type(of: inst.callee).params!
    for i in 0 ..< inst.args.count {
      switch params[i].policy {
      case .local, .inout:
        // Local and mutating parameters are borrowed.
        assert({
          let inst = module!.instructions[inst.args[i].inst!]
          return (inst is BorrowAddrInst) || (inst is BorrowExistAddrInst)
        }())

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
        // instruction, we must transfer the loan to the async instruction.
        let formerBorrower = inst.captures[i]
        guard let address = context[in: formerBorrower]?.asAddress else { illegalOperand() }
        context.endLoan(at: address, to: formerBorrower.inst!)
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

  private func visit(
    inst: BorrowAddrInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    return visitFallibleBorrow(
      borrower: index,
      isMutable: inst.isMutable,
      source: inst.source,
      range: inst.range,
      context: &context)
  }

  private func visit(
    inst: BorrowExistAddrInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    return visitFallibleBorrow(
      borrower: index,
      isMutable: inst.isMutable,
      source: inst.container,
      range: inst.range,
      context: &context)
  }

  private func visitFallibleBorrow(
    borrower: InstIndex,
    isMutable: Bool,
    source: Operand,
    range: SourceRange?,
    context: inout AbstractContext
  ) -> Bool {
    // Operationally, a borrow is just a copy of the source address.
    guard let source = context[in: source]?.asAddress else { illegalOperand() }
    context[in: Operand(borrower)] = .address(source)

    do {
      // Borrow the value at the source address.
      try context.lend(source, mutably: isMutable, to: borrower)
      return true
    } catch {
      report(error: error, range: range)
      return false
    }
  }

  private mutating func visit(
    inst: DeallocStackInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    guard let address = context[in: inst.alloc]?.asAddress else { illegalOperand() }
    assert(
      address.isBase && module!.instructions[address.provenance.inst!] is AllocStackInst,
      "bad VIL: operand of dealloc_stack is not alloc_stack")

    // Make sure the deallocated is uninitialized, inserting delete_addr if necessary.
    switch context[at: address]!.derivedState {
    case .owned:
      // The memory holds an owned value; delete it first.
      let i = module!.insertDeleteAddr(
        target: inst.alloc, at: .after(inst: index, in: inst.parent))
      let success = visit(
        inst: module!.instructions[i] as! DeleteAddrInst, index: i, context: &context)
      context[at: address] = nil
      return success

    case .lent, .projected:
      report(error: OwnershipError.useAfterFree, range: inst.range)
      return false

    case .consumed, .uninitialized:
      // The memory is unitialized; we're good.
      context[at: address] = nil
      return true

    case .dynamic:
      fatalError("not implemented")
    }
  }

//  private func visit(
//    inst: DeleteInst,
//    path: InstPath,
//    context: inout AbstractContext,
//    builder: inout Builder
//  ) -> Bool {
//    return consume(value: inst.value, consumer: path, in: &context)
//  }

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
      context[at: target] = AbstractObject(state: .consumed(consumer: index))
      report(error: error, range: inst.range)
      return false
    }
  }

  private func visit(
    inst: EndBorrowInst,
    index: InstIndex,
    context: inout AbstractContext
  ) -> Bool {
    guard let source = context[in: inst.source]?.asAddress else { illegalOperand() }
    context.endLoan(at: source, to: inst.source.inst!)
    return true
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
    guard let target = context[in: inst.target]?.asAddress else { illegalOperand() }
    var success = true

    // Make sure the target is uninitialized, inserting delete_addr if necessary.
    switch context[at: target]!.derivedState {
    case .owned:
      // The memory holds an owned object; delete it first.
      let i = module!.insertDeleteAddr(
        target: inst.target, at: .after(inst: index, in: inst.parent))
      success = visit(
        inst: module!.instructions[i] as! DeleteAddrInst, index: i, context: &context)

    case .lent:
      report(error: OwnershipError.writeAccessToProjectedValue, range: inst.range)
      success = false

    case .projected:
      report(error: OwnershipError.writeAccessToInoutedValue, range: inst.range)
      success = false

    case .consumed, .uninitialized:
      // The target is uninitialized; we're good.
      break

    case .dynamic:
      fatalError("not implemented")
    }

    // Transfer the object from register to memory.
    do {
      context[at: target] = try context.consume(inst.value, with: index)
      return success
    } catch {
      context[at: target] = .owned
      report(error: error, range: inst.range)
      return false
    }
  }

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

  private func report(error: Error, range: SourceRange?) {
    if let error = error as? OwnershipError {
      module!.context.report(error.diag(anchor: range))
    } else {
      module!.context.report(Diag(error.localizedDescription, anchor: range))
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
