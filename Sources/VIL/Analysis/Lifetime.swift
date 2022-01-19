/// The lifetime analysis pass.
///
/// Lifetime analysis consists of identifying the useful lifetime of objects and borrowed
/// references thereupon.
public struct LifetimeAnalyis {

  public init() {}

  public mutating func run(on funName: String, in module: inout Module) {
    let blocks = module.functions[funName]!.blocks
    for block in blocks {
      for index in module.blocks[block].instructions {
        switch module.instructions[index] {
        case let inst as AllocStackInst:
          endOwnedLifetime(
            of: Operand(index),
            definedInBlock: inst.parent,
            in: &module)

        case is BorrowAddrInst, is BorrowExistAddrInst:
          let inst = module.instructions[index]
          endBorrowedLifetime(
            of: index,
            definedInBlock: inst.parent,
            in: &module)

        default:
          continue
        }
      }
    }
  }

  private mutating func endOwnedLifetime(
    of address: Operand,
    definedInBlock block: BasicBlockIndex,
    in module: inout Module
  ) {
    // Collect and sort all uses of the object's address.
    var work = module.uses[address, default: []]
    var uses: [Use] = []
    var updates: [Use] = []

    while let use = work.popLast() {
      let user = module.instructions[use.user]
      switch user {
      case is DeallocStackInst:
        // Skip deallocation so that it doesn't appear in the live-range of the address.
        continue

      case is StoreInst, is InitExistInst:
        updates.append(use)

      case is CheckedCastAddrInst, is RecordMemberAddrInst, is OpenExistAddrInst:
        // Include uses of derived addresses.
        uses.append(use)
        work.append(contentsOf: module.uses[Operand(use.user), default: []])

      case is BorrowAddrInst, is BorrowExistAddrInst:
        // Include uses of the borrowed address.
        uses.append(use)
        endBorrowedLifetime(
          of: use.user,
          definedInBlock: user.parent,
          // after: lastUsers(of: use.user, in: module),
          in: &module)
        work.append(contentsOf: module.uses[Operand(use.user), default: []])

      default:
        uses.append(use)
      }
    }

    // Compute the complete liveness range of the object's address.
    let range = liveRange(of: address, definedInBlock: block, uses: uses, module: module)

    // Search for the last use of every store.
    var users = Set(uses.map({ $0.user }))
    var covered: Set<InstIndex> = []

    outer:for use in (uses + updates) {
      let user = module.instructions[use.user]
      guard covered.insert(use.user).inserted else { continue outer }
      covered.insert(use.user)

      // Skip consuming uses.
      switch user {
      case is DeleteAddrInst, is AsyncInst, is PartialApplyInst, is LoadInst:
        continue outer
      default:
        break
      }

      // Scan the instructions following the use to find the next update.
      let list = module.blocks[user.parent].instructions
      let startIndex = list.firstIndex(of: use.user)! + 1
      var last = use.user

      for i in startIndex ..< list.count where users.contains(list[i]) {
        // Go the the next use if we already covered that range.
        guard covered.insert(list[i]).inserted else { continue outer }

        switch module.instructions[list[i]] {
        case is StoreInst, is InitExistInst, is DeallocStackInst:
          users.insert(module.insertDeleteAddr(target: address, at: .after(inst: last)))
          continue outer

        case is DeleteAddrInst, is AsyncInst, is PartialApplyInst, is LoadInst:
          continue outer

        default:
          last = list[i]
        }
      }

      // If the address isn't live after this block, then we can end its lifetime.
      if !range[user.parent, default: (false, false)].isLiveOut {
        users.insert(module.insertDeleteAddr(target: address, at: .after(inst: last)))
      }
    }
  }

  /// Ends the lifetime of the specified borrowed operand after each of its last users.
  private mutating func endBorrowedLifetime(
    of borrow: InstIndex,
    definedInBlock block: BasicBlockIndex,
    in module: inout Module
  ) {
    // Collect the uses of the borrow.
    let source = Operand(borrow)
    let uses = module.uses[source, default: []]

    // End the loan right after its definition if it doesn't have any user.
    guard !uses.isEmpty else {
      module.insertEndBorrowAddr(source: source, at: .after(inst: borrow))
      return
    }

    // Compute the complete liveness range of the loan.
    var range = liveRange(of: source, definedInBlock: block, uses: uses, module: module)

    // End the loan after its last users.
    let users = Set(uses.map({ $0.user }))
    func insertEnd(inBlock block: BasicBlockIndex, in module: inout Module) {
      let list = module.blocks[block].instructions
      let lastUserIndex = list.last(where: users.contains(_:))!
      switch module.instructions[lastUserIndex] {
      case is EndBorrowInst:
        // The end of the loan is already defined, so we're done.
        break

      case is AsyncInst, is PartialApplyInst:
        // The loan is captured, so we're done.
        break

      case is CondBranchInst:
        // Loans can't be block arguments.
        fatalError("unreachable")

      default:
        // By default, loans must end after their last users.
        module.insertEndBorrowAddr(source: source, at: .after(inst: lastUserIndex))
      }
    }

    guard !range.isEmpty else {
      insertEnd(inBlock: block, in: &module)
      return
    }

    let fun = module.functions[module.blocks[block].parent]!
    for (current, flags) in range where flags.isLiveIn && !flags.isLiveOut {
      insertEnd(inBlock: current, in: &module)

      // If the loan is not live in all the "siblings" of the current block, insert a last use at
      // the beginning.
      for pred in fun.cfg.predecessors(of: current) {
        for succ in fun.cfg.successors(of: pred) where
          !range[succ, default: (false, false)].isLiveIn
        {
          module.insertEndBorrowAddr(source: source, at: .startOf(succ))
          range[succ, default: (false, false)].isLiveIn = true
        }
      }
    }
  }

}
