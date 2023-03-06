import Core
import DequeModule
import Utils

extension Module {

  /// Ensures the Law of Exclusivity is satisfied in `f`, reporting errors and warnings to
  /// `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public func ensureExclusivity(in f: Function.ID, diagnostics: inout DiagnosticSet) {

    /// The control flow graph of the function to analyze.
    let cfg = self[f].cfg()

    /// The dominator tree of the function to analyze.
    let dominatorTree = DominatorTree(function: f, cfg: cfg, in: self)

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
        let x = entryContext(of: f)
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
        case is DeallocStackInstruction:
          interpret(deallocStack: user, in: &newContext)
        case is ElementAddrInstruction:
          interpret(elementAddr: user, in: &newContext)
        case is EndBorrowInstruction:
          interpret(endBorrow: user, in: &newContext)
        default:
          continue
        }
      }

      return newContext
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(allocStack i: InstructionID, in context: inout Context) {
      let l = AbstractLocation.instruction(block: i.block, address: i.address)
      precondition(context.memory[l] == nil, "stack leak")

      context.memory[l] = .init(
        layout: AbstractTypeLayout(
          of: (self[i] as! AllocStackInstruction).allocatedType, definedIn: program),
        value: .full(.unique))
      context.locals[FunctionLocal(i, 0)] = .locations([l])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(borrow i: InstructionID, in context: inout Context) {
      let borrow = self[i] as! BorrowInstruction
      guard let k = FunctionLocal(operand: borrow.location) else {
        // Operand is a constant.
        fatalError("not implemented")
      }

      // Skip the instruction if an error occured upstream.
      guard context.locals[k] != nil else { return }

      let former = reborrowedSource(borrow)
      var hasConflict = false
      context.forEachObject(at: k) { (o) in
        // We can always create new borrows if there aren't any.
        // TODO: immutable sources
        let borrowers = o.value.borrowers
        if borrowers.isEmpty {
          o.value.insertBorrower(i)
          return
        }

        // Otherwise, we can form a new borrow if and only if:
        // * we're reborrowing from a unique mutable borrower; or
        // * we're borrowing a `let` and there's at least one immutable borrowers.
        switch borrow.capability {
        case .let:
          let isImmutable = borrowers.contains(where: isImmutableBorrow(_:))
          if isImmutable || former.map(borrowers.containsOnly(_:)) ?? false {
            o.value.insertBorrower(i)
          } else {
            diagnostics.insert(.error(illegalImmutableAccessAt: borrow.site))
            hasConflict = true
          }

        case .inout, .set:
          if former.map({ borrowers.containsOnly($0) && isMutableBorrow($0) }) ?? false {
            o.value.removeBorrower(former!)
            o.value.insertBorrower(i)
          } else {
            diagnostics.insert(.error(illegalMutableAccessAt: borrow.site))
            hasConflict = true
          }

        case .sink, .yielded:
          unreachable()
        }
      }

      // Don't set the locals if an error occured to avoid cascading errors downstream.
      if !hasConflict {
        context.locals[FunctionLocal(i, 0)] = context.locals[k]!
      }
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(deallocStack i: InstructionID, in context: inout Context) {
      let x = self[i] as! DeallocStackInstruction
      let k = FunctionLocal(x.location.instruction!, 0)
      let l = context.locals[k]!.unwrapLocations()!.uniqueElement!
      context.memory[l] = nil
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(elementAddr i: InstructionID, in context: inout Context) {
      let elementAddr = self[i] as! ElementAddrInstruction
      guard let k = FunctionLocal(operand: elementAddr.base) else {
        // Operand is a constant.
        fatalError("not implemented")
      }

      // Skip the instruction if an error occured upstream.
      guard let s = context.locals[k] else { return }

      let newLocations = s.unwrapLocations()!.map({ $0.appending(elementAddr.elementPath) })
      context.locals[FunctionLocal(i, 0)] = .locations(Set(newLocations))
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(endBorrow i: InstructionID, in context: inout Context) {
      let end = self[i] as! EndBorrowInstruction

      // Skip the instruction if an error occured upstream.
      let k = FunctionLocal(operand: end.borrow)!
      guard context.locals[k] != nil else { return }

      // Remove the ended borrow from the objects' borrowers, putting the borrowed source back in
      // case the ended borrow was a reborrow.
      let borrowID = end.borrow.instruction!
      let borrow = self[borrowID] as! BorrowInstruction
      let former = reborrowedSource(borrow)
      context.forEachObject(at: k) { (o) in
        if !o.value.removeBorrower(borrowID) { return }
        if let s = former {
          switch borrow.capability {
          case .let:
            assert(o.value.borrowers.contains(s))
          case .set, .inout:
            o.value.insertBorrower(s)
          case .sink, .yielded:
            unreachable()
          }
        }
      }
    }

  }

  private func entryContext(of f: Function.ID) -> Context {
    let function = self[f]
    var result = Context()

    for i in function.inputs.indices {
      let (parameterConvention, parameterType) = function.inputs[i]
      let parameterKey = FunctionLocal.parameter(block: function.entry!, index: i)
      let parameterLayout = AbstractTypeLayout(of: parameterType.astType, definedIn: program)

      switch parameterConvention {
      case .let, .inout, .set:
        let l = AbstractLocation.argument(index: i)
        result.locals[parameterKey] = .locations([l])
        result.memory[l] = .init(layout: parameterLayout, value: .full(.unique))

      case .sink:
        result.locals[parameterKey] = .object(
          .init(layout: parameterLayout, value: .full(.unique)))

      case .yielded:
        preconditionFailure("cannot represent instance of yielded type")
      }
    }

    return result
  }

  /// Returns `true` iff `i` is a `borrow` instruction taking the `inout` or `set` capability.
  private func isMutableBorrow(_ i: InstructionID) -> Bool {
    if let borrow = self[i] as? BorrowInstruction {
      return borrow.capability == .inout || borrow.capability == .set
    } else {
      return false
    }
  }

  /// Returns the borrowed instruction from which `b` reborrows, if any.
  private func reborrowedSource(_ b: BorrowInstruction) -> InstructionID? {
    if let s = accessSource(b.location).instruction, self[s] is BorrowInstruction {
      return s
    } else {
      return nil
    }
  }

  /// Returns the source of the access denoted by `o`.
  ///
  /// - Requires: `o` denotes a location.
  private func accessSource(_ o: Operand) -> Operand {
    if case .result(let i, _) = o, let a = self[i] as? ElementAddrInstruction {
      return accessSource(a.base)
    } else {
      return o
    }
  }

}

/// An abstract interpretation context.
private typealias Context = AbstractContext<State>

/// A map fron function block to the context of the abstract interpreter before and after the
/// evaluation of its instructions.
private typealias Contexts = [Function.Blocks.Address: (before: Context, after: Context)]

/// The ownership state of an object or sub-object.
///
/// Instances form a lattice whose supremum is `.unique` and infimum is `.shared(by: s)`
/// where `s` is the set of all instructions. The meet of two elements denotes the conservative
/// superposition of two initialization states.
private enum State: AbstractDomain {

  /// Object is unique.
  case unique

  /// Object is shared.
  ///
  /// - Requires: The payload is not empty.
  case shared(by: Set<InstructionID>)

  /// Forms a new state by merging `lhs` with `rhs`.
  static func && (lhs: State, rhs: State) -> State {
    switch lhs {
    case .unique:
      return rhs

    case .shared(let a):
      if case .shared(let b) = rhs {
        return .shared(by: a.union(b))
      } else {
        return .shared(by: a)
      }
    }
  }
  
}

extension State: CustomStringConvertible {

  var description: String {
    switch self {
    case .unique:
      return "\u{23Fa}"
    case .shared(let borrowers):
      return "←\(borrowers)"
    }
  }

}

extension AbstractObject.Value where Domain == State {

  /// The set of instructions borrowing the object or a part thereof.
  fileprivate var borrowers: Set<InstructionID> {
    switch self {
    case .full(.unique):
      return []
    case .full(.shared(let b)):
      return b
    case .partial(let parts):
      return parts.reduce(into: [], { $0.formUnion($1.borrowers) })
    }
  }

  /// Inserts `b` to the object's borrowers, returning `true` iff `b` wasn't already included.
  @discardableResult
  fileprivate mutating func insertBorrower(_ b: InstructionID) -> Bool {
    switch self {
    case .full(.unique):
      self = .full(.shared(by: [b]))
      return true

    case .full(.shared(var borrowers)):
      let (inserted, _) = borrowers.insert(b)
      self = .full(.shared(by: borrowers))
      return inserted

    case .partial(var parts):
      var inserted = 0
      for i in parts.indices {
        if parts[i].insertBorrower(b) { inserted += 1 }
      }
      self = .partial(parts)
      if inserted != parts.count {
        self = self.canonical
      }
      return inserted > 0
    }
  }

  /// Removes `b` from the object's borrowers, returning `true` iff `b` was present.
  @discardableResult
  fileprivate mutating func removeBorrower(_ b: InstructionID) -> Bool {
    switch self {
    case .full(.unique):
      return false

    case .full(.shared(var borrowers)):
      let removed = borrowers.remove(b) != nil
      self = borrowers.isEmpty ? .full(.unique) : .full(.shared(by: borrowers))
      return removed

    case .partial(var parts):
      var removed = false
      for i in parts.indices {
        removed = parts[i].removeBorrower(b) || removed
      }
      self = .partial(parts).canonical
      return removed
    }
  }

}

extension Set {

  /// Returns `true` iff `self` contains `uniqueElement` and no other element.
  fileprivate func containsOnly(_ uniqueElement: Element) -> Bool {
    (count == 1) && contains(uniqueElement)
  }

}

extension Diagnostic {

  fileprivate static func error(illegalImmutableAccessAt site: SourceRange) -> Diagnostic {
    .error("illegal immutable access", at: site)
  }

  fileprivate static func error(illegalMutableAccessAt site: SourceRange) -> Diagnostic {
    .error("illegal mutable access", at: site)
  }

}
