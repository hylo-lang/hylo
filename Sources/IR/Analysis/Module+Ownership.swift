import Core
import DequeModule
import Utils

extension Module {

  /// Ensures the Law of Exclusivity is satisfied in `f`, reporting errors and warnings to
  /// `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public func ensureExclusivity(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    var machine = AbstractInterpreter(analyzing: f, in: self, entryContext: entryContext(of: f))

    // Verify that the borrow instructions in `b` satisfy the Law of Exclusivity given `context`,
    // reporting violations of exclusivity in `diagnostics`.
    machine.fixedPoint { (b, _, context) in
      let blockInstructions = self[f][b].instructions
      for i in blockInstructions.indices {
        let user = InstructionID(f, b, i.address)

        switch blockInstructions[i] {
        case is AllocStackInstruction:
          interpret(allocStack: user, in: &context)
        case is BorrowInstruction:
          interpret(borrow: user, in: &context)
        case is CloseSumInstruction:
          interpret(closeSum: user, in: &context)
        case is DeallocStackInstruction:
          interpret(deallocStack: user, in: &context)
        case is EndBorrowInstruction:
          interpret(endBorrow: user, in: &context)
        case is EndProjectInstruction:
          interpret(endProject: user, in: &context)
        case is GlobalAddrInstruction:
          interpret(globalAddr: user, in: &context)
        case is OpenSumInstruction:
          interpret(openSum: user, in: &context)
        case is PointerToAddressInstruction:
          interpret(pointerToAddress: user, in: &context)
        case is ProjectInstruction:
          interpret(project: user, in: &context)
        case is SubfieldViewInstruction:
          interpret(subfieldView: user, in: &context)
        case is WrapExistentialAddrInstruction:
          interpret(wrapExistentialAddr: user, in: &context)
        default:
          continue
        }
      }
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(allocStack i: InstructionID, in context: inout Context) {
      let s = self[i] as! AllocStackInstruction
      let l = AbstractLocation.root(.register(i, 0))
      precondition(context.memory[l] == nil, "stack leak")

      context.memory[l] = .init(
        layout: AbstractTypeLayout(of: s.allocatedType, definedIn: program),
        value: .full(.unique))
      context.locals[.register(i, 0)] = .locations([l])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(borrow i: InstructionID, in context: inout Context) {
      let borrow = self[i] as! BorrowInstruction
      if case .constant = borrow.location { unreachable("borrowed source is a constant") }

      // Skip the instruction if an error occured upstream.
      guard context.locals[borrow.location] != nil else {
        assert(diagnostics.containsError)
        return
      }

      let former = reborrowedSource(borrow)
      var hasConflict = false
      context.forEachObject(at: borrow.location) { (o) in
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
        context.locals[.register(i, 0)] = context.locals[borrow.location]!
      }
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(closeSum i: InstructionID, in context: inout Context) {
      let s = self[i] as! CloseSumInstruction
      let payload = context.locals[s.start]!.unwrapLocations()!.uniqueElement!

      // The state of the projected payload can't be partial.
      let o = context.withObject(at: payload, { $0 })
      guard case .full(let payloadInitializationState) = o.value else {
        fatalError()
      }

      // Copy the state of the payload to set the state of the container.
      let start = self[s.start.instruction!] as! OpenSumInstruction
      context.forEachObject(at: start.container) { (o) in
        o.value = .full(payloadInitializationState)
      }

      context.memory[payload] = nil
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(deallocStack i: InstructionID, in context: inout Context) {
      let dealloc = self[i] as! DeallocStackInstruction
      let l = context.locals[dealloc.location]!.unwrapLocations()!.uniqueElement!
      context.memory[l] = nil
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(endBorrow i: InstructionID, in context: inout Context) {
      let end = self[i] as! EndBorrowInstruction

      // Skip the instruction if an error occured upstream.
      guard context.locals[end.borrow] != nil else {
        assert(diagnostics.containsError)
        return
      }

      // Remove the ended borrow from the objects' borrowers, putting the borrowed source back in
      // case the ended borrow was a reborrow.
      let borrowID = end.borrow.instruction!
      let borrow = self[borrowID] as! BorrowInstruction
      let former = reborrowedSource(borrow)
      context.forEachObject(at: end.borrow) { (o) in
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

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(endProject i: InstructionID, in context: inout Context) {
      let s = self[i] as! EndProjectInstruction

      // Skip the instruction if an error occured upstream.
      guard context.locals[s.projection] != nil else {
        assert(diagnostics.containsError)
        return
      }
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(globalAddr i: InstructionID, in context: inout Context) {
      let s = self[i] as! GlobalAddrInstruction
      let l = AbstractLocation.root(.register(i, 0))

      context.memory[l] = .init(
        layout: AbstractTypeLayout(of: s.valueType, definedIn: program),
        value: .full(.unique))
      context.locals[.register(i, 0)] = .locations([l])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(openSum i: InstructionID, in context: inout Context) {
      let s = self[i] as! OpenSumInstruction
      let l = AbstractLocation.root(.register(i, 0))
      precondition(context.memory[l] == nil, "overlapping accesses to sum payload")

      // Operand must be a location.
      let locations = context.locals[s.container]!.unwrapLocations()!

      // Objects at each location have the same state unless DI or LoE has been broken.
      let o = context.withObject(at: locations.first!, { $0 })
      let t = AbstractTypeLayout(of: s.payloadType, definedIn: program)

      context.memory[l] = .init(layout: t, value: o.value)
      context.locals[.register(i, 0)] = .locations([l])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(pointerToAddress i: InstructionID, in context: inout Context) {
      let s = self[i] as! PointerToAddressInstruction
      let l = AbstractLocation.root(.register(i, 0))

      context.memory[l] = .init(
        layout: AbstractTypeLayout(of: s.target.bareType, definedIn: program),
        value: .full(.unique))
      context.locals[.register(i, 0)] = .locations([l])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(project i: InstructionID, in context: inout Context) {
      let s = self[i] as! ProjectInstruction
      let l = AbstractLocation.root(.register(i, 0))
      precondition(context.memory[l] == nil, "projection leak")

      context.memory[l] = .init(
        layout: AbstractTypeLayout(of: s.projection.bareType, definedIn: program),
        value: .full(.unique))
      context.locals[.register(i, 0)] = .locations([l])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(subfieldView i: InstructionID, in context: inout Context) {
      let s = self[i] as! SubfieldViewInstruction
      if case .constant = s.recordAddress {
        // Operand is a constant.
        fatalError("not implemented")
      }

      // Skip the instruction if an error occured upstream.
      guard let base = context.locals[s.recordAddress] else {
        assert(diagnostics.containsError)
        return
      }

      let newLocations = base.unwrapLocations()!.map({ $0.appending(s.subfield) })
      context.locals[.register(i, 0)] = .locations(Set(newLocations))
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(wrapExistentialAddr i: InstructionID, in context: inout Context) {
      let s = self[i] as! WrapExistentialAddrInstruction
      if case .constant = s.witness {
        // Operand is a constant.
        fatalError("not implemented")
      }

      context.locals[.register(i, 0)] = context.locals[s.witness]
    }

  }

  /// Returns the initial context in which `f` should be interpreted.
  private func entryContext(of f: Function.ID) -> Context {
    let function = self[f]
    var result = Context()

    let entry = Block.ID(f, function.entry!)
    addParameter(.set, function.output, of: entry, at: function.inputs.count, in: &result)
    for i in function.inputs.indices {
      addParameter(function.inputs[i].type, of: entry, at: i, in: &result)
    }

    return result
  }

  /// Configure in `context` the initial state of the parameter at `position` in `entry`, which
  /// has type `t`.
  private func addParameter(
    _ t: ParameterType, of entry: Block.ID, at position: Int,
    in context: inout Context
  ) {
    addParameter(t.access, t.bareType, of: entry, at: position, in: &context)
  }

  /// Configure in `context` the initial state of the parameter at `position` in `entry`, which
  /// has type `t` passed with capability `k`.
  private func addParameter(
    _ k: AccessEffect, _ t: AnyType, of entry: Block.ID, at position: Int,
    in context: inout Context
  ) {
    let l = AbstractTypeLayout(of: t, definedIn: program)
    let p = Operand.parameter(entry, position)

    switch k {
    case .let, .inout, .set, .sink:
      let a = AbstractLocation.root(p)
      context.locals[p] = .locations([a])
      context.memory[a] = .init(layout: l, value: .full(.unique))

    case .yielded:
      preconditionFailure("cannot represent instance of yielded type")
    }
  }

  /// Returns `true` iff `i` is a `borrow` instruction taking the `inout` or `set` capability.
  private func isMutableBorrow(_ i: InstructionID) -> Bool {
    if let borrow = self[i] as? BorrowInstruction {
      return borrow.capability == .inout || borrow.capability == .set
    } else {
      return false
    }
  }

  /// Returns `true` iff `i` is a `borrow` instruction taking the `let` capability.
  private func isImmutableBorrow(_ i: InstructionID) -> Bool {
    if let borrow = self[i] as? BorrowInstruction {
      return borrow.capability == .let
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
    if case .register(let i, _) = o, let a = self[i] as? SubfieldViewInstruction {
      return accessSource(a.recordAddress)
    } else {
      return o
    }
  }

}

/// An abstract interpretation context.
private typealias Context = AbstractContext<State>

/// The ownership state of an object or sub-object.
///
/// Instances form a lattice whose supremum is `.unique` and infimum is `.shared(by: s)`
/// where `s` is the set of all instructions. The meet of two elements denotes the conservative
/// superposition of two ownership states.
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
