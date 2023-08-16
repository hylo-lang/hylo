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

    // Verify that access instructions in `b` satisfy the Law of Exclusivity given `context`,
    // reporting violations of exclusivity in `diagnostics`.
    machine.fixedPoint { (b, context) in
      let blockInstructions = self[f][b].instructions
      for i in blockInstructions.indices {
        let user = InstructionID(f, b, i.address)

        switch blockInstructions[i] {
        case is Access:
          interpret(access: user, in: &context)
        case is AllocStack:
          interpret(allocStack: user, in: &context)
        case is CloseUnion:
          interpret(closeUnion: user, in: &context)
        case is DeallocStack:
          interpret(deallocStack: user, in: &context)
        case is EndAccess:
          interpret(endBorrow: user, in: &context)
        case is EndProject:
          interpret(endProject: user, in: &context)
        case is GlobalAddr:
          interpret(globalAddr: user, in: &context)
        case is OpenCapture:
          interpret(openCapture: user, in: &context)
        case is OpenUnion:
          interpret(openUnion: user, in: &context)
        case is PointerToAddress:
          interpret(pointerToAddress: user, in: &context)
        case is Project:
          interpret(project: user, in: &context)
        case is SubfieldView:
          interpret(subfieldView: user, in: &context)
        case is WrapExistentialAddr:
          interpret(wrapExistentialAddr: user, in: &context)
        default:
          continue
        }
      }
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(access i: InstructionID, in context: inout Context) {
      let s = self[i] as! Access
      precondition(s.source.constant == nil, "borrowed source is a constant")

      // Access is expected to be reified at this stage.
      let request = s.capabilities.uniqueElement!

      // Skip the instruction if an error occured upstream.
      guard context.locals[s.source] != nil else {
        assert(diagnostics.containsError)
        return
      }

      let former = reborrowedSource(s)
      var hasConflict = false
      context.forEachObject(at: s.source) { (o) in
        // We can always create new borrows if there aren't any.
        // TODO: immutable sources
        let borrowers = o.value.borrowers
        if borrowers.isEmpty {
          o.value.insertBorrower(i)
          return
        }

        // Otherwise, we can form a new access if and only if ...
        // * we're borrowing a `let` and there's at least one immutable borrowers; or
        // * we're reborrowing from a unique borrower with a stronger or equal capability.
        switch request {
        case .yielded:
          unreachable()

        case .let:
          let isImmutable = borrowers.contains(where: { self[$0].isAccess(.let) })
          if isImmutable || former.map(borrowers.containsOnly(_:)) ?? false {
            o.value.insertBorrower(i)
          } else {
            diagnostics.insert(.error(illegalImmutableAccessAt: s.site))
            hasConflict = true
          }

        case let request:
          let ks = AccessEffectSet([.set, .inout, .sink]).filter(strongerOrEqualTo: request)
          if let f = former, borrowers.containsOnly(f) && self[f].isAccess(in: ks) {
            o.value.removeBorrower(former!)
            o.value.insertBorrower(i)
          } else {
            diagnostics.insert(.error(illegalMutableAccessAt: s.site))
            hasConflict = true
          }
        }
      }

      // Don't set the locals if an error occured to avoid cascading errors downstream.
      if !hasConflict {
        context.locals[.register(i)] = context.locals[s.source]!
      }
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(allocStack i: InstructionID, in context: inout Context) {
      let s = self[i] as! AllocStack
      let l = AbstractLocation.root(.register(i))
      precondition(context.memory[l] == nil, "stack leak")

      context.memory[l] = .init(
        layout: AbstractTypeLayout(of: s.allocatedType, definedIn: program),
        value: .full(.unique))
      context.locals[.register(i)] = .locations([l])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(closeUnion i: InstructionID, in context: inout Context) {
      let s = self[i] as! CloseUnion
      let payload = context.locals[s.start]!.unwrapLocations()!.uniqueElement!

      // The state of the projected payload can't be partial.
      let o = context.withObject(at: payload, { $0 })
      guard case .full(let payloadInitializationState) = o.value else {
        fatalError()
      }

      // Copy the state of the payload to set the state of the container.
      let start = self[s.start.instruction!] as! OpenUnion
      context.forEachObject(at: start.container) { (o) in
        o.value = .full(payloadInitializationState)
      }

      context.memory[payload] = nil
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(deallocStack i: InstructionID, in context: inout Context) {
      let dealloc = self[i] as! DeallocStack
      let l = context.locals[dealloc.location]!.unwrapLocations()!.uniqueElement!
      context.memory[l] = nil
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(endBorrow i: InstructionID, in context: inout Context) {
      let end = self[i] as! EndAccess

      // Skip the instruction if an error occured upstream.
      guard context.locals[end.start] != nil else {
        assert(diagnostics.containsError)
        return
      }

      // Remove the ended borrow from the objects' borrowers, putting the borrowed source back in
      // case the ended borrow was a reborrow.
      let borrower = end.start.instruction!
      let start = self[borrower] as! Access
      let former = reborrowedSource(start)
      context.forEachObject(at: end.start) { (o) in
        if !o.value.removeBorrower(borrower) { return }
        if let s = former {
          switch start.capabilities.uniqueElement! {
          case .let:
            assert(o.value.borrowers.contains(s))
          case .set, .inout, .sink:
            o.value.insertBorrower(s)
          case .yielded:
            unreachable()
          }
        }
      }
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(endProject i: InstructionID, in context: inout Context) {
      let s = self[i] as! EndProject

      // Skip the instruction if an error occured upstream.
      guard context.locals[s.start] != nil else {
        assert(diagnostics.containsError)
        return
      }
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(globalAddr i: InstructionID, in context: inout Context) {
      let s = self[i] as! GlobalAddr
      let l = AbstractLocation.root(.register(i))

      context.memory[l] = .init(
        layout: AbstractTypeLayout(of: s.valueType, definedIn: program),
        value: .full(.unique))
      context.locals[.register(i)] = .locations([l])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(openCapture i: InstructionID, in context: inout Context) {
      let s = self[i] as! OpenCapture

      // Simply share the ownership state of the capture container.
      let source = s.source.instruction!
      context.locals[.register(i)] = context.locals[.register(source)]
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(openUnion i: InstructionID, in context: inout Context) {
      let s = self[i] as! OpenUnion
      let l = AbstractLocation.root(.register(i))
      precondition(context.memory[l] == nil, "projection leak")

      // Operand must be a location.
      let locations = context.locals[s.container]!.unwrapLocations()!

      // Objects at each location have the same state unless DI or LoE has been broken.
      let o = context.withObject(at: locations.first!, { $0 })
      let t = AbstractTypeLayout(of: s.payloadType, definedIn: program)

      context.memory[l] = .init(layout: t, value: o.value)
      context.locals[.register(i)] = .locations([l])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(pointerToAddress i: InstructionID, in context: inout Context) {
      let s = self[i] as! PointerToAddress
      let l = AbstractLocation.root(.register(i))

      context.memory[l] = .init(
        layout: AbstractTypeLayout(of: s.target.bareType, definedIn: program),
        value: .full(.unique))
      context.locals[.register(i)] = .locations([l])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(project i: InstructionID, in context: inout Context) {
      let s = self[i] as! Project
      let l = AbstractLocation.root(.register(i))
      precondition(context.memory[l] == nil, "projection leak")

      context.memory[l] = .init(
        layout: AbstractTypeLayout(of: s.projection.bareType, definedIn: program),
        value: .full(.unique))
      context.locals[.register(i)] = .locations([l])
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(subfieldView i: InstructionID, in context: inout Context) {
      let s = self[i] as! SubfieldView
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
      context.locals[.register(i)] = .locations(Set(newLocations))
    }

    /// Interprets `i` in `context`, reporting violations into `diagnostics`.
    func interpret(wrapExistentialAddr i: InstructionID, in context: inout Context) {
      let s = self[i] as! WrapExistentialAddr
      if case .constant = s.witness {
        // Operand is a constant.
        fatalError("not implemented")
      }

      context.locals[.register(i)] = context.locals[s.witness]
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

  /// Returns the borrowed instruction from which `b` reborrows, if any.
  private func reborrowedSource(_ b: Access) -> InstructionID? {
    if let s = accessSource(b.source).instruction, self[s] is Access {
      return s
    } else {
      return nil
    }
  }

  /// Returns the source of the access denoted by `o`.
  ///
  /// - Requires: `o` denotes a location.
  private func accessSource(_ o: Operand) -> Operand {
    if let a = self[o] as? SubfieldView {
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
