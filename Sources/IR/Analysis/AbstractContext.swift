import Utils

/// The evaluation context of an abstract interpreter.
struct AbstractContext<Domain: AbstractDomain>: Equatable {

  /// The values of the locals.
  ///
  /// - Invariant: The keys of the table are `.register` or `.parameter`.
  var locals: [Operand: AbstractValue<Domain>] = [:]

  /// The state of the memory.
  var memory: [AbstractLocation: AbstractObject<Domain>] = [:]

  /// Creates an empty context.
  init() {}

  /// Forms a context by merging the contexts in `batch`.
  init<C: Collection<Self>>(merging batch: C) {
    if let (h, t) = batch.headAndTail {
      self = t.reduce(into: h, { $0.merge($1) })
    } else {
      self.init()
    }
  }

  /// Merges `other` into `self`.
  mutating func merge(_ other: Self) {
    // Merge the locals.
    for (key, lhs) in locals {
      // Ignore definitions that don't dominate the block.
      guard let rhs = other.locals[key] else {
        locals[key] = nil
        continue
      }

      // Merge both values conservatively.
      locals[key] = lhs && rhs
    }

    // Merge the state of the objects in memory.
    memory.merge(other.memory, uniquingKeysWith: &&)
  }

  /// Calls `action` with a projection of the objects at the locations assigned to `locals[k]`.
  ///
  /// - Requires: If defined, `locals[k]` is `.locations`.
  mutating func forEachObject(at k: Operand, _ action: (inout AbstractObject<Domain>) -> Void) {
    for l in locals[k]!.unwrapLocations()! {
      withObject(at: l, action)
    }
  }

  /// Returns the result calling `action` with a projection of the object at `location`.
  mutating func withObject<T>(
    at location: AbstractLocation,
    _ action: (inout AbstractObject<Domain>) -> T
  ) -> T {
    switch location {
    case .root:
      return action(&memory[location]!)

    case .sublocation(let root, let path):
      if path.isEmpty {
        return action(&memory[location]!)
      } else {
        return modify(&memory[.root(root)]!, { $0.withSubobject(at: path, action) })
      }
    }
  }

  /// Adds a new memory cell in `context` and binds its address to `i`, which is in `m`.
  mutating func declareStorage(
    assignedTo i: InstructionID, from f: Function.ID, in m: Module, initially initialState: Domain
  ) {
    let t = m.type(of: .register(AbsoluteInstructionID(f, i))).ast
    let l = AbstractTypeLayout(of: t, definedIn: m.program)
    let a = AbstractLocation.root(.register(AbsoluteInstructionID(f, i)))
    memory[a] = .init(layout: l, value: .full(initialState))
    locals[.register(AbsoluteInstructionID(f, i))] = .locations([a])
  }

}

extension AbstractContext: CustomStringConvertible {

  var description: String {
    """
    locals:
    \(locals.map({ "- \($0): \($1)" }).joined(separator: "\n"))
    memory:
    \(memory.map({ "- \($0): \($1)" }).joined(separator: "\n"))
    """
  }

}
