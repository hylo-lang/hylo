import Utils

/// The evaluation context of an abstract interpreter.
struct AbstractContext<Domain: AbstractDomain>: Equatable {

  /// The values of the locals.
  var locals: [FunctionLocal: AbstractValue<Domain>] = [:]

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

  /// Calls `action` with a projection of the objects at the locations assigned to `local`.
  ///
  /// - Requires: `locals[k]` is `.locations`.
  mutating func forEachObject(
    at k: FunctionLocal,
    _ action: (inout AbstractObject<Domain>) -> Void
  ) {
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
    case .null:
      preconditionFailure("null location")

    case .argument, .instruction:
      return action(&memory[location]!)

    case .sublocation(let rootLocation, let path):
      if path.isEmpty {
        return action(&memory[location]!)
      } else {
        return modifying(&memory[rootLocation]!, { $0.withSubobject(at: path, action) })
      }
    }
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
