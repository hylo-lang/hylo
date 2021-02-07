/// Small acceleration structure search for equivalences in a set of equivalence classes.
public struct EquivalenceClassSet {

  public init() {
    entries = []
    mapping = []
  }

  public init(classes: [[ValType]], numberOfEntries count: Int) {
    entries = []
    entries.reserveCapacity(count)
    mapping = []
    mapping.reserveCapacity(count)

    for class_ in classes {
      for entry in class_ {
        mapping.append(
          (entry,
           Int32(truncatingIfNeeded: entries.count),
           Int32(truncatingIfNeeded: entries.count + class_.count)))
      }
      entries.append(contentsOf: class_)
    }
  }

  var entries: [ValType]

  var mapping: [(ValType, Int32, Int32)]

  /// Returns whether the two given types belong to the same equivalence class.
  public func areEqual(_ lhs: ValType, _ rhs: ValType) -> Bool {
    // Fast check if both types are identical.
    if lhs === rhs { return true }

    // Search through the equivalence classes.
    if let index = mapping.first(where: { lhs == $0.0 }) {
      for i in index.1 ..< index.2 {
        if rhs == entries[Int(truncatingIfNeeded: i)] {
          return true
        }
      }
    }

    // No match.
    return false
  }

  public func equivalenceClass(containing type: ValType) -> ArraySlice<ValType>? {
    if let index = mapping.first(where: { type == $0.0 }) {
      return entries[Int(truncatingIfNeeded: index.1) ..< Int(truncatingIfNeeded: index.2)]
    } else {
      return nil
    }
  }

}
