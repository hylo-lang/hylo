// Implementation notes
// ====================
//
// `EquivalenceSet` uses two arrays. The first is a list of types. The second is a list of triples
// `(Type, Index, Index)` encoding mapping `Type -> (Index, Index)` from types to the slice of the
// first array representing their equivalence class.
//
// For example, let `S = [[A, B], [C, D, E]]` be a set of two equivalence classes:
//
//   entries: [A, B, C, D, E]
//   mapping: [(A, 0, 2), (B, 0, 2), (C, 2, 5), (D, 2, 5), (E, 2, 5)]
//
// To look for an equivalence `T == U`, we run a linear search on the first array to find the
// indices of `T`'s equivalence class. Then we run another linear search to check whether `U` is
// part of the class.
//
// The complexity of a lookup is O(n^2) in the worst case. Because equivalence classes are usually
// small, however, the overhead of hash tables will exceed that of the cost of a linear search.

/// An acceleration structure to search for equivalences in a set of equivalence classes.
public struct EquivalenceSet {

  private var entries: [ValType]

  private var mapping: [(type: ValType, startIndex: Int32, endIndex: Int32)]

  /// Creates an empty set of equivalence classes.
  public init() {
    entries = []
    mapping = []
  }

  /// Creates a set of equivalence classes.
  ///
  /// - Parameters:
  ///   - classes: The equivalence classes to encode.
  ///   - count: The total number of distinct types in all equivalence classes.
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

  /// Returns whether the two given types belong to the same equivalence class.
  ///
  /// - Parameters:
  ///   - lhs: A type.
  ///   - rhs: Another type.
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

  /// Returns the equivalence class in which the specified type belongs.
  ///
  /// - Parameter type: A type.
  public func equivalenceClass(containing type: ValType) -> ArraySlice<ValType>? {
    if let index = mapping.first(where: { type == $0.0 }) {
      return entries[Int(truncatingIfNeeded: index.1) ..< Int(truncatingIfNeeded: index.2)]
    } else {
      return nil
    }
  }

}
