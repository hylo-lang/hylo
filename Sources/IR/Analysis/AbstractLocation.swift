import FrontEnd

/// A memory location in an abstract interpreter.
enum AbstractLocation: Hashable, Sendable {

  /// A root location.
  case root(Operand)

  /// A sub-location rooted at an argument or instruction.
  ///
  /// `path[i]` denotes the index of a property in the abstract layout of the object stored at
  /// `.sublocation(root: r, path: path.prefix(upTo: i))`. For example, if `r` is the location
  /// identifying storage of type `{{A, B}, C}`, then `sublocation(root: r, path: [0, 1])` is a
  /// location identifying storage of type `B`.
  ///
  /// - Note: Use `appending(_:)` to create instances of this case.
  indirect case sublocation(root: Operand, subfield: RecordPath)

  /// Returns a new locating created by appending `suffix` to this one.
  ///
  /// - Requires: `self` is not `.null`.
  func appending(_ suffix: RecordPath) -> AbstractLocation {
    if suffix.isEmpty { return self }

    switch self {
    case .root(let root):
      return .sublocation(root: root, subfield: suffix)
    case .sublocation(let root, let prefix):
      return .sublocation(root: root, subfield: prefix + suffix)
    }
  }

}

extension AbstractLocation: CustomStringConvertible {

  var description: String {
    switch self {
    case .root(let r):
      return String(describing: r)
    case .sublocation(let root, let path):
      return "\(root).\(list: path, joinedBy: ".")"
    }
  }

}
