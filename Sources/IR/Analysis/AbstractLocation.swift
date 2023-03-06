import Core

/// A memory location in an abstract interpreter.
enum AbstractLocation: Hashable {

  /// The null location.
  case null

  /// The location of an argument to a `let`, `inout`, or `set` parameter.
  case argument(index: Int)

  /// A location produced by an instruction.
  case instruction(block: Function.Blocks.Address, address: Block.Instructions.Address)

  /// A sub-location rooted at an argument or instruction.
  ///
  /// `path[i]` denotes the index of a property in the abstract layout of the object stored at
  /// `.sublocation(root: r, path: path.prefix(upTo: i))`. For example, if `r` is the location
  /// identifying storage of type `{{A, B}, C}`, then `sublocation(root: r, path: [0, 1])` is a
  /// location identifying storage of type `B`.
  ///
  /// - Note: Use `appending(_:)` to create instances of this case.
  /// - Requires: `root` is `.argument` or `.instruction` and `path` is not empty.
  indirect case sublocation(root: AbstractLocation, path: PartPath)

  /// Returns a new locating created by appending `suffix` to this one.
  ///
  /// - Requires: `self` is not `.null`.
  func appending(_ suffix: PartPath) -> AbstractLocation {
    if suffix.isEmpty { return self }

    switch self {
    case .null:
      preconditionFailure("null location")
    case .argument, .instruction:
      return .sublocation(root: self, path: suffix)
    case .sublocation(let root, let prefix):
      return .sublocation(root: root, path: prefix + suffix)
    }
  }

}

extension AbstractLocation: CustomStringConvertible {

  var description: String {
    switch self {
    case .null:
      return "Null"
    case .argument(let k):
      return "a\(k)"
    case .instruction(let b, let i):
      return "i\(b).\(i)"
    case .sublocation(let root, let path):
      return "\(root).\(list: path, joinedBy: ".")"
    }
  }

}
