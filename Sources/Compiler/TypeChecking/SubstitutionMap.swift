/// A substitution table mapping type variables to assumptions during type inference
struct SubstitutionMap {

  /// The internal storage of the map.
  private var storage: [TypeVariable: AnyType] = [:]

  /// Creates an empty substitution map.
  init() {}

  /// Returns the substitution for `type` if it is a variable to which a type is assigned in this
  /// map. Otherwise, returns `type`.
  subscript(type: AnyType) -> AnyType {
    var walked = type
    while let a = TypeVariable(walked) {
      if let b = storage[a] {
        walked = b
      } else {
        break
      }
    }
    return walked
  }

  /// Assigns `substition` to `variable`.
  mutating func assign(_ substitution: AnyType, to variable: TypeVariable) {
    var walked = variable
    while let a = storage[walked] {
      guard let b = TypeVariable(a) else {
        precondition(a == substitution, "'\(variable)' already bound to '\(a)'")
        return
      }
      walked = b
    }
    storage[walked] = substitution
  }

  /// Returns a dictionary representing the same mapping as `self`.
  func asDictionary() -> [TypeVariable: AnyType] {
    Dictionary(uniqueKeysWithValues: storage.lazy.map { (k, v) in (k, self[v]) })
  }

}

extension SubstitutionMap: CustomStringConvertible {

  var description: String {
    let elements = storage.map({ (key, value) in "\(key): \(value)" }).joined(separator: ", ")
    return "[\(elements)]"
  }

}

extension SubstitutionMap: CustomReflectable {

  var customMirror: Mirror {
    Mirror(
      self, children: storage.map({ (key, value) in (label: "\(key)", value: value) }),
      displayStyle: .collection)
  }

}
