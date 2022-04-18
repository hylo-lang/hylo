/// A substitution table mapping type variables to assumptions during type inference
struct SubstitutionMap {

  /// The internal storage of the map.
  private var storage: [TypeVariable: Type] = [:]

  /// Creates an empty substitution map.
  init() {}

  /// Returns the substitution for `type` if it is a variable to which a type is assigned in this
  /// map. Otherwise, returns `type`.
  subscript(type: Type) -> Type {
    var walked = type
    while case .variable(let a) = walked {
      if let b = storage[a] {
        walked = b
      } else {
        break
      }
    }
    return walked
  }

  /// Assigns `substition` to `variable`.
  mutating func assign(_ substitution: Type, to variable: TypeVariable) {
    var walked = variable
    while let a = storage[walked] {
      guard case .variable(let b) = a else {
        precondition(a == substitution, "'\(variable)' already bound to '\(a)'")
        return
      }
      walked = b
    }
    storage[walked] = substitution
  }

  /// Returns the contents of the map flattened into a dictionary.
  func flattened() -> [TypeVariable: Type] {
    storage.reduce(into: [:], { (d, e) in d[e.key] = self[e.value] })
  }

}

extension SubstitutionMap: Collection {

  typealias Index = Dictionary<TypeVariable, Type>.Index

  typealias Element = (key: TypeVariable, value: Type)

  var startIndex: Index { storage.startIndex }

  var endIndex: Index { storage.endIndex }

  func index(after i: Index) -> Index { storage.index(after: i) }

  subscript(position: Index) -> Element {
    let key = storage[position].key
    return (key: key, value: self[.variable(key)])
  }

}

extension SubstitutionMap: CustomStringConvertible {

  var description: String {
    let elements = self.map({ (key, value) in "\(key): \(value)" }).joined(separator: ", ")
    return "[\(elements)]"
  }

}

extension SubstitutionMap: CustomReflectable {

  var customMirror: Mirror {
    Mirror(
      self,
      children: self.map({ (key, value) in (label: "\(key)", value: value) }),
      displayStyle: .collection)
  }

}
