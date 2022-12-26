import Core

/// A substitution table mapping type variables to assumptions during type inference
struct SubstitutionMap {

  /// The internal storage of the map.
  private var storage: [TypeVariable: AnyType] = [:]

  /// Creates an empty substitution map.
  init() {}

  /// Returns a copy of this instance with its internal representation optimized.
  func optimized() -> Self {
    var result = SubstitutionMap()
    result.storage = storage.mapValues({ self[$0] })
    return result
  }

  /// Returns the substitution for `variable`, if any.
  subscript(variable: TypeVariable) -> AnyType? {
    if let t = storage[variable] {
      return self[t]
    } else {
      return nil
    }
  }

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

  /// Assigns `substitution` to `variable`.
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

}

extension SubstitutionMap: ExpressibleByDictionaryLiteral {

  init(dictionaryLiteral elements: (TypeVariable, AnyType)...) {
    for (k, v) in elements {
      self.assign(v, to: k)
    }
  }

}

extension SubstitutionMap: CustomStringConvertible {

  var description: String { String(describing: storage) }

}

extension SubstitutionMap: CustomReflectable {

  var customMirror: Mirror { storage.customMirror }

}
