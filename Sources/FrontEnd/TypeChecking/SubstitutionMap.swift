import Core

/// A substitution table mapping type variables to assumptions during type inference
struct SubstitutionMap {

  /// A policy for substituting type variales during reification.
  enum SubstitutionPolicy {

    /// Substitute free variables by error types.
    case substituteByError

    /// Do not substitute free variables.
    case keep

  }

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

  /// Subtitutes each type variable occuring in `type` by its corresponding substitution in `self`,
  /// apply `substitutionPolicy` to deal with free variables.
  func reify(_ type: AnyType, withVariables substitutionPolicy: SubstitutionPolicy) -> AnyType {
    func _impl(type: AnyType) -> TypeTransformAction {
      if type.base is TypeVariable {
        // Walk `type`.
        let walked = self[type]

        // Substitute `walked` for `type`.
        if walked.base is TypeVariable {
          switch substitutionPolicy {
          case .substituteByError:
            return .stepInto(.error)
          case .keep:
            return .stepOver(type)
          }
        } else {
          return .stepInto(walked)
        }
      } else if !type[.hasVariable] {
        // Nothing to do if the type doesn't contain any variable.
        return .stepOver(type)
      } else {
        // Recursively visit other types.
        return .stepInto(type)
      }
    }

    return type.transform(_impl(type:))
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
