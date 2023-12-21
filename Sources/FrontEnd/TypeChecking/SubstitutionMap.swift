import Core

/// A substitution table mapping type variables to assumptions during type inference.
struct SubstitutionMap {

  /// A policy for substituting type variales during reification.
  enum SubstitutionPolicy {

    /// Free variables are substituted by errors.
    case substitutedByError

    /// Free variables are kept.
    case kept

  }

  /// The internal storage of a substitution table.
  typealias Storage = [TypeVariable: AnyType]

  /// The internal storage of the map.
  private(set) var storage: Storage = [:]

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
  /// map; returns `type` otherwise.
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

  /// Substitutes each type variable occurring in `type` by its corresponding substitution in `self`,
  /// apply `substitutionPolicy` to deal with free variables.
  ///
  /// The default substitution policy is `substituteByError` because we typically use `reify` after
  /// having built a complete solution and therefore don't expect its result to still contain open
  /// type variables.
  func reify(
    _ type: AnyType, withVariables substitutionPolicy: SubstitutionPolicy = .substitutedByError
  ) -> AnyType {
    return type.transform(transform(type:))

    func transform(type: AnyType) -> TypeTransformAction {
      if type.base is TypeVariable {
        let walked = self[type]

        // Substitute `walked` for `type`.
        if walked.base is TypeVariable {
          switch substitutionPolicy {
          case .substitutedByError:
            return .stepOver(.error)
          case .kept:
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
  }

  /// Returns `r` where each type variable occurring in its generic arguments of `r` are replaced by
  /// their corresponding value in `self`, applying `substitutionPolicy` to handle free variables.
  func reify(
    _ r: DeclReference, withVariables substitutionPolicy: SubstitutionPolicy
  ) -> DeclReference {
    switch r {
    case .direct(let d, let a):
      return .direct(d, reify(a, withVariables: substitutionPolicy))
    case .member(let d, let a, let r):
      return .member(d, reify(a, withVariables: substitutionPolicy), r)
    case .constructor(let d, let a):
      return .constructor(d, reify(a, withVariables: substitutionPolicy))
    case .builtinModule, .builtinType, .builtinFunction, .compilerKnownType:
      return r
    }
  }

  /// Returns `a` with its type variables replaced by their their corresponding value in `self`,
  /// applying `substitutionPolicy` to handle free variables.
  private func reify(
    _ a: GenericArguments, withVariables substitutionPolicy: SubstitutionPolicy
  ) -> GenericArguments {
    a.mapValues({ reify(value: $0, withVariables: substitutionPolicy) })
  }

  /// Returns `v` with its type variables replaced by their their corresponding value in `self`,
  /// applying `substitutionPolicy` to handle free variables.
  private func reify(
    value v: CompileTimeValue, withVariables substitutionPolicy: SubstitutionPolicy
  ) -> CompileTimeValue {
    if case .type(let t) = v {
      return .type(reify(t, withVariables: substitutionPolicy))
    } else {
      return v
    }
  }

  /// Removes the key/value pairs in `self` that are not also in `other`.
  mutating func formIntersection(_ other: Self) {
    for (key, lhs) in storage {
      storage[key] = (lhs == other[key]) ? lhs : nil
    }
  }

  /// Returns a new substitution map containing the key/value pairs common to `self` and `other`.
  func intersection(_ other: Self) -> Self {
    var result = SubstitutionMap()
    for (key, lhs) in storage {
      result.storage[key] = (lhs == other[key]) ? lhs : nil
    }
    return result
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
