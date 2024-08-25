/// A substitution table mapping type and term variables to assumptions during inference.
struct SubstitutionMap {

  /// A policy for substituting variables during reification.
  enum SubstitutionPolicy {

    /// Free variables are substituted by errors.
    case substitutedByError

    /// Free variables are kept.
    case kept

  }

  /// A map from type variable to its assignment.
  private(set) var types: [TypeVariable: AnyType] = [:]

  /// A map from term variable to its assignment.
  private(set) var terms: [TermVariable: AnyTerm] = [:]

  /// Creates an empty substitution map.
  init() {}

  /// Returns a copy of this instance with its internal representation optimized.
  func optimized() -> Self {
    var result = SubstitutionMap()
    result.types = types.mapValues({ self[$0] })
    return result
  }

  /// Returns the substitution for `v`, if any.
  subscript(v: TypeVariable) -> AnyType? {
    types[walk(v)]
  }

  /// Returns the substitution of `t` in this map or `t` is no such substitution exists.
  subscript(t: AnyType) -> AnyType {
    var walked = t
    while let a = TypeVariable(walked) {
      if let b = types[a] {
        walked = b
      } else {
        break
      }
    }
    return walked
  }

  /// Returns the substitution for `v`, if any.
  subscript(v: TermVariable) -> AnyTerm? {
    terms[walk(v)]
  }

  /// Returns the substitution of `t` in this map or `t` is no such substitution exists.
  subscript(t: AnyTerm) -> AnyTerm {
    var walked = t
    while let a = TermVariable(walked) {
      if let b = terms[a] {
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
    while let a = types[walked] {
      guard let b = TypeVariable(a) else {
        precondition(a == substitution, "'\(variable)' already bound to '\(a)'")
        return
      }
      walked = b
    }

    precondition(
      !occurCheck(walked, substitution),
      "illegal substitution: '\(walked)' for '\(substitution)'")
    types[walked] = substitution
  }

  /// Returns the type variable representing the equivalence class of `v` in `self`.
  private func walk(_ v: TypeVariable) -> TypeVariable {
    var w = v
    while let a = TypeVariable(types[w]) { w = a }
    return w
  }

  /// Returns the term variable representing the equivalence class of `v` in `self`.
  private func walk(_ v: TermVariable) -> TermVariable {
    var w = v
    while let a = TermVariable(terms[w]) { w = a }
    return w
  }

  /// Returns `true` if any variable in the equivalence class of `v` occurs nested in `t`.
  private func occurCheck(_ v: TypeVariable, _ t: AnyType) -> Bool {
    if (t.base is TypeVariable) || !t[.hasVariable] { return false }
    var occurs = false
    t.forEachOpenVariable(mutate: &occurs) { (c, u) in
      c = c || walk(u) == v
    }
    return occurs
  }

  /// Returns a copy of `type` where type variable occurring in is replaced by its corresponding
  /// substitution in `self`, applying `substitutionPolicy` to deal with free variables.
  ///
  /// The default substitution policy is `substituteByError` because we typically use `reify` after
  /// having built a complete solution and therefore don't expect its result to still contain open
  /// type variables.
  func reify(
    _ type: AnyType, withVariables substitutionPolicy: SubstitutionPolicy = .substitutedByError
  ) -> AnyType {
    type.transform { (t: AnyType) -> TypeTransformAction in
      if t.base is TypeVariable {
        let walked = self[t]

        // Substitute `walked` for `type`.
        if walked.base is TypeVariable {
          switch substitutionPolicy {
          case .substitutedByError:
            return .stepOver(.error)
          case .kept:
            return .stepOver(walked)
          }
        } else {
          return .stepInto(walked)
        }
      } else if !t[.hasVariable] {
        // Nothing to do if the type doesn't contain any variable.
        return .stepOver(t)
      } else {
        // Recursively visit other types.
        return .stepInto(t)
      }
    }
  }

  /// Returns a copy of `r` where each generic argument is replaced by the result of applying
  /// `reify(withVariables:)` on it.
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
    self = self.intersection(other)
  }

  /// Returns a new substitution map containing the key/value pairs common to `self` and `other`.
  func intersection(_ other: Self) -> Self {
    var result = SubstitutionMap()
    result.types.reserveCapacity(types.capacity)
    result.terms.reserveCapacity(terms.capacity)
    for (key, lhs) in types {
      result.types[key] = (lhs == other[key]) ? lhs : nil
    }
    for (key, lhs) in terms {
      result.terms[key] = (lhs == other[key]) ? lhs : nil
    }
    return result
  }

}

extension SubstitutionMap: CustomStringConvertible {

  var description: String {
    let items = types.map(String.init(describing:)) + terms.map(String.init(describing:))
    return "[\(list: items)]"
  }

}
