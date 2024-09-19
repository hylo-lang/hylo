/// A substitution table mapping type and term variables to assumptions during inference.
struct SubstitutionMap {

  /// A policy for substituting variables during reification.
  enum SubstitutionPolicy {

    /// Free variables are substituted by errors.
    case substitutedByError

    /// Free variables are kept.
    case kept

    /// Returns the application of this policy to `t`.
    fileprivate func callAsFunction(_ t: TypeVariable) -> AnyType {
      switch self {
      case .substitutedByError:
        return .error
      case .kept:
        return ^t
      }
    }

    /// Returns the application of this policy to `t`.
    fileprivate func callAsFunction(_ t: TermVariable) -> AnyTerm {
      switch self {
      case .substitutedByError:
        return .error
      case .kept:
        return ^t
      }
    }

  }

  /// A map from type variable to its assignment.
  private(set) var types: [TypeVariable: AnyType]

  /// A map from term variable to its assignment.
  private(set) var terms: [TermVariable: AnyTerm]

  /// Creates an empty substitution map.
  init() {
    self.init(types: [:], terms: [:])
  }

  /// Creates an instance with the given properties.
  private init(types: [TypeVariable: AnyType], terms: [TermVariable: AnyTerm]) {
    self.types = types
    self.terms = terms
  }

  /// Returns a copy of this instance with its internal representation optimized.
  func optimized() -> Self {
    .init(
      types: types.mapValues({ self[$0] }),
      terms: terms.mapValues({ self[$0] }))
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

  /// Assigns `substitution` to `variable`.
  mutating func assign(_ substitution: AnyTerm, to variable: TermVariable) {
    var walked = variable
    while let a = terms[walked] {
      guard let b = TermVariable(a) else {
        precondition(a == substitution, "'\(variable)' already bound to '\(a)'")
        return
      }
      walked = b
    }
    terms[walked] = substitution
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

  /// Returns a copy of `type` where each variable is replaced by its substitution in `self` or the
  /// application of `substitutionPolicy` is no such substitution exists.
  ///
  /// The default substitution policy is `substituteByError` because we typically use `reify` after
  /// having built a complete solution and therefore don't expect its result to still contain open
  /// type variables.
  func reify(
    _ type: AnyType, withVariables substitutionPolicy: SubstitutionPolicy = .substitutedByError
  ) -> AnyType {
    type.transform { (t: AnyType) -> TypeTransformAction in
      switch t.base {
      case let u as BufferType:
        let n = reify(u.count, withVariables: substitutionPolicy)
        return .stepInto(^BufferType(u.element, n))

      case _ where t[.hasVariable]:
        let walked = self[t]
        if let w = TypeVariable(walked) {
          return .stepOver(substitutionPolicy(w))
        } else {
          return .stepInto(walked)
        }

      default:
        // Nothing to do if the type doesn't contain any variable.
        return .stepOver(t)
      }
    }
  }

  /// Returns a copy of `term` where each variable is replaced by its substitution in `self` or the
  /// application of `substitutionPolicy` is no such substitution exists.
  func reify(
    _ term: AnyTerm, withVariables substitutionPolicy: SubstitutionPolicy
  ) -> AnyTerm {
    let walked = self[term]
    return TermVariable(walked).map({ (w) in substitutionPolicy(w) }) ?? walked
  }

  /// Returns a copy of `r` where each generic argument is replaced by the result of applying
  /// `reify(withVariables:)` on it.
  func reify(
    reference r: DeclReference, withVariables substitutionPolicy: SubstitutionPolicy
  ) -> DeclReference {
    switch r {
    case .direct(let d, let a):
      return .direct(d, reify(argument: a, withVariables: substitutionPolicy))
    case .member(let d, let a, let r):
      return .member(d, reify(argument: a, withVariables: substitutionPolicy), r)
    case .constructor(let d, let a):
      return .constructor(d, reify(argument: a, withVariables: substitutionPolicy))
    case .builtinModule, .builtinType, .builtinFunction, .compilerKnownType:
      return r
    }
  }

  /// Returns a copy of `a` where each variable is replaced by its substitution value in `self` or
  /// the application `substitutionPolicy` is no such substitution exists.
  private func reify(
    argument a: GenericArguments, withVariables substitutionPolicy: SubstitutionPolicy
  ) -> GenericArguments {
    a.mapValues({ reify(value: $0, withVariables: substitutionPolicy) })
  }

  /// Returns a copy of `v` where each variable is replaced by its substitution value in `self` or
  /// the application `substitutionPolicy` is no such substitution exists.
  private func reify(
    value v: CompileTimeValue, withVariables substitutionPolicy: SubstitutionPolicy
  ) -> CompileTimeValue {
    switch v {
    case .type(let t):
      return .type(reify(t, withVariables: substitutionPolicy))
    case .term(let t):
      return .term(reify(t, withVariables: substitutionPolicy))
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
    let ts = types.map({ "\($0.key): \($0.value)" })
    let us = terms.map({ "\($0.key): \($0.value)" })
    return "[\(list: ts + us)]"
  }

}
