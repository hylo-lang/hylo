import Utils

/// Context to interpret the generic parameters of a declaration.
public struct GenericEnvironment {

  /// The declaration associated with the environment.
  public let decl: AnyDeclID

  /// The generic parameters introduced in the environment, in the order there declaration appears
  /// in Hylo sources.
  private(set) var parameters: [GenericParameterDecl.ID]

  /// The traits for which a conformance can be derived given the environment.
  private(set) var dependencies: [TraitType] = []

  /// The uninstantiated type constraints.
  private(set) var constraints: [GenericConstraint] = []

  /// The properties of the generic parameters visible in the environment.
  var requirements = RequirementSystem()

  /// The index of the first public rule.
  private(set) var publicStart = 0

  /// Creates an empty environment associated with `d`, which introduces `parameters`.
  public init(of d: AnyDeclID, introducing parameters: [GenericParameterDecl.ID]) {
    self.decl = d
    self.parameters = parameters
  }

  /// The non-inherited requirement rules in the environment.
  var publicRules: some Collection<RequirementRule> {
    requirements.rules[publicStart...].lazy.filter({ (r) in !r.isSimplified })
  }

  /// Returns the set of traits to which `type` conforms in t`self`.
  func conformedTraits(of t: AnyType, querying checker: inout TypeChecker) -> [TraitType] {
    var result: [TraitType] = []
    let v = checker.buildTerm(^t)
    let w = requirements.reduce(v)
    for c in dependencies {
      let u = v.appending(.trait(c.decl))
      if requirements.reduce(u) == w {
        result.append(c)
      }
    }
    return result
  }

  /// Returns `true` iff `t` is known to be semantically equivalent to `u` in this environment.
  func areEquivalent(_ t: AnyType, _ u: AnyType, querying checker: inout TypeChecker) -> Bool {
    let a = checker.buildTerm(t)
    let b = checker.buildTerm(u)
    return requirements.reduce(a) == requirements.reduce(b)
  }

  /// Add the parameters, dependencies, and constraints of `base` to the environment.
  mutating func registerInheritance(_ base: GenericEnvironment) {
    // Parameters of the inherited environment occur first.
    var ps = base.parameters
    ps.append(disjointContentsOf: parameters)
    self.parameters = ps

    registerDependencies(base.dependencies)
    registerConstraints(base.constraints)
  }

  /// Adds the contents of `ds` to the trait dependencies of the environment.
  mutating func registerDependencies(_ ds: [TraitType]) {
    dependencies.append(disjointContentsOf: ds)
  }

  /// Adds the contents of `cs` to the constraints of the environment.
  mutating func registerConstraints(_ cs: [GenericConstraint]) {
    constraints.append(contentsOf: cs)
  }

  /// Adds `c` to the constraints of the environment.
  mutating func registerConstraint(_ c: GenericConstraint) {
    constraints.append(c)
  }

  /// Marks all the requirement rules currently in the environment as inherited.
  mutating func markRequirementsAsInherited() {
    publicStart = requirements.rules.count
  }

}

extension GenericEnvironment: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    // TODO: What kind of equivalence can we reasonably implement?
    l.decl == r.decl
  }

}

extension Array where Element: Equatable {

  /// Appends to `self` the elements in `s` that not contained in `self`.
  ///
  /// - Complexity: O(*m* * *n*) where *m* is the length of `s` and *n* is the length of `self`.
  fileprivate mutating func append<S: Sequence<Element>>(disjointContentsOf s: S) {
    let r = self[...]
    reserveCapacity(r.count + s.underestimatedCount)
    for e in s where !r.contains(e) {
      append(e)
    }
  }

}
