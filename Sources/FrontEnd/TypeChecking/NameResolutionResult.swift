import Core

/// The result of a name resolution request.
enum NameResolutionResult {

  /// Name resolution applied on the nominal prefix that doesn't require any overload resolution.
  /// The payload contains the collections of resolved and unresolved components.
  case done(resolved: [ResolvedComponent], unresolved: [NameExpr.ID])

  /// Name resolution failed.
  case failed

  /// Name resolution couln't start because the first component of the expression isn't a name
  /// The payload contains the collection of unresolved components, after the first one.
  case inexecutable(_ components: [NameExpr.ID])

  /// The result of name resolution for a single name component.
  struct ResolvedComponent {

    /// The resolved component.
    let component: NameExpr.ID

    /// The declarations to which the component may refer.
    let candidates: [Candidate]

    /// Creates an instance with the given properties.
    init(_ component: NameExpr.ID, _ candidates: [Candidate]) {
      self.component = component
      self.candidates = candidates
    }

  }

  /// A candidate found by name resolution.
  struct Candidate {

    /// Declaration being referenced.
    let reference: DeclReference

    /// The quantifier-free type of the declaration at its use site.
    let type: InstantiatedType

    /// The diagnostics of the error related to this candidate's generic arguments, if any.
    let argumentsDiagnostic: Diagnostic?

    /// Creates an instance with the given properties.
    init(reference: DeclReference, type: InstantiatedType, argumentsDiagnostic: Diagnostic?) {
      self.reference = reference
      self.type = type
      self.argumentsDiagnostic = argumentsDiagnostic
    }

    /// Creates an instance denoting a built-in function.
    init(_ f: BuiltinFunction) {
      self.reference = .builtinFunction(f)
      self.type = .init(shape: ^f.type(), constraints: [])
      self.argumentsDiagnostic = nil
    }

    /// Creates an instance denoting a built-in type.
    init(_ t: BuiltinType) {
      self.reference = .builtinType
      self.type = .init(shape: ^t, constraints: [])
      self.argumentsDiagnostic = nil
    }

  }

  /// A set of candidates found by name resolution.
  struct CandidateSet: ExpressibleByArrayLiteral {

    /// The candidates in the set.
    internal private(set) var elements: [Candidate] = []

    /// The positions of candidates in `elements` that are viable given the generic arguments that
    /// where used for name resolution.
    internal private(set) var viable: [Int] = []

    init(arrayLiteral candidates: Candidate...) {
      for c in candidates {
        insert(c)
      }
    }

    /// Inserts `c` into `self`.
    mutating func insert(_ c: Candidate) {
      if c.argumentsDiagnostic == nil {
        viable.append(elements.count)
      }
      elements.append(c)
    }

  }

}
