import Core

/// The result of a name resolution request.
enum NameResolutionResult {

  /// A candidate found by name resolution.
  struct Candidate {

    /// Declaration being referenced.
    let reference: DeclReference

    /// The quantifier-free type of the declaration at its use site.
    let type: InstantiatedType

    // The generic arguments applied to the resolved declaration.
    let arguments: GenericArguments

    /// Creates an instance with the given properties.
    init(reference: DeclReference, type: InstantiatedType, arguments: GenericArguments) {
      self.reference = reference
      self.type = type
      self.arguments = arguments
    }

    /// Creates an instance denoting a built-in function.
    init(_ f: BuiltinFunction) {
      self.reference = .builtinFunction(f)
      self.type = .init(shape: ^f.type(), constraints: [])
      self.arguments = [:]
    }

    /// Creates an instance denoting a built-in type.
    init(_ t: BuiltinType) {
      self.reference = .builtinType
      self.type = .init(shape: ^t, constraints: [])
      self.arguments = [:]
    }

  }

  /// The resolut of name resolution for a single name component.
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

  /// Name resolution applied on the nominal prefix that doesn't require any overload resolution.
  /// The payload contains the collections of resolved and unresolved components.
  case done(resolved: [ResolvedComponent], unresolved: [NameExpr.ID])

  /// Name resolution failed.
  case failed

  /// Name resolution couln't start because the first component of the expression isn't a name
  /// The payload contains the collection of unresolved components, after the first one.
  case inexecutable(_ components: [NameExpr.ID])

}
