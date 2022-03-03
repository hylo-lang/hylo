struct GenericEnvironment {

  /// The equivalence classes of the environment.
  var equivalences = EquivalenceClassSet()

  /// The conformances of the environment's generic type parameters.
  var conformances: [String: Set<ViewTypeDecl>] = [:]

}
