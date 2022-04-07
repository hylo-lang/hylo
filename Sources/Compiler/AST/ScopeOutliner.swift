/// An AST node that outlines a lexical scope.
protocol ScopeOutliner {

  /// A scope identifier that's unique in a given module.
  ///
  /// - Note: Use `ModuleDecl.makeScopeID` to generate unique scope identifiers.
  var scopeID: ScopeID { get }

}

/// A scope identifier.
typealias ScopeID = Int
