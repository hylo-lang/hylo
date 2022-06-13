extension EBNF {
  struct Grammar {
    typealias Symbol = EBNF.Symbol
    typealias Error = EBNF.Error
    typealias Alternative = EBNF.Alternative
    typealias AlternativeList = EBNF.AlternativeList
    typealias Term = EBNF.Term
    let definitions: [Definition]
    typealias DefinitionsByLHS = [EBNF.Symbol: Definition]
    let definitionsByLHS: DefinitionsByLHS
    let start: Symbol
  }
}

extension EBNF.Grammar {
  init(_ ast: [EBNF.Definition], start startName: String) throws {
    var errors: EBNFErrorLog = []
    definitions = ast
    definitionsByLHS = Dictionary(ast.lazy.map {(key: $0.lhs, value: $0)}) { a, b in
      errors.insert(
        Error(
        "Duplicate symbol definition", at: b.position,
        notes: [.init(message: "First definition", site: a.position)]))
      return a
    }
    guard let startDefinition = ast.first(where: { d in d.lhs.name == startName }) else {
      errors.insert(
        Error("Start symbol \(startName) not defined\n\(self.definitions)", at: ast.position))
      throw errors
    }
    self.start = startDefinition.lhs

    checkAllSymbolsDefined(into: &errors)
    checkAllSymbolsReachable(into: &errors)
    checkNoRecursiveTokens(into: &errors)
    if !errors.isEmpty { throw errors }
  }
}
