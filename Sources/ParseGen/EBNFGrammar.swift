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
    let whitespaceOpt: Symbol
    let horizontalSpaceOpt: Symbol
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

    let lhsSymbol: (_: String) throws -> Symbol = { [definitionsByLHS] name in
      if let x = definitionsByLHS[.init(name, at: .empty)] { return x.lhs }
      errors.insert(
        Error("Symbol \(name) not defined\n\(ast)", at: ast.position))
      throw errors
    }
    start = try lhsSymbol(startName)
    whitespaceOpt = try lhsSymbol("whitespace-opt")
    horizontalSpaceOpt = try lhsSymbol("horizontal-space-opt")

    checkAllSymbolsDefined(into: &errors)
    checkAllSymbolsReachable(into: &errors)
    checkNoRecursiveTokens(into: &errors)
    if !errors.isEmpty { throw errors }
  }
}
