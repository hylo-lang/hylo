extension EBNF {
  struct Grammar {
    typealias Token = EBNF.Token
    typealias Error = EBNF.Error
    typealias Alternative = EBNF.Alternative
    typealias AlternativeList = EBNF.AlternativeList
    typealias Term = EBNF.Term
    typealias Symbol = String
    typealias Definitions = [Symbol: Definition]
    let definitions: Definitions
    let start: Token
  }
}

extension EBNF.Grammar {
  init(_ ast: [EBNF.Definition], start: Symbol) throws {
    var errors: EBNFErrorLog = []

    definitions = Dictionary(ast.lazy.map {(key: $0.lhs.text, value: $0)}) { a, b in
      errors.insert(
        Error(
        "Duplicate symbol definition", at: b.position,
        notes: [.init(message: "First definition", site: a.position)]))
      return a
    }

    Self.checkAllSymbolsDefined(in: definitions, into: &errors)
    guard let d = self.definitions[start] else {
      errors.insert(
        Error("Start symbol \(start) not defined\n\(self.definitions)", at: ast.position))
      throw errors
    }
    self.start = d.lhs
    checkAllSymbolsReachable(into: &errors)
    checkNoRecursiveTokens(into: &errors)
    if !errors.isEmpty { throw errors }
  }
}
