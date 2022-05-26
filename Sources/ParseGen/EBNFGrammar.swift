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

  func validate(allowUnreachable: Bool = false, into errors: inout EBNFErrorLog) {
    var reachable: Set<Symbol> = []

    for d in definitions.values {
      checkDefined(d.alternatives)
    }

    reach(start)

    if allowUnreachable { return }

    for d in definitions.values {
      if !reachable.contains(d.lhs.text) {
        errors.insert(
          Error("Symbol '\(d.lhs.text)' is unreachable from start symbol '\(start.text)'",
                at: d.lhs.position))
      }
    }

    func reach(_ x: Token) {
      if reachable.insert(x.text).inserted {
        reach(definitions[x.text]!.alternatives)
      }
    }

    func reach(_ x: EBNF.AlternativeList) {
      for a in x {
        for t in a { reach(t) }
      }
    }

    func reach(_ x: Term) {
      switch x {
      case .group(let g): reach(g)
      case .symbol(let s): reach(s)
      case .regexp(_, _): do {}
      case .literal(_, _): do {}
      case .quantified(let t, _, _): reach(t)
      }
    }

    func checkDefined(_ x: EBNF.Token) {
      if definitions[x.text] == nil {
        errors.insert(Error("undefined symbol '\(x.text)'", at: x.position))
      }
    }

    func checkDefined(_ x: EBNF.AlternativeList) {
      for a in x {
        for t in a { checkDefined(t) }
      }
    }

    func checkDefined(_ x: Term) {
      switch x {
      case .group(let g): checkDefined(g)
      case .symbol(let s): checkDefined(s)
      case .regexp(_, _): do {}
      case .literal(_, _): do {}
      case .quantified(let t, _, _): checkDefined(t)
      }
    }
  }
}
