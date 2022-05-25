import CitronLexerModule

extension EBNF.Grammar {
  static func checkAllSymbolsDefined(in definitions: Definitions, into errors: inout EBNFErrorLog) {
    for d in definitions.values {
      checkDefined(d.alternatives)
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

  func checkAllSymbolsReachable(into errors: inout EBNFErrorLog) {
    var reachable: Set<Symbol> = []
    reach(start)

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
  }

  /*
  typealias Tokens = [Symbol: (String, isRegexp: Bool, position: SourceRegion)]
  func tokens() -> Tokens {
    var literals: Set<String> = []
    var regexps: Set<String> = []
    var visited: Set<Symbol> = []

    func visit(_ x: Token) {
      if !visited.insert(x.text).inserted { return }
      visit(definitions[x.text]!.alternatives)
    }

    func visit(_ x: AlternativeList) {
      for rhs in x { visit(rhs) }
    }

    func visit(_ x: Alternative) {
      for t in x { visit(t) }
    }

    func visit(_ x: Term) {
      switch x {
      case .group(let g): visit(g)
      case .symbol(let s): visit(s)
      case .regexp(let r, _): regexps.insert(r)
      case .literal(let l, _): literals.insert(l)
      case .quantified(let t, _, _): visit(t)
      }
    }
  }
   */
}
