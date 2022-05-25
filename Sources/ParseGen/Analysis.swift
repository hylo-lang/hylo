/*
extension EBNF.Grammar {
  typealias Tokens = [Symbol: (String, isRegexp: Bool, position: SourceRegion)]
  func tokens() -> Tokens {
    var literals: Set<String> = []
    var regexps: Set<String> = []
    var visited: Set<Symbol> = []

    func visit(_ x: Token) {
      if !visited.insert(x).inserted { return }
      guard let d = definitions[x] else
      for d in definitions[x] { visit(d.alternatives) }
    }

    func visit(_ x: AlternativeList) {
      for rhs in x { visit(rhs) }
    }

    func visit(_ x: Alternative) {
      for rhs in l { visit(rhs) }
    }

    func visit(_ x: Term) {
      switch x {
      case .group(let g): visit(g)
      case .symbol(let s): visit(s)
      case .regexp(let r, _): regexps.insert(r)
      case .literal(let l, _): literals.insert(l)
      case .quantified(let t, let q, _): visit(t)
      }
    }
  }
}
*/
