import CitronLexerModule
import Foundation
import Marpa

extension EBNF.Grammar {
  /// Adds errors to `errors` for any symbols that don't appear on the LHS of a definition.
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

  /// Adds errors to `errors` for any symbols that can't be produced by the start symbol.
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

  /// Adds errors to `errors` for any `(token)` rules that are (directly or
  /// indirectly) self-referential.
  func checkNoRecursiveTokens(into errors: inout EBNFErrorLog) {
    var stack: [Token] = []
    var visited: Set<String> = []
    for d in definitions.values where d.kind == .token {
      visit(d.lhs)
    }

    func visit(_ x: Token) {
      stack.append(x)
      defer { stack.removeLast() }

      if visited.contains(x.text) {
        errors.insert(
          Error(
            "Recursive token definition '\(stack[0].text)'", at: stack[0].position,
            notes: stack.dropFirst().map {
              .init(message: "via: '\($0.text)'", site: $0.position)
            }))
      }
      else {
        visited.insert(x.text)
        defer { visited.remove(x.text) }

        visit(definitions[x.text]!.alternatives)
      }
    }

    func visit(_ x: EBNF.AlternativeList) {
      for a in x {
        for t in a { visit(t) }
      }
    }

    func visit(_ x: Term) {
      switch x {
      case .group(let g): visit(g)
      case .symbol(let s): visit(s)
      case .regexp(_, _): do {}
      case .literal(_, _): do {}
      case .quantified(let t, _, _): visit(t)
      }
    }
  }

  /// Returns the set of literal strings recognized as terminals.
  func literals() -> Set<String> {
    var visited: Set<String> = []
    var r: Set<String> = []

    visit(start)

    func visit(_ x: Token) {
      if !visited.insert(x.text).inserted { return }
      let d = definitions[x.text]!
      if d.kind == .plain || d.kind == .oneOf {
        visit(d.alternatives)
      }
    }

    func visit(_ x: AlternativeList) {
      for a in x {
        for t in a { visit(t) }
      }
    }

    func visit(_ x: Term) {
      switch x {
      case .group(let g): 
        visit(g)
      case .symbol(let s): 
        visit(s)
      case .regexp(_, _): do {}
      case .literal(let l, _): 
        r.insert(l)
      case .quantified(let t, _, _):
        visit(t)
      }
    }
    return r
  }

  /// Returns a mapping from terminal symbols to the regular expressions that describe them.
  func regexps() -> [Symbol: String] {
    var visited: Set<Symbol> = []
    var r: [Symbol: String] = [:]
    visitSymbol(start)

    func visitSymbol(_ x: Token) {
      if !visited.insert(x.text).inserted { return }

      let d = definitions[x.text]!
      if d.kind == .token || d.kind == .regexp {
        r[x.text] = regexp(d.alternatives)
      }
      else {
        visitSymbols(in: d.alternatives)
      }
    }

    func visitSymbols(in x: AlternativeList) {
      for a in x {
        for t in a { visitSymbols(in: t) }
      }
    }

    func visitSymbols(in x: Term) {
      switch x {
      case .group(let g):
        visitSymbols(in: g)
      case .symbol(let s):
        visitSymbol(s)
      case .regexp(_, _): fatalError("unreachable")
      case .literal(_, _): return
      case .quantified(let t, _, _):
        visitSymbols(in: t)
      }
    }

    func regexp(_ x: Token) -> String {
      let d = definitions[x.text]!
      return regexp(d.alternatives)
    }

    func regexp(_ x: AlternativeList) -> String {
      let inner = x.lazy.map { a in regexp(a) }.joined(separator: "|")
      return x.count <= 1 ? inner : "(?:" + inner + ")"
    }

    func regexp(_ a: Alternative) -> String {
      a.lazy.map { t in regexp(t) }.joined()
    }

    func regexp(_ x: Term) -> String {
      switch x {
      case .group(let g): return regexp(g)
      case .symbol(let s): return regexp(s)
      case .regexp(let r, _): return r
      case .literal(let l, _): return NSRegularExpression.escapedPattern(for: l)
      case .quantified(let t, let q, _): return regexp(t) + String(q)
      }
    }
    return r
  }

  /// Returns the nonterminal symbols of the analyzed grammar.
  func nonterminals() -> Set<Symbol> {
    var r: Set<Symbol> = []

    visit(start)

    func visit(_ x: Token) {
      let d = definitions[x.text]!
      if d.kind == .regexp || d.kind == .token { return }
      if !r.insert(x.text).inserted { return }
      visit(d.alternatives)
    }

    func visit(_ x: AlternativeList) {
      for a in x {
        for t in a { visit(t) }
      }
    }

    func visit(_ x: Term) {
      switch x {
      case .group(let g): visit(g)
      case .symbol(let s): visit(s)
      case .regexp(_, _): do {}
      case .literal(_, _): do {}
      case .quantified(let t, _, _):
        visit(t)
      }
    }
    return r
  }

  func finalized() -> (
    scanner: CitronLexerModule.Scanner<Marpa.Symbol>,
    unrecognizedToken: Marpa.Symbol,
    grammar: Marpa.Grammar,
    symbolNames: [Marpa.Symbol: String]
  ) {
    let g = Marpa.Grammar()

    // Sort these sets for deterministic results run-to-run.
    let regexps = self.regexps()
    let nonterminals = self.nonterminals().sorted()

    let unrecognizedToken = g.makeTerminal()
    var symbols = ["<UNRECOGNIZED>": unrecognizedToken]
    var literals: [String: Marpa.Symbol] = [:]

    for text in self.literals().sorted() {
      literals[text] = g.makeTerminal()
    }
    for r in regexps.keys.sorted() {
      symbols[r] = g.makeTerminal()
    }
    for s in nonterminals {
      symbols[s] = g.makeNonterminal()
    }
    g.startSymbol = symbols[start.text]!

    func newNonterminal(_ root: String) -> Marpa.Symbol {
      for suffix in 0...20 {
        let name = suffix == 0 ? root : "\(root)_\(suffix)"
        if symbols[name] == nil {
          let r = g.makeNonterminal()
          symbols[name] = r
          return r
        }
      }
      fatalError("More than 20?!")
    }

    for lhs in nonterminals {
      for rhs in definitions[lhs]!.alternatives {
        _ = g.makeRule(lhs: symbols[lhs]!, rhs: rhs.lazy.map { t in symbol(t, lhsContext: lhs) })
      }
    }

    let symbolNames = Dictionary(
      uniqueKeysWithValues: literals.map { kv in (kv.1, kv.0) }
        + symbols.lazy.map { kv in (kv.1, kv.0) }
    )

    func symbol(_ x: Term, lhsContext: String) -> Marpa.Symbol {
      switch x {
      case .group(let alternatives):
        let innerLHS = newNonterminal(lhsContext)
        for rhs in alternatives {
          _ = g.makeRule(
            lhs: innerLHS, rhs: rhs.lazy.map { t in symbol(t, lhsContext: lhsContext) })
        }
        return innerLHS
      case .symbol(let s): return symbols[s.text]!
      case .regexp(_, _): fatalError("unreachable")
      case .literal(let l, _): return literals[l]!
      case .quantified(let t, "?", _):
        let innerLHS = newNonterminal(lhsContext)
        _ = g.makeRule(lhs: innerLHS, rhs: EmptyCollection())
        _ = g.makeRule(lhs: innerLHS, rhs: CollectionOfOne(symbol(t, lhsContext: lhsContext)))
        return innerLHS

      case .quantified(let t, let q, _):
        // TODO use sequence rules.
        let innerLHS = newNonterminal(lhsContext)
        let ts = symbol(t, lhsContext: lhsContext)
        if q == "+" {
          _ = g.makeRule(lhs: innerLHS, rhs: CollectionOfOne(ts))
        }
        else {
          _ = g.makeRule(lhs: innerLHS, rhs: EmptyCollection())
        }
        return innerLHS
      }
    }
    var patterns = Dictionary(
      uniqueKeysWithValues: regexps.lazy.map {
        name, pattern in (pattern, symbols[name]!)})
    patterns[#"\s+"#] = nil // ignore whitespace

    g.precompute()

    return (
      Scanner(literalStrings: literals, patterns: patterns),
      unrecognizedToken,
      g,
      symbolNames)
  }
}
