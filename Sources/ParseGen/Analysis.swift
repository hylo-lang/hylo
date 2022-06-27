import CitronLexerModule
import Foundation
import Marpa

extension EBNF.Grammar {
  private typealias Symbol = EBNF.Symbol
  private typealias Error = EBNF.Error
  private typealias Alternative = EBNF.Alternative
  private typealias AlternativeList = EBNF.AlternativeList
  private typealias Term = EBNF.Term

  /// Adds errors to `errors` for any symbols that don't appear on the LHS of a definition.
  func checkAllSymbolsDefined(
    into errors: inout EBNFErrorLog
  ) {
    for d in definitionsByLHS.values {
      checkDefined(d.alternatives)
    }

    func checkDefined(_ x: EBNF.Symbol) {
      if definitionsByLHS[x] == nil {
        errors.insert(Error("undefined symbol '\(x.name)'", at: x.position))
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

    for d in definitions {
      if !reachable.contains(d.lhs) {
        errors.insert(
          Error("Symbol '\(d.lhs.name)' is unreachable from start symbol '\(start.name)'",
                at: d.lhs.position))
      }
    }

    func reach(_ x: Symbol) {
      if reachable.insert(x).inserted {
        reach(definitionsByLHS[x]!.alternatives)
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
    var stack: [EBNF.Symbol] = []
    var visited: Set<EBNF.Symbol> = []
    for d in definitions where d.kind == .token {
      visit(d.lhs)
    }

    func visit(_ x: EBNF.Symbol) {
      stack.append(x)
      defer { stack.removeLast() }

      if visited.contains(x) {
        errors.insert(
          Error(
            "Recursive token definition '\(stack[0].name)'", at: stack[0].position,
            notes: stack.dropFirst().map {
              .init(message: "via: '\($0.name)'", site: $0.position)
            }))
      }
      else {
        visited.insert(x)
        defer { visited.remove(x) }

        visit(definitionsByLHS[x]!.alternatives)
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

    func visit(_ x: Symbol) {
      if !visited.insert(x.name).inserted { return }
      let d = definitionsByLHS[x]!
      if d.kind != .token && d.kind != .regexp {
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
  func regexps() -> [EBNF.Symbol: String] {
    var visited: Set<Symbol> = []
    var r: [Symbol: String] = [:]
    visitSymbol(start)

    func visitSymbol(_ x: Symbol) {
      if !visited.insert(x).inserted { return }

      let d = definitionsByLHS[x]!
      if d.kind == .token || d.kind == .regexp {
        r[x] = regexp(d.alternatives)
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

    func regexp(_ x: Symbol) -> String {
      let d = definitionsByLHS[x]!
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
  func nonterminals() -> Set<EBNF.Symbol> {
    var r: Set<Symbol> = []

    visit(start)

    func visit(_ x: Symbol) {
      let d = definitionsByLHS[x]!
      if d.kind == .regexp || d.kind == .token { return }
      if !r.insert(x).inserted { return }
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
}
