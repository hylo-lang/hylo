import Marpa
import CitronLexerModule

func makeParser(_ sourceGrammar: EBNF.Grammar) throws -> Parser {
  let g: Marpa.Grammar

  g = Marpa.Grammar()
  var symbols: [String: Marpa.Symbol] = [:]

  // Note: sorting input sets for deterministic results run-to-run.

  // make MARPA symbols for literal strings
  var literals: [String: Marpa.Symbol] = [:]
  for text in sourceGrammar.literals().sorted() {
    let s = g.makeTerminal()
    literals[text] = s
    symbols["'\(text)'"] = s
  }

  // make MARPA symbols for regexp patterns
  let regexps = sourceGrammar.regexps()
  for r in regexps.keys.sorted() {
    symbols[r] = g.makeTerminal()
  }

  let unrecognizedToken = g.makeTerminal()
  var tokenPatterns = Dictionary(
    uniqueKeysWithValues: regexps.lazy.map { name, pattern in (pattern, symbols[name])})
  // ignore whitespace and single-line comments
  tokenPatterns[#"\s+|//.*\p{Zl}*"#, default: nil] = nil 


  // make MARPA symbols for nonterminals
  let sortedNonterminals = sourceGrammar.nonterminals().sorted()
  for s in sortedNonterminals {
    symbols[s] = g.makeNonterminal()
  }
  g.startSymbol = symbols[sourceGrammar.start.text]!

  var symbolName = Dictionary(uniqueKeysWithValues: symbols.lazy.map { (k, v) in (v, k) })
  symbolName[unrecognizedToken] = "<UNRECOGNIZED>"

  // make MARPA rules
  var ruleLocation: [Marpa.Rule: SourceRegion] = [:]
  for lhs in sortedNonterminals {
    for rhs in sourceGrammar.definitions[lhs]!.alternatives {
      makeRule(lhs: symbols[lhs]!, rhs: rhs)
    }
  }

  // Check the grammar and prepare for use.
  if let err = g.precompute() {
    var errors: EBNFErrorLog = []
    switch err {
    case .grammarHasCycle:
      for r in g.rules {
        if g.isLoop(r) {
          errors.insert(
            EBNFError("Rule '\(description(r))' is part of a cycle", at: ruleLocation[r]!))
        }
      }
    default:
      errors.insert(EBNFError("MARPA error: \(err)", at: sourceGrammar.start.position))
    }
    throw errors
  }

  /// Returns a new MARPA rule with the given LHS symbol and MARPA symbols corresponding to rhs.
  func makeRule(lhs: Marpa.Symbol, rhs: EBNF.Grammar.Alternative) {
    let r = g.makeRule(lhs: lhs, rhs: rhs.lazy.map { t in symbol(t, lhs: symbolName[lhs]!) })
    ruleLocation[r] = rhs.position
  }

  /// Returns a new nonterminal symbol with a unique name based on `root`.
  func synthesizedNonterminal(_ root: EBNF.Grammar.Symbol) -> Marpa.Symbol {
    for suffix in 0...50 {
      let name = suffix == 0 ? root : "\(root)_\(suffix)"
      if symbols[name] == nil {
        let r = g.makeNonterminal()
        symbols[name] = r
        symbolName[r] = name
        return r
      }
    }
    fatalError("More than 50 nonterminals synthesized with root '\(root)'?!")
  }

  func description(_ r: Marpa.Rule) -> String {
    let lhsName = symbolName[g.lhs(r)]!
    let rhs = g.rhs(r).lazy.map { s in symbolName[s]! }.joined(separator: " ")
    return "\(lhsName) -> \(rhs)"
  }

  func symbol(_ x: EBNF.Grammar.Term, lhs: EBNF.Grammar.Symbol) -> Marpa.Symbol {
    switch x {
    case .group(let alternatives):
      let innerLHS = synthesizedNonterminal(lhs)
      for rhs in alternatives { makeRule(lhs: innerLHS, rhs: rhs) }
      return innerLHS
    case .symbol(let s): return symbols[s.text]!
    case .regexp(_, _): fatalError("unreachable")
    case .literal(let l, _): return literals[l]!
    case .quantified(let t, let q, _):
      let ts = symbol(t, lhs: lhs)
      let innerLHS = synthesizedNonterminal(symbolName[ts]! + (q == "?" ? "-opt" : "-list"))
      func makeRule<R: Collection>(rhs: R) where R.Element == Marpa.Symbol {
        let r = g.makeRule(lhs: innerLHS, rhs: rhs)
        ruleLocation[r] = x.position
      }
      if q == "*" || q == "?" { makeRule(rhs: EmptyCollection()) }
      if q == "+" || q == "?" { makeRule(rhs: CollectionOfOne(ts)) }
      if q == "*" || q == "+" { makeRule(rhs: [innerLHS, ts]) }
      return innerLHS
    }
  }

  return Parser(
    grammar: g, unrecognizedToken: unrecognizedToken,
    scanner: Scanner(literalStrings: literals, patterns: tokenPatterns),
    symbolName: symbolName,
    ruleLocation: ruleLocation)
}
