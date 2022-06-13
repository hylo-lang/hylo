import Marpa
import CitronLexerModule

extension Collection {
  // TODO: Should return an efficient adapter
  /// Returns the elements of `self` with `x` between each adjacent pair.
  func interjecting(_ x: Element) -> [Element] {
    var r: [Element] = []
    var source = self[...]
    if let first = source.popFirst() {
      r.reserveCapacity(count * 2 - 1)
      r.append(first)
      r.append(contentsOf: source.lazy.flatMap { y in [x, y] })
    }
    return r
  }
}

func makeParser(_ sourceGrammar: EBNF.Grammar) throws -> Parser {
  let g: Marpa.Grammar

  g = Marpa.Grammar()
  var symbols: [String: Marpa.Symbol] = [:]
  var astSymbols: [AnyHashable: Marpa.Symbol] = [:]

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

  // make MARPA symbols for nonterminals
  let sortedNonterminals = sourceGrammar.nonterminals().sorted()
  for s in sortedNonterminals {
    symbols[s] = g.makeNonterminal()
  }
  g.startSymbol = symbols[sourceGrammar.start.text]!

  var symbolName = Dictionary(uniqueKeysWithValues: symbols.lazy.map { (k, v) in (v, k) })
  symbolName[unrecognizedToken] = "<UNRECOGNIZED>"
  let whitespaceOpt = symbols["whitespace-opt"]!
  let horizontalSpaceOpt = symbols["horizontal-space-opt"]!

  // make MARPA rules
  var ruleLocation: [Marpa.Rule: SourceRegion] = [:]
  for lhs in sortedNonterminals {
    let d = sourceGrammar.definitionsByLHS[lhs]!
    for rhs in d.alternatives {
      makeRule(lhs: symbols[lhs]!, rhs: rhs, kind: d.kind)
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

  /// Returns a new MARPA rule of the given `kind` with the given `lhs` symbol
  /// and MARPA symbols corresponding to `rhs`.
  func makeRule(lhs: Marpa.Symbol, rhs: EBNF.Grammar.Alternative, kind: EBNF.Definition.Kind) {
    let basicRHS = rhs.lazy.map { t in symbol(t, lhs: symbolName[lhs]!, kind: kind) }
    makeRule(lhs: lhs, rhs: basicRHS, kind: kind, origin: rhs.position)
  }

  /// Returns a new MARPA rule of the given `kind` with the given `lhs` and `rhs` symbol.
  func makeRule<RHS: Collection>(
    lhs: Marpa.Symbol, rhs basicRHS: RHS, kind: EBNF.Definition.Kind,
    origin: SourceRegion
  )
    where RHS.Element == Marpa.Symbol
  {
    let r = kind == .noWhitespace
      ? g.makeRule(lhs: lhs, rhs: basicRHS)
      : g.makeRule(
        lhs: lhs,
        rhs: basicRHS.interjecting(kind == .noNewline ? horizontalSpaceOpt : whitespaceOpt))
    ruleLocation[r] = origin
  }

  /// Returns a new nonterminal symbol with a unique name based on `root`.
  func makeSymbol(_ root: EBNF.Grammar.Symbol, terminal: Bool = false) -> Marpa.Symbol {
    for suffix in 0...50 {
      let name = suffix == 0 ? root : "\(root)_\(suffix)"
      if symbols[name] == nil {
        let r = terminal ? g.makeTerminal() : g.makeNonterminal()
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

  func symbol(_ x: EBNF.Grammar.Term, lhs: EBNF.Grammar.Symbol, kind: EBNF.Definition.Kind)
    -> Marpa.Symbol
  {
    if let r = astSymbols[x] { return r }
    switch x {
    case .group(let alternatives):
      let innerLHS = makeSymbol(lhs)
      astSymbols[x] = innerLHS
      for rhs in alternatives { makeRule(lhs: innerLHS, rhs: rhs, kind: kind) }
      return innerLHS
    case .symbol(let s): return symbols[s.text]!
    case .regexp(_, _): fatalError("unreachable")
    case .literal(let l, _): return literals[l]!

    case .quantified(let t, let q, _):
      let t1 = symbol(t, lhs: lhs, kind: kind)
      let innerLHS = makeSymbol(symbolName[t1]! + (q == "?" ? "-opt" : "-list"))
      if q == "*" || q == "?" {
        makeRule(lhs: innerLHS, rhs: EmptyCollection(), kind: kind, origin: x.position)
      }
      if q == "+" || q == "?" {
        makeRule(lhs: innerLHS, rhs: CollectionOfOne(t1), kind: kind, origin: x.position)
      }
      if q == "*" || q == "+" {
        makeRule(lhs: innerLHS, rhs: [innerLHS, t1], kind: kind, origin: x.position)
      }
      return innerLHS
    }
  }

  let tokenPatterns = Dictionary(
    uniqueKeysWithValues: regexps.lazy.map { name, pattern in (pattern, symbols[name])})

  return Parser(
    grammar: g, unrecognizedToken: unrecognizedToken,
    scanner: Scanner(literalStrings: literals, patterns: tokenPatterns),
    symbolName: symbolName,
    ruleLocation: ruleLocation)
}
