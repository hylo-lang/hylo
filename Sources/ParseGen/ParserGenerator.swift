import Marpa
import CitronLexerModule

func makeParser(_ sourceGrammar: EBNF.Grammar) throws -> Parser {
  var bnfizer = EBNFToBNF(from: sourceGrammar, into: MarpaGrammarBuilder())
  bnfizer.build()
  let g = bnfizer.output.result

  let unrecognizedToken = g.makeTerminal()

  // Check the grammar and prepare for use.
  if let err = g.precompute() {
    var errors: EBNFErrorLog = []
    switch err {
    case .grammarHasCycle:
      for r in g.rules {
        if g.isLoop(r) {
          errors.insert(EBNFError("Rule '\(description(r))' is part of a cycle", at: location(r)))
        }
      }
    default:
      errors.insert(EBNFError("MARPA error: \(err)", at: sourceGrammar.start.position))
    }
    throw errors
  }

  func location(_ r: Marpa.Rule) -> SourceRegion { bnfizer.output.ruleSource[r]! }

  func name(_ s: Marpa.Symbol) -> String { bnfizer.output.symbolSource[s]!.name }

  func description(_ r: Marpa.Rule) -> String {
    let lhsName = name(g.lhs(r))
    let rhs = g.rhs(r).lazy.map { s in name(s) }.joined(separator: " ")
    return "\(lhsName) -> \(rhs)"
  }

  let literals = Dictionary(
    uniqueKeysWithValues: sourceGrammar.literals().lazy.map { l in
      (l, bnfizer.asBNF(literal: l))
    })

  var tokenPatterns = Dictionary(
    uniqueKeysWithValues: sourceGrammar.regexps().lazy.map { s, pattern in
      (pattern, Optional(bnfizer.asBNF(s)))
    })

  return Parser(
    grammar: g, unrecognizedToken: unrecognizedToken,
    scanner: Scanner(literalStrings: literals, patterns: tokenPatterns),
    symbolName: name,
    ruleLocation: location)
}
