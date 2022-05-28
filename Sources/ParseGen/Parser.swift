import Marpa
import CitronLexerModule

struct Parser {
  let grammar: Marpa.Grammar
  let unrecognizedToken: Marpa.Symbol
  let scanner: Scanner<Marpa.Symbol>
  let symbolName: [Marpa.Symbol: String]
  let ruleLocation: [Marpa.Rule: SourceRegion]

  func dumpGrammar() {
    for r in grammar.rules {
      print("\(ruleLocation[r]!): note:", description(r))
    }
  }

  func description(_ r: Marpa.Rule) -> String {
    let lhsName = symbolName[grammar.lhs(r)]!
    let rhs = grammar.rhs(r).lazy.map { s in symbolName[s]! }.joined(separator: " ")
    return "\(lhsName) -> \(rhs)"
  }
}
