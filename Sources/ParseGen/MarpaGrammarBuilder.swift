import Marpa
import CitronLexerModule

struct MarpaGrammarBuilder: BNFBuilder {
  typealias Symbol = Marpa.Symbol
  typealias Rule = Marpa.Rule

  /// Mapping from each symbol to the EBNF that generated it
  public private(set) var symbolSource: [Symbol: (name: String, position: SourceRegion)] = [:]
  /// Mapping from each rule to the EBNF that generated it
  public private(set) var ruleSource: [Rule: SourceRegion] = [:]

  public private(set) var result = Marpa.Grammar()

  mutating func makeTerminal<N: EBNFNode>(_ n: N) -> Symbol {
    let r = result.makeTerminal()
    symbolSource[r] = (n.bnfSymbolName, n.position)
    return r
  }

  mutating func makeNonterminal<N: EBNFNode>(_ n: N) -> Symbol {
    let r = result.makeNonterminal()
    symbolSource[r] = (n.bnfSymbolName, n.position)
    return r
  }

  mutating func setStartSymbol(_ s: Symbol) {
    result.startSymbol = s
  }

  mutating func addRule<RHS: Collection, Source: EBNFNode>(
    reducing rhs: RHS, to lhs: Symbol, source: Source) where RHS.Element == Symbol
  {
    let r = result.makeRule(lhs: lhs, rhs: rhs)
    ruleSource[r] = source.position
  }
}
