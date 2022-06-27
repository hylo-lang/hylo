import CitronLexerModule
import Utils

extension Collection {
  var onlyElement: Element? {
    return first.map { x in dropFirst().isEmpty ? x : nil } ?? nil
  }
}

protocol BNFBuilder {
  associatedtype Symbol: Hashable
  mutating func makeTerminal<N: EBNFNode>(_ n: N) -> Symbol
  mutating func makeNonterminal<N: EBNFNode>(_ n: N) -> Symbol
  mutating func setStartSymbol(_: Symbol)
  mutating func addRule<RHS: Collection, Source: EBNFNode>(
    reducing rhs: RHS, to lhs: Symbol, source: Source) where RHS.Element == Symbol
}

internal struct EBNFToBNF<BNF: BNFBuilder> {

  public private(set) var output: BNF
  private let input: EBNF.Grammar

  /// The inputs's nonterminal symbols
  private let inputNonterminals: Set<EBNF.Symbol>

  /// Mapping from pieces of EBNF AST to BNF symbol.
  private var bnfSymbol: [EBNF.Term: BNF.Symbol] = [:]

  public init(from input: EBNF.Grammar, into output: BNF) {
    (self.input, self.output) = (input, output)
    inputNonterminals = input.nonterminals()
  }

  public func asBNF(_ s: EBNF.Symbol) -> BNF.Symbol {
    bnfSymbol[.symbol(s)]!
  }

  public func asBNF(literal l: String) -> BNF.Symbol {
    bnfSymbol[.literal(l, position: .init(.empty))]!
  }

  private mutating func demandSymbol(_ s: EBNF.Symbol) -> BNF.Symbol {
    demandBNFSymbol(.symbol(s))
  }

  mutating func build() {
    for d in input.definitions {
      if inputNonterminals.contains(d.lhs) {
        for a in d.alternatives {
          buildRule(reducing: a, to: demandBNFSymbol(.symbol(d.lhs)), source: a)
        }
      }
    }
    output.setStartSymbol(demandSymbol(input.start))
  }

  mutating func buildRule<RHS: Collection, Source: EBNFNode>(
    reducing rhs: RHS, to lhs: BNF.Symbol, source: Source
  ) where RHS.Element == EBNF.Term {
    buildRule(reducingBNF: rhs.map { t in demandBNFSymbol(t) }, to: lhs, source: source)
  }

  mutating func buildRule<RHS: Collection, Source: EBNFNode>(
    reducingBNF rhs: RHS, to lhs: BNF.Symbol, source: Source
  ) where RHS.Element == BNF.Symbol {
    output.addRule(reducing: rhs, to: lhs, source: source)
  }

  mutating func demandBNFSymbol(_ t: EBNF.Term) -> BNF.Symbol {
    if let r = bnfSymbol[t] {
      return r
    }
    let lhs: BNF.Symbol
    defer { bnfSymbol[t] = lhs }

    switch t {
    case .group(let alternatives):
      lhs = output.makeNonterminal(alternatives)
      for rhs in alternatives {
        buildRule(reducing: rhs, to: lhs, source: t)
      }
    case .symbol(let s):
      lhs = inputNonterminals.contains(s) ? output.makeNonterminal(s) : output.makeTerminal(s)
    case .literal, .regexp:
      lhs = output.makeTerminal(t)
    case .quantified(let t1, let q, _):
      lhs = output.makeNonterminal(t)
      if q == "*" || q == "?" {
        buildRule(reducingBNF: EmptyCollection(), to: lhs, source: t)
      }
      if q == "+" || q == "?" {
        buildRule(reducing: CollectionOfOne(t1), to: lhs, source: t)
      }
      if q == "*" || q == "+" {
        let t2 = demandBNFSymbol(t1)
        buildRule(reducingBNF: [lhs, t2], to: lhs, source: t)
      }
    }
    return lhs
  }
}
