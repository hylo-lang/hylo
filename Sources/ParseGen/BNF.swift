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
  private let inputNonterminals: Set<EBNF.Grammar.Symbol>

  private struct SymbolKey: Hashable {
    let term: EBNF.Term
    let normalizedSpacer: BNF.Symbol?
    
    init(_ term: EBNF.Term, rawSpacer: BNF.Symbol?) {
      self.term = term
      switch term {
      case .symbol, .regexp, .literal:
        normalizedSpacer = nil
      default:
        normalizedSpacer = rawSpacer
      }
    }
  }
  
  /// Mapping from bits of EBNF AST to BNF symbol.
  private var bnfSymbol_: [SymbolKey: BNF.Symbol] = [:]

  public init(from input: EBNF.Grammar, into output: BNF) {
    (self.input, self.output) = (input, output)
    inputNonterminals = input.nonterminals()
  }

  public func asBNF(_ s: EBNF.Symbol) -> BNF.Symbol {
    bnfSymbol_[SymbolKey(.symbol(s), rawSpacer: nil)]!
  }

  public func asBNF(literal l: String) -> BNF.Symbol {
    bnfSymbol_[SymbolKey(.literal(l, position: .init(.empty)), rawSpacer: nil)]!
  }

  private mutating func demandSymbol(_ s: EBNF.Symbol) -> BNF.Symbol {
    demandBNFSymbol(.symbol(s), interjecting: nil)
  }

  mutating func build() {
    let horizontalSpaceOpt = demandSymbol(input.horizontalSpaceOpt)
    let whitespaceOpt = demandSymbol(input.whitespaceOpt)

    for d in input.definitions {
      if inputNonterminals.contains(d.lhs) {
        for a in d.alternatives {
          let spacer = d.kind == .noWhitespace ? nil : d.kind == .noNewline ? horizontalSpaceOpt
          : whitespaceOpt
          buildRule(
            reducing: a, to: demandBNFSymbol(.symbol(d.lhs), interjecting: spacer),
            interjecting: spacer, source: a)
        }
      }
    }
    output.setStartSymbol(demandSymbol(input.start))
  }

  mutating func buildRule<RHS: Collection, Source: EBNFNode>(
    reducing rhs: RHS, to lhs: BNF.Symbol, interjecting spacer: BNF.Symbol?,
    source: Source
  ) where RHS.Element == EBNF.Term {
    buildRule(
      reducingBNF: rhs.map { t in demandBNFSymbol(t, interjecting: spacer) },
      to: lhs, interjecting: spacer, source: source)
  }

  mutating func buildRule<RHS: Collection, Source: EBNFNode>(
    reducingBNF rhs: RHS, to lhs: BNF.Symbol, interjecting spacer: BNF.Symbol?,
    source: Source
  ) where RHS.Element == BNF.Symbol {
    let rhs1 = spacer.map { s in rhs.interjecting(s) } ?? Array(rhs)
    output.addRule(reducing: rhs1, to: lhs, source: source)
  }

  mutating func demandBNFSymbol(_ t: EBNF.Term, interjecting spacer: BNF.Symbol?) -> BNF.Symbol {
    let k = SymbolKey(t, rawSpacer: spacer)
    if let r = bnfSymbol_[k] {
      return r
    }
    let lhs: BNF.Symbol
    defer { bnfSymbol_[k] = lhs }

    switch t {
    case .group(let alternatives):
      lhs = output.makeNonterminal(alternatives)
      for rhs in alternatives {
        buildRule(reducing: rhs, to: lhs, interjecting: spacer, source: t)
      }
    case .symbol(let s):
      lhs = inputNonterminals.contains(s) ? output.makeNonterminal(s) : output.makeTerminal(s)
    case .literal, .regexp:
      lhs = output.makeTerminal(t)
    case .quantified(let t1, let q, _):
      lhs = output.makeNonterminal(t)
      if q == "*" || q == "?" {
        buildRule(reducingBNF: EmptyCollection(), to: lhs, interjecting: spacer, source: t)
      }
      if q == "+" || q == "?" {
        buildRule(reducing: CollectionOfOne(t1), to: lhs, interjecting: spacer, source: t)
      }
      if q == "*" || q == "+" {
        let t2 = demandBNFSymbol(t1, interjecting: spacer)
        buildRule(reducingBNF: [lhs, t2], to: lhs, interjecting: spacer, source: t)
      }
    }
    return lhs
  }
}
