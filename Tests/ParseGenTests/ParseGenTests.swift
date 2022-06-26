import XCTest
import Utils
import Marpa
import CitronLexerModule
import CitronParserModule

@testable import ParseGen

private extension String {
  func asEBNFGrammar() throws -> EBNF.Grammar {
    let ebnfBlocks = self.markdownCodeBlocks(language: "ebnf")
    let parser = EBNFParser()
    for b in ebnfBlocks where !b.isEmpty {
      let startLine = b.first!.0 + 1
      let text = self[b.first!.1.startIndex..<b.last!.1.endIndex]
      for t in EBNF.tokens(in: text, onLine: startLine, fromFile: specPath) {
        try parser.consume(token: t, code: t.id)
      }
    }
    let definitions = try parser.endParsing()
    return try EBNF.Grammar(definitions, start: "module-definition")
  }
}

private struct TestBuilder: BNFBuilder {
  typealias Symbol = Int
  var symbolName: [String] = []
  var symbolLocation: [SourceRegion] = []
  var rules: [(lhs: Symbol, rhs: [Symbol])] = []
  var ruleLocation: [SourceRegion] = []
  var startSymbol: Symbol?

  mutating func makeTerminal<N: EBNFNode>(_ n: N) -> Symbol {
    makeSymbol(n)
  }

  mutating func makeNonterminal<N: EBNFNode>(_ n: N) -> Symbol {
    makeSymbol(n)
  }

  private mutating func makeSymbol<N: EBNFNode>(_ n: N) -> Symbol {
    symbolName.append(n.bnfSymbolName)
    symbolLocation.append(n.position)
    return symbolName.count - 1
  }

  mutating func setStartSymbol(_ s: Symbol) {
    startSymbol = s
  }

  mutating func addRule<RHS: Collection, Source: EBNFNode>(
    reducing rhs: RHS, to lhs: Symbol, source: Source
  )
  where RHS.Element == Symbol
  {
    rules.append((lhs: lhs, rhs: Array(rhs)))
    ruleLocation.append(source.position)
  }
}

final class ParseGenTests: XCTestCase {
  func testReadSpec() throws {
    let specContents = try String(contentsOfFile: specPath, encoding: .utf8)
    let ebnfBlocks = specContents.markdownCodeBlocks(language: "ebnf")
    #if false
    for r in ebnfBlocks {
      for l in r { print(l) }
    }
    #endif
  }

  func testEBNFScanner() throws {
    let specContents = try String(contentsOfFile: specPath, encoding: .utf8)
    let ebnfBlocks = specContents.markdownCodeBlocks(language: "ebnf")
    for b in ebnfBlocks where !b.isEmpty {
      let startLine = b.first!.0 + 1
      let text = specContents[b.first!.1.startIndex..<b.last!.1.endIndex]
      let tokens = EBNF.tokens(in: text, onLine: startLine, fromFile: specPath)
      _ = tokens
    }
  }

  func testBNFConversion() {
    do {
      let specContents = try String(contentsOfFile: specPath, encoding: .utf8)
      let g = try specContents.asEBNFGrammar()
      var conversion = EBNFToBNF(from: g, into: TestBuilder())
      conversion.build()
      #if false
      print(conversion.output.rules)
      #endif
    }
    catch let e as EBNFErrorLog {
      XCTFail("Unexpected error\n\(e.report())")
    }
    catch let e {
      XCTFail("Unexpected error: \(e)")
    }
  }

  func testMARPA() {
    do {
      let specContents = try String(contentsOfFile: specPath, encoding: .utf8)
      let g = try specContents.asEBNFGrammar()

      let valBlocks = specContents.markdownCodeBlocks(language: "val")
      var errors: EBNFErrorLog = []
      for b in valBlocks {
        let valParser = try makeParser(g)
        let blockStart = (line: b.first!.0, column: 0)
        let text = specContents[b.first!.1.startIndex..<b.last!.1.endIndex]
        errors.formUnion(valParser.recognize(text, startingAt: blockStart, inFile: specPath))
      }
      if !errors.isEmpty { throw errors }
    }
    catch let e as EBNFErrorLog {
      XCTFail("Unexpected error\n\(e.report())")
    }
    catch let e {
      XCTFail("Unexpected error: \(e)")
    }
  }
}
