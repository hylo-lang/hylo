import XCTest
import Utils
import Marpa

@testable import ParseGen


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
//      print("at:", startLine)
//      print(text)
      let tokens = EBNF.tokens(in: text, onLine: startLine, fromFile: specPath)
//      print("------------------------------------")
//      for t in tokens { print(t) }
      _ = tokens
    }
  }

  func testEBNFParser() {
    do {
      let specContents = try String(contentsOfFile: specPath, encoding: .utf8)

      let ebnfBlocks = specContents.markdownCodeBlocks(language: "ebnf")
      let parser = EBNFParser()
      for b in ebnfBlocks where !b.isEmpty {
        let startLine = b.first!.0 + 1
        let text = specContents[b.first!.1.startIndex..<b.last!.1.endIndex]
        for t in EBNF.tokens(in: text, onLine: startLine, fromFile: specPath) {
          try parser.consume(token: t, code: t.id)
        }
      }
      let definitions = try parser.endParsing()
      let g = try EBNF.Grammar(definitions, start: "module-definition")
      // print("literals:", g.literals())
      let r = g.regexps()
      // print("regexps:")
      // for (k, v) in r {
      //   print("  \(k) ::= /\(v)/")
      // }
      let n = g.nonterminals()
      XCTAssert(n.isDisjoint(with: r.keys))
      let valParser = try makeParser(g)
      //print("-----------------------------")
      //valParser.dumpGrammar()
      //print("-----------------------------")
      let valBlocks = specContents.markdownCodeBlocks(language: "val")
      var errors: EBNFErrorLog = []

      for b in valBlocks {
        let blockStart = (line: b.first!.0, column: 0)
        let text = specContents[b.first!.1.startIndex..<b.last!.1.endIndex]
        let tokens = valParser.scanner.tokens(
          in: String(text), fromFile: specPath, unrecognizedToken: valParser.unrecognizedToken)
        let r = Marpa.Recognizer(valParser.grammar)
        r.startInput()
        var earleySets: [EarleySet] = []

        for (t, s, position) in tokens {
          earleySets.append(r.latestEarleySet)
          let specPosition = position + blockStart
          if let err = r.read(t) {
            var progressReport: [EBNFError.Note] = []

            for (e, (t, s, position)) in zip(earleySets, tokens) {

              let xx = "-------------------"
              progressReport.append(
                EBNFError.Note(
                  message: "\(xx) token \(e.id): '\(s)' (\(valParser.symbolName[t]!)) \(xx)",
                  site: position + blockStart))

              for (rule, origin, n) in r.progress(at: e) {
                let ruleDescription = valParser.description(rule, dotPosition: n < 0 ? nil : n)
                progressReport.append(
                  EBNFError.Note(
                    message: "\(ruleDescription) (\(origin.id))",
                    site: valParser.ruleLocation[rule]!))
              }
            }
            switch err {
            case .unexpectedToken:
              errors.insert(
                EBNFError(
                  "\(err) \(valParser.symbolName[t]!): '\(s)'",
                  at: specPosition,
                  notes: [
                    EBNFError.Note(
                      message: "expected one of: "
                        + r.expectedTerminals.lazy.map { t in valParser.symbolName[t]! }
                        .joined(separator: ", "),
                      site: specPosition)] + progressReport
                ))

            default:
              errors.insert(EBNFError("\(err)", at: specPosition, notes: progressReport))
            }
            break
          }
          r.advanceEarleme()
        }
      }
      if !errors.isEmpty { throw errors }
    }
    catch let e as EBNFErrorLog {
      XCTFail("Unexpected error\n\(e.report())")
    }
    catch let e {
      XCTFail("Unexpected error\n\(e)")
    }
  }
}
