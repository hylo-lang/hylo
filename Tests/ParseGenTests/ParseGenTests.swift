import XCTest
import Utils
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
      let startLine = b.first!.0
      let text = specContents[b.first!.1.startIndex..<b.last!.1.endIndex]
//      print("at:", startLine)
//      print(text)
      let tokens = EBNF.tokens(in: text, onLine: startLine, fromFile: specPath)
//      print("------------------------------------")
//      for t in tokens { print(t) }
      _ = tokens
    }
  }

  func testEBNFParser() throws {
    let specContents = try String(contentsOfFile: specPath, encoding: .utf8)
    let ebnfBlocks = specContents.markdownCodeBlocks(language: "ebnf")
    for b in ebnfBlocks where !b.isEmpty {
      let startLine = b.first!.0
      let text = specContents[b.first!.1.startIndex..<b.last!.1.endIndex]
      print("------------------------------------")
      print(text)
      let parser = EBNFParser()
      for t in EBNF.tokens(in: text, onLine: startLine, fromFile: specPath) {
        try parser.consume(token: t, code: t.id)
      }
      print(try parser.endParsing().dump)
    }
  }
}
