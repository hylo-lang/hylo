import XCTest
import Utils
@testable import ParseGen

func unbridge(_ s: UnsafePointer<UInt8>) -> String {
  String(cString: s)
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
//      print("at:", startLine)
//      print(text)
      let tokens = EBNF.tokens(in: text, onLine: startLine, fromFile: specPath)
//      print("------------------------------------")
//      for t in tokens { print(t) }
      _ = tokens
    }
  }

  func testEBNFParser() throws {
    let specContents = try unbridge(String(contentsOfFile: specPath, encoding: .utf8))
    // Horrible no good very bad workaround for https://github.com/apple/swift/issues/59066.
//    let specContents = String(decoding: specContents_.utf8, as: UTF8.self)

    let ebnfBlocks = specContents.markdownCodeBlocks(language: "ebnf")
    let parser = EBNFParser()
    for b in ebnfBlocks where !b.isEmpty {
      let startLine = b.first!.0 + 1
      let text = specContents[b.first!.1.startIndex..<b.last!.1.endIndex]
      // print("------------------------------------")
      // print(text)
      for t in EBNF.tokens(in: text, onLine: startLine, fromFile: specPath) {
        try parser.consume(token: t, code: t.id)
      }
    }
    let definitions = try parser.endParsing()
    print(definitions.dump)
    let g = try EBNF.Grammar(definitions, start: "module-definition")
    print(g)
  }
}
