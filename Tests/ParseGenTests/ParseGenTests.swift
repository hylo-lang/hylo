import XCTest
@testable import ParseGen

extension StringProtocol {
  func strippingWhitespace() -> SubSequence {
    var r = self.drop { c in c.isWhitespace }
    while r.last?.isWhitespace ?? false { _ = r.popLast() }
    return r
  }
}

extension Sequence {
  func elementsBetween(
    thoseSatisfying isOpenDelimiter: (Element)->Bool,
    andThoseSatisfying isCloseDelimiter: (Element)->Bool
  ) -> [Element] {
    var r: [Element] = []
    var betweenDelimiters = false
    for x in self {
      if (betweenDelimiters ? isCloseDelimiter : isOpenDelimiter)(x) {
        betweenDelimiters.toggle()
      }
      else if betweenDelimiters {
        r.append(x)
      }
    }
    return r
  }
}


final class ParseGenTests: XCTestCase {

  func testReadSpec() throws {
    let rootRelativeSpecPath
      = (#filePath.split(separator: "/").dropLast(3) + ["spec", "spec.md"]).joined(separator: "/")
    let specContents = try String(contentsOfFile: "/" + rootRelativeSpecPath, encoding: .utf8)
    let allLines = specContents.split { c in c.isNewline }.enumerated()
    let ebnfLines = allLines.elementsBetween(
      thoseSatisfying: { l in l.1.strippingWhitespace() == "```ebnf" },
      andThoseSatisfying: { l in l.1.strippingWhitespace() == "```" }
    )
  }

}
