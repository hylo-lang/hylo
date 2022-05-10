import XCTest
import LoftDataStructures_Zip2Collection
@testable import ParseGen

extension StringProtocol {
  func strippingWhitespace() -> SubSequence {
    var r = self.drop { c in c.isWhitespace }
    while r.last?.isWhitespace ?? false { _ = r.popLast() }
    return r
  }
}

extension Swift.Collection {
  /// Returns the `SubSequence`s between pairs of elements bounded by an element
  /// satisfying `isOpenDelimiter` as an open deliminter and `isCloseDelimiter`
  /// as a closing delimiter.
  ///
  /// - Precondition: elements satisfying the two predicates strictly alternate,
  ///   starting with `isOpenDelimiter`.
  func subSequencesBetween(
    elementsSatisfying isOpenDelimiter: (Element)->Bool,
    and isCloseDelimiter: (Element)->Bool
  ) -> [SubSequence] {
    var r: [SubSequence] = []
    var openDelimiterPosition: Index? = nil
    for i in self.indices {
      if let o = openDelimiterPosition {
        if isCloseDelimiter(self[i]) {
          r.append(self[index(after: o)..<i])
          openDelimiterPosition = nil
        }
      }
      else {
        if isOpenDelimiter(self[i]) {
          openDelimiterPosition = i
        }
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
    let ebnfLines = allLines.subSequencesBetween(
      elementsSatisfying: { l in l.1.strippingWhitespace() == "```ebnf" },
      and: { l in l.1.strippingWhitespace() == "```" }
    )
    #if false
    for r in ebnfLines {
      for l in r { print(l) }
    }
    #endif
  }

}
