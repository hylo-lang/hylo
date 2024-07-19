import Utils
import XCTest

@testable import FrontEnd

final class RewritingSystemTests: XCTestCase {

  func testInsert() {
    var s = RewritingSystem<Term>()
    let r1 = s.insert(.init("abcd", "ab"))
    XCTAssert(r1.inserted)
    XCTAssertEqual(s.rules[r1.ruleAfterInsertion], .init("abcd", "ab"))

    // Insertion is idempotent.
    let r2 = s.insert(.init("abcd", "ab"))
    XCTAssertFalse(r2.inserted)
    XCTAssertEqual(r2.ruleAfterInsertion, r1.ruleAfterInsertion)

    // Inserting a "less direct" rule inserts an intermediate step.
    let r3 = s.insert(.init("abcd", "abc"))
    XCTAssert(r3.inserted)
    XCTAssertEqual(s.rules[r3.ruleAfterInsertion], .init("abc", "ab"))

    // Inserting a "more direct" rule triggers right simplification.
    let r4 = s.insert(.init("abcd", "a"))
    XCTAssert(r4.inserted)
    XCTAssert(s.rules[r1.ruleAfterInsertion].flags.contains(.isRightSimplified))
  }

  func testReduce() {
    var s = RewritingSystem<Term>()
    s.insert(.init("ab", "a"))
    s.insert(.init("cd", "ab"))
    XCTAssertEqual(s.reduce("abcd"), "aa")
  }

  func testCompletion() {
    var s = RewritingSystem<Term>()
    s.insert(.init("12", "1"))
    s.insert(.init("21", "2"))

    // Knuth-Bendix succeeds.
    XCTAssert(s.complete(orderingTermsWith: StrictOrdering.init))
    XCTAssert(s.rules.contains(.init("11", "1")))
    XCTAssert(s.rules.contains(.init("22", "2")))
  }

  func testNonTerminatingCompletion() {
    var s = RewritingSystem<Term>()
    s.insert(.init("12", "1"))
    s.insert(.init("21", "2"))
    s.insert(.init("31", "13"))

    // Knuth-Bendix fails.
    XCTAssertFalse(s.complete(orderingTermsWith: StrictOrdering.init))
  }

  func testLeftSimplification() {
    var s = RewritingSystem<Term>()
    let (_, r1) = s.insert(.init("abcabc", "abc"))
    s.insert(.init("c", "b"))
    let (_, r2) = s.insert(.init("abc", "a"))
    XCTAssert(s.complete(orderingTermsWith: StrictOrdering.init))
    XCTAssert(s.rules[r1].flags.contains(.isLeftSimplified))
    XCTAssert(s.rules[r2].flags.contains(.isLeftSimplified))
  }

}

/// A term made of UTF8 code units.
struct Term: Hashable {

  /// The symbols in the term.
  let symbols: [UTF8.CodeUnit]

  /// Creates an instance with the value of `s`.
  init(_ s: String) {
    symbols = .init(s.utf8)
  }

}

extension Term: ExpressibleByStringLiteral {

  init(stringLiteral s: String) {
    symbols = .init(s.utf8)
  }

}

extension Term: Comparable {

  static func < (lhs: Term, rhs: Term) -> Bool {
    lhs.symbols.lexicographicallyPrecedes(rhs.symbols)
  }

}

extension Term: RewritingTerm {

  init<S: Sequence<UTF8.CodeUnit>>(_ s: S) {
    symbols = .init(s)
  }

  func substituting(_ s: Self, for t: Self) -> Self {
    .init(symbols.replacing(s.symbols, with: t.symbols))
  }

  static func + (u: Self, v: Self) -> Self {
    .init(u.symbols + v.symbols)
  }

  static func + <S: Sequence<Element>>(u: Self, v: S) -> Self {
    .init(u.symbols + v)
  }

  static func + <S: Sequence<Element>>(u: S, v: Self) -> Self {
    .init(u + v.symbols)
  }

}

extension Term: Collection {

  typealias Element = UTF8.CodeUnit

  typealias Index = Int

  var startIndex: Int { 0 }

  var endIndex: Int { symbols.count }

  func index(after i: Int) -> Int { i + 1 }

  subscript(p: Int) -> UTF8.CodeUnit { symbols[p] }

}

extension Term: CustomStringConvertible {
  var description: String { String(bytes: symbols, encoding: .utf8)! }
}

extension RewritingSystem where Term: Comparable {

  /// Inserts the given rule.
  @discardableResult
  mutating func insert(_ r: RewritingRule<Term>) -> (inserted: Bool, ruleAfterInsertion: RuleID) {
    insert(r, orderingTermsWith: StrictOrdering.init)
  }

}
