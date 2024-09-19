import Utils
import XCTest

final class TrieTests: XCTestCase {

  func testIsEmpty() {
    var s = Trie<String, Int>()
    XCTAssert(s.isEmpty)

    s["abc"] = 1
    s["ab"] = 2
    XCTAssertFalse(s.isEmpty)

    s["abc"] = nil
    s["ab"] = nil
    XCTAssert(s.isEmpty)

    s["ab"] = 3
    XCTAssertFalse(s.isEmpty)
  }

  func testCount() {
    var s = Trie<String, Int>()
    XCTAssertEqual(s.count, 0)

    s["abc"] = 1
    s["ab"] = 2
    XCTAssertEqual(s.count, 2)

    s["abx"] = 3
    XCTAssertEqual(s.count, 3)

    s["abc"] = nil
    s["abx"] = 4
    XCTAssertEqual(s.count, 2)
  }

  func testElements() {
    var s = Trie<String, Int>()
    XCTAssert(s.elements.isEmpty)

    s["ab"] = 0
    s["abc"] = 1
    s["abC"] = 2
    s["xyz"] = 3
    XCTAssertEqual(
      Array(s.elements.sorted(by: \.value).map({ (k, _) in String(k) })),
      ["ab", "abc", "abC", "xyz"])

    s["abC"] = nil
    s["xyZ"] = 4
    XCTAssertEqual(
      Array(s.elements.sorted(by: \.value).map({ (k, _) in String(k) })),
      ["ab", "abc", "xyz", "xyZ"])
  }

  func testSubtrieElements() throws {
    var s = Trie<String, Int>()

    s["ab"] = 0
    s["abc"] = 1
    s["abC"] = 2
    s["xyz"] = 3
    let t = try XCTUnwrap(s[prefix: "ab"])
    XCTAssertEqual(
      Array(t.elements.sorted(by: \.value).map({ (k, _) in String(k) })),
      ["", "c", "C"])
  }

  func testElementsIndexComparison() {
    var s = Trie<String, Int>()
    s["abc"] = 1
    s["abC"] = 2
    let a = s.elements.startIndex
    XCTAssertLessThan(a, s.elements.index(after: a))
    XCTAssertLessThan(a, s.elements.endIndex)
    XCTAssertGreaterThan(s.elements.endIndex, a)
  }

  func testPrefix() throws {
    var s = Trie<[Int], Int>()
    XCTAssertNil(s[prefix: [1]])

    s[[1, 2, 3]] = 1
    s[[1, 2, 5]] = 2
    s[[7, 8, 9]] = 3
    let t = try XCTUnwrap(s[prefix: [1, 2]])
    XCTAssertEqual(t.count, 2)
  }

  func testLongestPrefix() {
    var s = Trie<[Int], Int>()

    s[[1, 2, 3]] = 1
    s[[1, 2, 5]] = 2
    s[[7, 8, 9]] = 3
    let (t, i) = s.longestPrefix(startingWith: [1, 2, 0])
    XCTAssertEqual(i, 2)
    XCTAssertEqual(t.count, 2)
  }

  func testSubscript() {
    var s = Trie<String, Int>()
    
    // Read
    XCTAssertNil(s["x"])
    
    // Write
    s["abc"] = 1
    s["abcd"] = 2
    s["abcde"] = 3
    XCTAssertEqual(s["abc"], 1)

    // Modify
    s["abcde"] = nil
    s["abcdE"] = 3
    XCTAssertNil(s["abcde"])
    XCTAssertEqual(s["abcdE"], 3)
  }

  func testSubtrieSubscript() throws {
    var s = Trie<String, Int>()
    s["abc"] = 1
    s["abcd"] = 2
    s["abcde"] = 3
    let t = try XCTUnwrap(s[prefix: []])
    XCTAssertNil(t["xyz"])
    XCTAssertEqual(t["abc"], 1)
  }

  func testEquatable() {
    var s = Trie<String, Int>()
    s["abc"] = 1
    s["ab"] = 2
    
    var t = s
    XCTAssertEqual(s, t)
    t["abc"] = nil
    XCTAssertNotEqual(s, t)
    t["abc"] = 1
    XCTAssertEqual(s, t)
  }

  func testHashable() {
    var s = Trie<String, Int>()
    s["abc"] = 1
    s["ab"] = 2

    var h1 = Hasher()
    var h2 = Hasher()
    s.hash(into: &h1)
    s.hash(into: &h2)
    XCTAssertEqual(h1.finalize(), h2.finalize())

    var t = s
    t["abc"] = nil
    
    h1 = Hasher()
    h2 = Hasher()
    s.hash(into: &h1)
    t.hash(into: &h2)
    XCTAssertNotEqual(h1.finalize(), h2.finalize())

    t["abc"] = 1
    h1 = Hasher()
    h2 = Hasher()
    s.hash(into: &h1)
    t.hash(into: &h2)
    XCTAssertEqual(h1.finalize(), h2.finalize())
  }

}
