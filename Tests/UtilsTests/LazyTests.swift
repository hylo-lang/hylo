import Utils
import XCTest

final class LazyTests: XCTestCase {

  func testComputedOnceNoCopy() {
    var counter = 0
    let l = Lazy { readAndIncrement(&counter) }

    XCTAssertEqual(l[], 0)
    XCTAssertEqual(counter, 1)
    XCTAssertEqual(l[], 0)
    XCTAssertEqual(counter, 1)
  }

  func testComputedOnceWithCopy() {
    var counter = 0
    let l = Lazy { readAndIncrement(&counter) }
    let l1 = l
    XCTAssertEqual(l[], 0)
    XCTAssertEqual(counter, 1)
    XCTAssertEqual(l1[], 0)
    XCTAssertEqual(counter, 1)
    XCTAssertEqual(l[], 0)
    XCTAssertEqual(counter, 1)
  }

  func testComputedOnceWithPostCallCopy() {
    var counter = 0
    let l = Lazy { readAndIncrement(&counter) }
    XCTAssertEqual(l[], 0)
    XCTAssertEqual(counter, 1)
    let l1 = l
    XCTAssertEqual(l1[], 0)
    XCTAssertEqual(counter, 1)
    XCTAssertEqual(l[], 0)
    XCTAssertEqual(counter, 1)
  }

}

final class LazyThrowingTests: XCTestCase {

  func testComputedOnceNoCopyNoThrow() throws {
    var counter = 0
    let l = LazyThrowing { readAndIncrement(&counter) }

    XCTAssertEqual(try l[], 0)
    XCTAssertEqual(counter, 1)
    XCTAssertEqual(try l[], 0)
    XCTAssertEqual(counter, 1)
  }

  func testComputedOnceWithCopyNoThrow() throws {
    var counter = 0
    let l = LazyThrowing { readAndIncrement(&counter) }
    let l1 = l
    XCTAssertEqual(try l[], 0)
    XCTAssertEqual(counter, 1)
    XCTAssertEqual(try l1[], 0)
    XCTAssertEqual(counter, 1)
    XCTAssertEqual(try l[], 0)
    XCTAssertEqual(counter, 1)
  }

  func testComputedOnceWithPostCallCopyNoThrow() throws {
    var counter = 0
    let l = LazyThrowing { readAndIncrement(&counter) }
    XCTAssertEqual(try l[], 0)
    XCTAssertEqual(counter, 1)
    let l1 = l
    XCTAssertEqual(try l1[], 0)
    XCTAssertEqual(counter, 1)
    XCTAssertEqual(try l[], 0)
    XCTAssertEqual(counter, 1)
  }

  struct X: Error {}

  func testComputedOnceNoCopyWithThrow() throws {
    var counter = 0
    let l = LazyThrowing<Int> {
      counter += 1
      throw X()
    }

    do {
      _ = try l[]
      XCTFail()
    } catch is X {}

    XCTAssertEqual(counter, 1)
    do {
      _ = try l[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter, 1)
  }

  func testComputedOnceWithCopyWithThrow() throws {
    var counter = 0
    let l = LazyThrowing<Int> {
      counter += 1
      throw X()
    }
    let l1 = l

    do {
      _ = try l[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter, 1)

    do {
      _ = try l1[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter, 1)

    do {
      _ = try l[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter, 1)
  }

  func testComputedOnceWithPostCallCopyWithThrow() throws {
    var counter = 0
    let l = LazyThrowing<Int> {
      counter += 1
      throw X()
    }

    do {
      _ = try l[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter, 1)

    let l1 = l
    do {
      _ = try l1[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter, 1)

    do {
      _ = try l[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter, 1)
  }

}

/// Returns the current value of `counter` and increments it.
private func readAndIncrement(_ counter: inout Int) -> Int {
  defer { counter += 1 }
  return counter
}
