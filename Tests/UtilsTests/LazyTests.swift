import Utils
import XCTest

final class LazyTests: XCTestCase {

  func testComputedOnceNoCopy() {
    let counter = SharedMutable(0)
    let l = Lazy { counter.modify { readAndIncrement(&$0) } }

    XCTAssertEqual(l[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
    XCTAssertEqual(l[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
  }

  func testComputedOnceWithCopy() {
    let counter = SharedMutable(0)
    let l = Lazy { counter.modify { readAndIncrement(&$0) } }
    let l1 = l
    XCTAssertEqual(l[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
    XCTAssertEqual(l1[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
    XCTAssertEqual(l[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
  }

  func testComputedOnceWithPostCallCopy() {
    let counter = SharedMutable(0)
    let l = Lazy { counter.modify { readAndIncrement(&$0) } }
    XCTAssertEqual(l[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
    let l1 = l
    XCTAssertEqual(l1[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
    XCTAssertEqual(l[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
  }

}

final class LazyThrowingTests: XCTestCase {

  func testComputedOnceNoCopyNoThrow() throws {
    let counter = SharedMutable(0)
    let l = LazyThrowing { counter.modify { readAndIncrement(&$0) } }

    XCTAssertEqual(try l[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
    XCTAssertEqual(try l[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
  }

  func testComputedOnceWithCopyNoThrow() throws {
    let counter = SharedMutable(0)
    let l = LazyThrowing { counter.modify { readAndIncrement(&$0) } }
    let l1 = l
    XCTAssertEqual(try l[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
    XCTAssertEqual(try l1[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
    XCTAssertEqual(try l[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
  }

  func testComputedOnceWithPostCallCopyNoThrow() throws {
    let counter = SharedMutable(0)
    let l = LazyThrowing { counter.modify { readAndIncrement(&$0) } }
    XCTAssertEqual(try l[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
    let l1 = l
    XCTAssertEqual(try l1[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
    XCTAssertEqual(try l[], 0)
    XCTAssertEqual(counter.read {$0}, 1)
  }

  struct X: Error, Sendable {}

  func testComputedOnceNoCopyWithThrow() throws {
    let counter = SharedMutable(0)
    let l = LazyThrowing<Int> {
      counter.modify{ $0 += 1 }
      throw X()
    }

    do {
      _ = try l[]
      XCTFail()
    } catch is X {}

    XCTAssertEqual(counter.read {$0}, 1)
    do {
      _ = try l[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter.read {$0}, 1)
  }

  func testComputedOnceWithCopyWithThrow() throws {
    let counter = SharedMutable(0)
    let l = LazyThrowing<Int> {
      counter.modify{ $0 += 1 }
      throw X()
    }
    let l1 = l

    do {
      _ = try l[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter.read {$0}, 1)

    do {
      _ = try l1[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter.read {$0}, 1)

    do {
      _ = try l[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter.read {$0}, 1)
  }

  func testComputedOnceWithPostCallCopyWithThrow() throws {
    let counter = SharedMutable(0)
    let l = LazyThrowing<Int> {
      counter.modify{ $0 += 1 }
      throw X()
    }

    do {
      _ = try l[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter.read {$0}, 1)

    let l1 = l
    do {
      _ = try l1[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter.read {$0}, 1)

    do {
      _ = try l[]
      XCTFail()
    } catch is X {}
    XCTAssertEqual(counter.read {$0}, 1)
  }

}

/// Returns the current value of `counter` and increments it.
private func readAndIncrement(_ counter: inout Int) -> Int {
  defer { counter += 1 }
  return counter
}
