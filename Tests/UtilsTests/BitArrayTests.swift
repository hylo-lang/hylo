import Utils
import XCTest

final class BitArrayTests: XCTestCase {

  func testInit() {
    let empty = BitArray()
    XCTAssert(empty.isEmpty)
    XCTAssertEqual(empty.count, 0)
  }

  func testInitRepeating() {
    let a0 = BitArray(repeating: false, count: 100)
    XCTAssertEqual(a0.count, 100)
    XCTAssertFalse(a0[0])
    XCTAssertFalse(a0[1])
    XCTAssertFalse(a0[64])
    XCTAssertFalse(a0[65])

    let a1 = BitArray(repeating: true, count: 100)
    XCTAssertEqual(a1.count, 100)
    XCTAssert(a1[0])
    XCTAssert(a1[1])
    XCTAssert(a1[64])
    XCTAssert(a1[65])
  }

  func testInitWithSequence() {
    let a0 = BitArray([true, false, true, false])
    XCTAssertEqual(a0.count, 4)
    XCTAssert(a0[0])
    XCTAssertFalse(a0[1])
    XCTAssert(a0[2])
    XCTAssertFalse(a0[3])
  }

  func testAllTrue() {
    var a0 = BitArray()
    XCTAssert(a0.allTrue)
    a0.append(true)
    XCTAssert(a0.allTrue)
    a0.append(false)
    XCTAssertFalse(a0.allTrue)

    var a1 = BitArray(repeating: true, count: 100)
    XCTAssert(a1.allTrue)
    a1.append(false)
    XCTAssertFalse(a1.allTrue)
  }

  func testAllFalse() {
    var a0 = BitArray()
    XCTAssert(a0.allFalse)
    a0.append(false)
    XCTAssert(a0.allFalse)
    a0.append(true)
    XCTAssertFalse(a0.allFalse)

    var a1 = BitArray(repeating: false, count: 100)
    XCTAssert(a1.allFalse)
    a1.append(true)
    XCTAssertFalse(a1.allFalse)
  }

  func testReserveCapacity() {
    let a0 = BitArray((0 ..< 100).map({ $0 % 2 == 0 }))
    var a1 = a0
    a1.reserveCapacity(1000)
    XCTAssert(a1.capacity >= 1000)
    XCTAssertEqual(a0, a1)
  }

  func testAppend() {
    var a0 = BitArray()
    a0.append(true)
    XCTAssertEqual(a0.count, 1)
    XCTAssert(a0[0])
    a0.append(false)
    XCTAssertEqual(a0.count, 2)
    XCTAssert(a0[0])
    XCTAssertFalse(a0[1])

    var a1 = BitArray(repeating: false, count: UInt.bitWidth)
    a1.append(true)
    XCTAssertEqual(a1.count, UInt.bitWidth + 1)
    XCTAssert(a1[UInt.bitWidth])
    a1.append(false)
    XCTAssertEqual(a1.count, UInt.bitWidth + 2)
    XCTAssert(a1[UInt.bitWidth])
    XCTAssertFalse(a1[UInt.bitWidth + 1])
  }

  func testPopLast() {
    var a0 = BitArray()
    XCTAssertNil(a0.popLast())
    a0.append(true)
    XCTAssertEqual(a0.popLast(), true)
    XCTAssert(a0.isEmpty)

    var a1 = BitArray(repeating: true, count: UInt.bitWidth)
    XCTAssertEqual(a1.popLast(), true)
    XCTAssertEqual(a1.count, UInt.bitWidth - 1)
  }

  func testRemoveLast() {
    var a0 = BitArray([true])
    XCTAssert(a0.removeLast())
    XCTAssert(a0.isEmpty)

    var a1 = BitArray(repeating: true, count: UInt.bitWidth)
    XCTAssert(a1.removeLast())
    XCTAssertEqual(a1.count, UInt.bitWidth - 1)
  }

  func testRemoveAll() {
    var a0 = BitArray(repeating: true, count: 100)
    a0.removeAll()
    XCTAssert(a0.isEmpty)

    var a1 = BitArray(repeating: true, count: 100)
    let k = a1.capacity
    a1.removeAll(keepingCapacity: true)
    XCTAssert(a1.isEmpty)
    XCTAssertEqual(a1.capacity, k)
  }

  func testBitwiseOr() {
    let a0 = BitArray((0 ..< 100).map({ $0 % 2 == 0 }))
    let b0 = BitArray(repeating: false, count: 100)
    XCTAssertEqual(a0 | b0, a0)
  }

  func testBitwiseAnd() {
    let a0 = BitArray((0 ..< 100).map({ $0 % 2 == 0 }))
    let b0 = BitArray(repeating: true, count: 100)
    XCTAssertEqual(a0 & b0, a0)
  }

  func testBitwiseXor() {
    let a0 = BitArray((0 ..< 100).map({ $0 % 2 == 0 }))
    let b0 = BitArray(repeating: true, count: 100)
    XCTAssertEqual(a0 ^ b0, BitArray((0 ..< 100).map({ $0 % 2 != 0 })))
  }

  func testEquatable() {
    let a0 = BitArray((0 ..< 100).map({ $0 % 2 == 0 }))
    var a1 = a0
    a1.reserveCapacity(1000)
    XCTAssertEqual(a0, a1)

    var a2 = a0
    a2.append(true)
    XCTAssertNotEqual(a0, a2)
  }

  func testHashable() {
    let a0 = BitArray((0 ..< 100).map({ $0 % 2 == 0 }))
    var a1 = a0
    a1.reserveCapacity(1000)
    XCTAssertEqual(a0.hashValue, a1.hashValue)
  }

  func testCollection() {
    let a0 = BitArray((0 ..< 100).map({ $0 % 2 == 0 }))
    XCTAssertEqual(Array(a0), (0 ..< 100).map({ $0 % 2 == 0 }))
  }

  func testPositionToOffset() {
    let a0 = BitArray((0 ..< 100).map({ $0 % 2 == 0 }))
    let p = a0.index(a0.startIndex, offsetBy: 80)
    XCTAssertEqual(a0.offset(of: p), 80)
  }

  func testOffsetToPosition() {
    let a0 = BitArray((0 ..< 100).map({ $0 % 2 == 0 }))
    let p = a0.index(a0.startIndex, offsetBy: 80)
    XCTAssertEqual(a0.index(at: 80), p)
  }

  func testDescription() {
    let a0 = BitArray([true, false, true])
    XCTAssertEqual(a0.description, "101")
  }

}
