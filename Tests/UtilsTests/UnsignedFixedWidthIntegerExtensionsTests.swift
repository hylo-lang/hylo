import Utils
import XCTest

func twoToThe(_ x: UInt) -> UInt {
  1 << x
}

func average(_ x: UInt, _ y: UInt) -> UInt { (x + y) / 2 }
final class UnsignedFixedWidthIntegerExtensionTests: XCTestCase {

  func testBitsToBytes() {
    XCTAssertEqual(UInt(0).bitsToBytes, 0)
    XCTAssertEqual(UInt(1).bitsToBytes, 1)

    XCTAssertEqual(UInt(8 * 1 - 1).bitsToBytes, 1)
    XCTAssertEqual(UInt(8 * 1).bitsToBytes, 1)
    XCTAssertEqual(UInt(8 * 1 + 1).bitsToBytes, 2)

    XCTAssertEqual(UInt(8 * 2 - 1).bitsToBytes, 2)
    XCTAssertEqual(UInt(8 * 2).bitsToBytes, 2)
    XCTAssertEqual(UInt(8 * 2 + 1).bitsToBytes, 3)

    XCTAssertEqual(UInt(8 * 3 - 1).bitsToBytes, 3)
    XCTAssertEqual(UInt(8 * 3).bitsToBytes, 3)
    XCTAssertEqual(UInt(8 * 3 + 1).bitsToBytes, 4)
  }

  func testBytesToBits() {
    XCTAssertEqual(UInt(0).bytesToBits, 0)
    XCTAssertEqual(UInt(1).bytesToBits, 8)
    XCTAssertEqual(UInt(2).bytesToBits, 16)
    XCTAssertEqual(UInt(3).bytesToBits, 24)
  }

  func testBitsInRepresentation() {
    XCTAssertEqual(UInt(0).bitsInRepresentation, 0)
    XCTAssertEqual(UInt(1).bitsInRepresentation, 1)
    XCTAssertEqual(UInt((1 << 2) - 1).bitsInRepresentation, 2)
    XCTAssertEqual(UInt(1 << 2).bitsInRepresentation, 3)
    XCTAssertEqual(UInt((1 << 3) - 1).bitsInRepresentation, 3)
    XCTAssertEqual(UInt(1 << 3).bitsInRepresentation, 4)
    XCTAssertEqual(UInt((1 << 4) - 1).bitsInRepresentation, 4)
    XCTAssertEqual(UInt(1 << 4).bitsInRepresentation, 5)
  }

  func testRoundedUpToPowerOf2() {
    XCTAssertEqual(UInt(0).roundedUpToPowerOf2, 1)
    XCTAssertEqual(twoToThe(0).roundedUpToPowerOf2, 1)

    XCTAssertEqual((twoToThe(0) + 1).roundedUpToPowerOf2, 2)
    XCTAssertEqual((twoToThe(1)).roundedUpToPowerOf2, 2)

    XCTAssertEqual((twoToThe(1) + 1).roundedUpToPowerOf2, 4)
    XCTAssertEqual((twoToThe(2) - 1).roundedUpToPowerOf2, 4)
    XCTAssertEqual((twoToThe(2)).roundedUpToPowerOf2, 4)

    XCTAssertEqual((twoToThe(2) + 1).roundedUpToPowerOf2, 8)
    XCTAssertEqual((twoToThe(3) - 1).roundedUpToPowerOf2, 8)
    XCTAssertEqual((twoToThe(3)).roundedUpToPowerOf2, 8)

    XCTAssertEqual((twoToThe(3) + 1).roundedUpToPowerOf2, 16)
    XCTAssertEqual((average(twoToThe(3),twoToThe(4))).roundedUpToPowerOf2, 16)
    XCTAssertEqual((twoToThe(4) - 1).roundedUpToPowerOf2, 16)
    XCTAssertEqual((twoToThe(4)).roundedUpToPowerOf2, 16)
  }

}
