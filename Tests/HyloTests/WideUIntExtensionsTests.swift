import Utils
import XCTest

@testable import IR

final class WideUIntExtensionsTests: XCTestCase {

  func testInitFromHyloLiteral() throws {
    assert(WideUInt(hyloLiteral: "0", signed: true, bitWidth: 8), is: "0", withWidth: 8)
    assert(WideUInt(hyloLiteral: "1", signed: true, bitWidth: 8), is: "1", withWidth: 8)
    assert(WideUInt(hyloLiteral: "-1", signed: true, bitWidth: 8), is: "255", withWidth: 8)
    assert(WideUInt(hyloLiteral: "-128", signed: true, bitWidth: 8), is: "128", withWidth: 8)

    XCTAssertNil(WideUInt(hyloLiteral: "128", signed: true, bitWidth: 8))
    XCTAssertNil(WideUInt(hyloLiteral: "-129", signed: true, bitWidth: 8))

    assert(WideUInt(hyloLiteral: "0", signed: false, bitWidth: 8), is: "0", withWidth: 8)
    assert(WideUInt(hyloLiteral: "1", signed: false, bitWidth: 8), is: "1", withWidth: 8)
    assert(WideUInt(hyloLiteral: "128", signed: false, bitWidth: 8), is: "128", withWidth: 8)

    assert(WideUInt(hyloLiteral: "0b10", signed: false, bitWidth: 8), is: "2", withWidth: 8)
    assert(WideUInt(hyloLiteral: "0o10", signed: false, bitWidth: 8), is: "8", withWidth: 8)
    assert(WideUInt(hyloLiteral: "0x10", signed: false, bitWidth: 8), is: "16", withWidth: 8)

    XCTAssertNil(WideUInt(hyloLiteral: "-1", signed: false, bitWidth: 8))

    assert(
      WideUInt(hyloLiteral: "10_000_000", signed: true, bitWidth: 64),
      is: "10000000", withWidth: 64)
    assert(
      WideUInt(hyloLiteral: "18446744073709551616", signed: true, bitWidth: 128),
      is: "18446744073709551616", withWidth: 128)
    assert(
      WideUInt(hyloLiteral: "-1", signed: true, bitWidth: 128),
      is: "340282366920938463463374607431768211455", withWidth: 128)
  }

  /// Asserts that `a` has given `description` and `bitWidth`.
  private func assert(
    _ a: WideUInt?, is description: String, withWidth bitWidth: Int,
    file: StaticString = #file, line: UInt = #line
  ) {
    guard let w = a else {
      XCTFail("expected '\(description)'", file: file, line: line)
      return
    }

    XCTAssertEqual(w.description, description, file: file, line: line)
    XCTAssertEqual(w.bitWidth, bitWidth, file: file, line: line)
  }

}
