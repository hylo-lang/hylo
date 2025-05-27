import FrontEnd
import XCTest

final class AccessEffectSetTests: XCTestCase {

  func testIsEmpty() {
    XCTAssert(AccessEffectSet().elements.isEmpty)
    XCTAssertFalse(AccessEffectSet.let.elements.isEmpty)
  }

  func testContains() {
    XCTAssertFalse(AccessEffectSet().contains(AccessEffect.inout))
    XCTAssertFalse(AccessEffectSet.set.contains(AccessEffect.inout))
    XCTAssert(AccessEffectSet.inout.contains(AccessEffect.inout))
    XCTAssert(AccessEffectSet([.inout, .sink]).contains(AccessEffect.inout))
  }

  func testIsSingleton() {
    XCTAssertFalse(AccessEffectSet().isSingleton)
    XCTAssertFalse(AccessEffectSet([.let, .set]).isSingleton)
    XCTAssert(AccessEffectSet.let.isSingleton)
  }

  func testUniqueElement() {
    XCTAssertNil(AccessEffectSet().uniqueElement)
    XCTAssertNil(AccessEffectSet([.let, .set]).uniqueElement)
    XCTAssertEqual(AccessEffectSet.inout.uniqueElement, AccessEffect.inout)
  }

  func testWeakest() {
    XCTAssertNil(AccessEffectSet().weakest)
    XCTAssertEqual(AccessEffectSet([.inout, .sink]).weakest, AccessEffect.inout)
  }

  func testStrongestIncluding() {
    XCTAssertEqual(AccessEffectSet().strongest(including: .inout), AccessEffect.inout)
    XCTAssertEqual(AccessEffectSet([.inout, .sink]).strongest(including: .let), AccessEffect.sink)
  }

  func testInsert() {
    var i = false
    var k = AccessEffect.let
    var s = AccessEffectSet()

    (i, k) = s.insert(.inout)
    XCTAssert(i)
    XCTAssertEqual(k, .inout)

    (i, k) = s.insert(.sink)
    XCTAssert(i)
    XCTAssertEqual(k, .sink)

    (i, k) = s.insert(.inout)
    XCTAssertFalse(i)
    XCTAssertEqual(k, .inout)
  }

  func testRemove() {
    var s = AccessEffectSet.all

    XCTAssertEqual(s.remove(.inout), .inout)
    XCTAssertFalse(s.contains(.inout))
    XCTAssertNil(s.remove(.inout))
  }

  func testUpdate() {
    var s = AccessEffectSet()
    XCTAssertNil(s.update(with: .inout))
    XCTAssertNil(s.update(with: .sink))
    XCTAssertEqual(s.update(with: .inout), .inout)
  }

  func testForUseOfBundle() {
    XCTAssertEqual(AccessEffectSet.forUseOfBundle(performingInPlaceMutation: false), .letOrSink)
    XCTAssertEqual(AccessEffectSet.forUseOfBundle(performingInPlaceMutation: true), .setOrInout)
  }

  func testElements() {
    XCTAssert(AccessEffectSet().elements.elementsEqual([]))
    XCTAssert(AccessEffectSet.letOrSink.elements.elementsEqual([.let, .sink]))
  }

}
