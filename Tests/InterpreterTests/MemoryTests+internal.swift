import TestUtils
import FrontEnd
import XCTest
@testable import Interpreter

final class InterpreterMemoryTestsInternal: XCTestCase {
func testInterpreter_Memory_requireInitialized() throws {
  var layouts = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
  let void_ = layouts[AnyType.void]
  let voids = layouts[^TupleType(types: [.void, .void])]

  var m = Memory()
  let p = m.allocate(voids.size, bytesWithAlignment: voids.alignment)

  let voidsFirstPart = voids.componentIDs.first!

  XCTAssertThrowsError(try m.allocation[p.allocation]!
      .requireInitialized(part: voidsFirstPart, baseOffset: 0, region: 0)) {
    XCTAssertEqual($0 as! Memory.Error, .partUninitialized(p, voidsFirstPart))
  }

  try m.compose(void_, at: p + voids.components[0].offset)
  // It should be possible to initialize both parts at the same address
  try m.compose(void_, at: p + voids.components[1].offset)
  try m.compose(voids, at: p)

  let i8s = layouts[^TupleType(types: [.builtin(.i(8)), .builtin(.i(8))])]
  let i8sFirstPart = i8s.componentIDs.first!

  XCTAssertThrowsError(try m.allocation[p.allocation]!
    .requireInitialized(part: i8sFirstPart, baseOffset: 0, region: 0)) {
    XCTAssertEqual($0 as! Memory.Error, .partType(voids.type, part: i8sFirstPart))
  }
}
}
