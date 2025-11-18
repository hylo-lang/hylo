import XCTest
import TestUtils
import FrontEnd
@testable import Interpreter

final class InterpreterMemoryInternalTests: XCTestCase {

  func test_requireInitialized() throws {
    var layouts = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let void_ = layouts[AnyType.void]
    let voidPair = layouts[^TupleType(types: [.void, .void])]

    var m = Memory()
    let p = m.allocate(voidPair.size, bytesWithAlignment: voidPair.alignment)

    let voidPairPart0 = voidPair.partIDs.first!

    check(throws: Memory.Error.noComposedPart(at: p, voidPairPart0)) {
      try m.allocation[p.allocation]!
        .requireComposed(part: voidPairPart0, baseOffset: 0, region: 0)
    }

    try m.compose(void_, at: p + voidPair.parts[0].offset)
    // It should be possible to initialize both parts at the same address
    try m.compose(void_, at: p + voidPair.parts[1].offset)
    try m.compose(voidPair, at: p)

    let i8Pair = layouts[^TupleType(types: [.builtin(.i(8)), .builtin(.i(8))])]
    let i8PairPart0 = i8Pair.partIDs.first!

    check(throws: Memory.Error.partType(voidPair.type, part: i8PairPart0)) {
      try m.allocation[p.allocation]!
        .requireComposed(part: i8PairPart0, baseOffset: 0, region: 0)
    }
  }

  func test_canAccessUnsafePointerToAllocatedBytes() throws {
    var memory = Memory()
    let allocationAddress = memory.allocate(1, bytesWithAlignment: 1)

    memory.allocation[allocationAddress.allocation]!.withUnsafePointer(to: UInt8.self, at: 0) { p in
    }
  }
}
