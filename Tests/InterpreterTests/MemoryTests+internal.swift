import Testing
import TestUtils
import FrontEnd
@testable import Interpreter

@Test func Interpreter_Memory_requireInitialized() throws {
  var m = Memory()
  let p = m.allocate(10, bytesWithAlignment: 1)
  var layouts = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
  let void_ = layouts[AnyType.void]
  let voids = layouts[^TupleType(types: [.void, .void])]
  let voidsFirstPart = voids.componentIDs.first!

  #expect(throws: Memory.Error.partUninitialized(p, voidsFirstPart)) {
    try m.allocation[p.allocation]!
      .requireInitialized(part: voidsFirstPart, baseOffset: 0, region: 0)
  }

  try m.finishInitialization(at: p + voids.components[0].offset, to: void_)
  // It should be possible to initialize both parts at the same address
  try m.finishInitialization(at: p + voids.components[1].offset, to: void_)
  try m.finishInitialization(at: p, to: voids)

  let i8s = layouts[^TupleType(types: [.builtin(.i(8)), .builtin(.i(8))])]
  let i8sFirstPart = i8s.componentIDs.first!

  #expect(throws: Memory.Error.partType(voids.type, part: i8sFirstPart)) {
    try m.allocation[p.allocation]!
      .requireInitialized(part: i8sFirstPart, baseOffset: 0, region: 0)
  }
}
