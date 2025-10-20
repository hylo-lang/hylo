import FrontEnd
import Testing

@testable import Interpreter

@Test func InterpreterMemory_allocation() throws {
  var m = Memory()
  var allocations: [Memory.Address] = []
  for alignmentPower in 0..<8 {
    let alignment = 1 << alignmentPower
    let p = m.allocate(3, bytesWithAlignment: alignment)
    allocations.append(p)
    #expect(m.allocation[p.allocation]!.size == 3, "alignment \(alignment)")
    #expect(m.address(p, hasAlignment: alignment))
  }

  for p in allocations {
    try m.deallocate(p)

    #expect(throws: Memory.Error.doubleDeallocation(p)) {
      try m.deallocate(p)
    }

    let q = Memory.Address(allocation: p.allocation, offset: p.offset + 1)
    #expect(throws: Memory.Error.deallocationNotAtStartOfAllocation(q)) {
      try m.deallocate(q)
    }
  }
}
