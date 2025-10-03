import FrontEnd
import Testing

@testable import Interpreter

@Test func InterpreterMemory_allocation() {
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
    m.deallocate(p)
  }

  _ = allocations[0]
}
