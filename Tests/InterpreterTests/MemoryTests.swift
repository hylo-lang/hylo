import FrontEnd
import Testing
import Interpreter

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

    #expect(throws: Memory.Error.noLongerAllocated(p)) {
      try m.deallocate(p)
    }

    let q = Memory.Address(allocation: p.allocation, offset: p.offset + 1)
    #expect(throws: Memory.Error.deallocationNotAtStartOfAllocation(q)) {
      try m.deallocate(q)
    }
  }
}

@Test func InterpreterMemory_tupleComposeDecompose() throws {
  var layouts = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
  let i16s = layouts[^TupleType(types: [.builtin(.i(16)), .builtin(.i(16))])]
  let i16 = layouts[.builtin(.i(16))]

  assert(i16s.alignment > 1, "Need to produce misaligned access for testing")

  var m = Memory()
  let p = m.allocate(i16s.size, bytesWithAlignment: i16s.alignment)

  #expect(throws: Memory.Error.alignment(p + 1, for: i16)) {
    try m.compose(i16, at: p + 1)
  }

  // An address that would be suitably aligned, but out of bounds for
  // i16s initialization.  The initialized object would extend past
  // the end of the allocation.
  let outOfBoundsForI16s = p + i16.size
  #expect(throws: Memory.Error.bounds(outOfBoundsForI16s, for: i16s, allocationSize: i16s.size)) {
    try m.compose(i16s, at: outOfBoundsForI16s)
  }

  let parts = i16s.components
  let partIDs = Array(i16s.componentIDs)
  #expect(throws: Memory.Error.partUninitialized(p, partIDs[0])) {
    try m.compose(i16s, at: p)
  }

  try m.compose(i16, at: p + parts[0].offset)

  #expect(throws: Memory.Error.partUninitialized(p + parts[1].offset, partIDs[1])) {
    try m.compose(i16s, at: p)
  }

  try m.compose(i16, at: p + parts[1].offset)

  try m.compose(i16s, at: p)

  #expect(throws: Memory.Error.noDecomposable(i16, at: p)) {
    try m.decompose(i16, at: p)
  }

  try m.decompose(i16s, at: p)

  #expect(throws: Memory.Error.noDecomposable(i16s, at: p)) {
    try m.decompose(i16s, at: p)
  }

  try m.decompose(i16, at: p + parts[0].offset)

  #expect(throws: Memory.Error.noDecomposable(i16, at: p + parts[0].offset)) {
    try m.decompose(i16, at: p)
  }

  try m.decompose(i16, at: p + parts[1].offset)

}
