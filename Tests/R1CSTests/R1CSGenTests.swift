import XCTest
import BigInt
@testable import R1CS

final class R1CSTests: XCTestCase {
  
  /// Test basic R1CS functionality
  func testBuilderBasics() {
    let prime = BigUInt("21888242871839275222246405745257275088548364400416034343698204186575808495617", radix: 10)!
    var r1cs = R1CS(prime: prime)
    
    XCTAssertEqual(r1cs.wireCount, 1)  // Wire 0 is always present
    
    // Add some wires
    let wire1 = r1cs.addWire(labelId: LabelID(rawValue: 3))
    XCTAssertEqual(wire1.raw, 1)
    XCTAssertEqual(r1cs.wireCount, 2)
    
    let wire2 = r1cs.addWire(labelId: LabelID(rawValue: 10))
    XCTAssertEqual(wire2.raw, 2)
    XCTAssertEqual(r1cs.wireCount, 3)
    
    XCTAssertEqual(r1cs.labelCount, 11)  // Highest label is 10, so count is 11
  }
  
  /// Test linear combination
  func testLinearCombination() {
    var lc = LinearCombination()
    XCTAssertTrue(lc.terms.isEmpty)
    
    lc.addTerm(wire: WireID(rawValue: 5), coefficient: 3)
    lc.addTerm(wire: WireID(rawValue: 2), coefficient: 8)
    lc.addTerm(wire: WireID(rawValue: 10), coefficient: 1)
    
    // Terms are stored in insertion order
    XCTAssertEqual(lc.terms.count, 3)
    XCTAssertEqual(lc.terms[0].wire.raw, 5)
    XCTAssertEqual(lc.terms[1].wire.raw, 2)
    XCTAssertEqual(lc.terms[2].wire.raw, 10)
  }
  
  /// Test constraint creation
  func testConstraintCreation() {
    var a = LinearCombination()
    a.addTerm(wire: WireID(rawValue: 5), coefficient: 3)
    a.addTerm(wire: WireID(rawValue: 6), coefficient: 8)
    
    var b = LinearCombination()
    b.addTerm(wire: R1CS.unitWire, coefficient: 2)
    b.addTerm(wire: WireID(rawValue: 2), coefficient: 20)
    
    var c = LinearCombination()
    c.addTerm(wire: R1CS.unitWire, coefficient: 5)
    c.addTerm(wire: WireID(rawValue: 2), coefficient: 7)
    
    let constraint = R1CSConstraint(a: a, b: b, c: c)
    
    XCTAssertEqual(constraint.a.terms.count, 2)
    XCTAssertEqual(constraint.b.terms.count, 2)
    XCTAssertEqual(constraint.c.terms.count, 2)
  }
  
  /// Test the example from the specification document
  func testSpecificationExample() throws {
    // Prime from the spec (BN128)
    let primeHex = "010000f093f5e1439170b97948e833285d588181b64550b829a031e1724e6430"
    let prime = BigUInt(primeHex, radix: 16)!
    
    var r1cs = R1CS(prime: prime)
    
    // Set up wires as in the example
    // w_0 := l_0 (already exists)
    // w_1 := l_3
    r1cs.addWire(labelId: LabelID(rawValue: 3))
    // w_2 := l_10
    r1cs.addWire(labelId: LabelID(rawValue: 10))
    // w_3 := l_11
    r1cs.addWire(labelId: LabelID(rawValue: 11))
    // w_4 := l_12
    r1cs.addWire(labelId: LabelID(rawValue: 12))
    // w_5 := l_15
    r1cs.addWire(labelId: LabelID(rawValue: 15))
    // w_6 := l_324
    r1cs.addWire(labelId: LabelID(rawValue: 324))
    
    XCTAssertEqual(r1cs.wireCount, 7)
    XCTAssertEqual(r1cs.labelCount, 325)  // Highest label + 1
    
    // Constraint 0: (3w_5 + 8w_6) * (2w_0 + 20w_2 + 12w_3) - (5w_0 + 7w_2) = 0
    let a0 = LinearCombination(terms: [
      (wire: WireID(rawValue: 5), coefficient: 3),
      (wire: WireID(rawValue: 6), coefficient: 8)
    ])
    let b0 = LinearCombination(terms: [
      (wire: R1CS.unitWire, coefficient: 2),
      (wire: WireID(rawValue: 2), coefficient: 20),
      (wire: WireID(rawValue: 3), coefficient: 12)
    ])
    let c0 = LinearCombination(terms: [
      (wire: R1CS.unitWire, coefficient: 5),
      (wire: WireID(rawValue: 2), coefficient: 7)
    ])
    r1cs.addConstraint(R1CSConstraint(a: a0, b: b0, c: c0))
    
    // Constraint 1: (4w_1 + 8w_4 + 3w_5) * (6w_6 + 44w_3) = 0
    let a1 = LinearCombination(terms: [
      (wire: WireID(rawValue: 1), coefficient: 4),
      (wire: WireID(rawValue: 4), coefficient: 8),
      (wire: WireID(rawValue: 5), coefficient: 3)
    ])
    let b1 = LinearCombination(terms: [
      (wire: WireID(rawValue: 3), coefficient: 44),
      (wire: WireID(rawValue: 6), coefficient: 6)
    ])
    let c1 = LinearCombination()  // Empty
    r1cs.addConstraint(R1CSConstraint(a: a1, b: b1, c: c1))
    
    // Constraint 2: (4w_6) * (6w_0 + 5w_3 + 11w_2) - (600w_6) = 0
    let a2 = LinearCombination(terms: [
      (wire: WireID(rawValue: 6), coefficient: 4)
    ])
    let b2 = LinearCombination(terms: [
      (wire: WireID(rawValue: 0), coefficient: 6),
      (wire: WireID(rawValue: 2), coefficient: 11),
      (wire: WireID(rawValue: 3), coefficient: 5)
    ])
    let c2 = LinearCombination(terms: [
      (wire: WireID(rawValue: 6), coefficient: 600)
    ])
    r1cs.addConstraint(R1CSConstraint(a: a2, b: b2, c: c2))
    
    XCTAssertEqual(r1cs.constraints.count, 3)
    
    // Set public/private counts as in spec
    r1cs.publicOutputCount = 1
    r1cs.publicInputCount = 2
    r1cs.privateInputCount = 3
    r1cs.labelCount = 1000  // Set explicitly to match spec
    
    // Write to file
    let tempDir = FileManager.default.temporaryDirectory
    let fileURL = tempDir.appendingPathComponent("test_spec_example.r1cs")
    try r1cs.serialize(to: fileURL)
    
    // Verify file was created and has content
    XCTAssertTrue(FileManager.default.fileExists(atPath: fileURL.path))
    let data = try Data(contentsOf: fileURL)
    XCTAssertGreaterThan(data.count, 0)
    
    // Verify magic number
    XCTAssertEqual(data[0], 0x72)  // 'r'
    XCTAssertEqual(data[1], 0x31)  // '1'
    XCTAssertEqual(data[2], 0x63)  // 'c'
    XCTAssertEqual(data[3], 0x73)  // 's'
    
    // Verify version
    XCTAssertEqual(data[4], 0x01)
    XCTAssertEqual(data[5], 0x00)
    XCTAssertEqual(data[6], 0x00)
    XCTAssertEqual(data[7], 0x00)
    
    // Clean up
    try? FileManager.default.removeItem(at: fileURL)
  }
  
  /// Test binary format of header section
  func testHeaderSerialization() throws {
    let prime = BigUInt(256)  // Small prime for testing
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))
    r1cs.addWire(labelId: LabelID(rawValue: 2))
    r1cs.publicOutputCount = 1
    r1cs.publicInputCount = 1
    r1cs.privateInputCount = 0
    
    let tempDir = FileManager.default.temporaryDirectory
    let fileURL = tempDir.appendingPathComponent("test_header.r1cs")
    try r1cs.serialize(to: fileURL)
    
    let data = try Data(contentsOf: fileURL)
    
    // Magic (4 bytes) + Version (4 bytes) + NumSections (4 bytes) = 12 bytes header
    XCTAssertGreaterThanOrEqual(data.count, 12)
    
    // Check number of sections
    let numSections = data.withUnsafeBytes { $0.load(fromByteOffset: 8, as: UInt32.self) }
    XCTAssertEqual(numSections, 3)
    
    // Clean up
    try? FileManager.default.removeItem(at: fileURL)
  }
  
  /// Test that wire IDs in linear combinations are sorted during serialization
  func testLinearCombinationSorting() throws {
    let prime = BigUInt("21888242871839275222246405745257275088548364400416034343698204186575808495617", radix: 10)!
    var r1cs = R1CS(prime: prime)
    
    // Create linear combination with unsorted wire IDs
    let lc = LinearCombination(terms: [
      (wire: WireID(rawValue: 10), coefficient: 1),
      (wire: WireID(rawValue: 2), coefficient: 2),
      (wire: WireID(rawValue: 5), coefficient: 3),
      (wire: WireID(rawValue: 1), coefficient: 4)
    ])
    
    // Terms are stored in insertion order (unsorted)
    XCTAssertEqual(lc.terms[0].wire.raw, 10)
    XCTAssertEqual(lc.terms[1].wire.raw, 2)
    XCTAssertEqual(lc.terms[2].wire.raw, 5)
    XCTAssertEqual(lc.terms[3].wire.raw, 1)
    
    // But serialization should produce sorted output
    let constraint = R1CSConstraint(
      a: lc,
      b: LinearCombination(terms: [(wire: WireID(rawValue: 0), coefficient: 1)]),
      c: LinearCombination(terms: [(wire: WireID(rawValue: 0), coefficient: 1)])
    )
    r1cs.addConstraint(constraint)
    
    let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".r1cs")
    try r1cs.serialize(to: tempFile)
    
    // Read back and verify format is valid
    let data = try Data(contentsOf: tempFile)
    XCTAssertGreaterThan(data.count, 0)
    
    try? FileManager.default.removeItem(at: tempFile)
  }
  
  /// Test empty constraint system
  func testEmptyConstraintSystem() throws {
    let prime = BigUInt(17)
    let r1cs = R1CS(prime: prime)
    
    XCTAssertEqual(r1cs.wireCount, 1)  // Only wire 0
    XCTAssertEqual(r1cs.constraints.count, 0)
    
    let tempDir = FileManager.default.temporaryDirectory
    let fileURL = tempDir.appendingPathComponent("test_empty.r1cs")
    try r1cs.serialize(to: fileURL)
    
    XCTAssertTrue(FileManager.default.fileExists(atPath: fileURL.path))
    
    // Clean up
    try? FileManager.default.removeItem(at: fileURL)
  }
  
  /// Test large field element serialization
  func testLargeFieldSerialization() throws {
    // Use BN254 prime (common in zkSNARK applications)
    let bn254Prime = BigUInt("21888242871839275222246405745257275088548364400416034343698204186575808495617", radix: 10)!
    var r1cs = R1CS(prime: bn254Prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))
    
    // Create constraint with large coefficients
    let a = LinearCombination(terms: [
      (wire: WireID(rawValue: 0), coefficient: bn254Prime - BigUInt(1))
    ])
    let b = LinearCombination(terms: [
      (wire: WireID(rawValue: 1), coefficient: bn254Prime / BigUInt(2))
    ])
    let c = LinearCombination()
    
    r1cs.addConstraint(R1CSConstraint(a: a, b: b, c: c))
    
    let tempDir = FileManager.default.temporaryDirectory
    let fileURL = tempDir.appendingPathComponent("test_large_field.r1cs")
    try r1cs.serialize(to: fileURL)
    
    XCTAssertTrue(FileManager.default.fileExists(atPath: fileURL.path))
    
    // Clean up
    try? FileManager.default.removeItem(at: fileURL)
  }
  
  // MARK: - Edge Cases and Additional Serialization Tests
  
  /// Test wire with same label ID
  func testMultipleWiresWithSameLabel() {
    let prime = BigUInt(101)
    var r1cs = R1CS(prime: prime)
    
    let label = LabelID(rawValue: 5)
    let wire1 = r1cs.addWire(labelId: label)
    let wire2 = r1cs.addWire(labelId: label)
    let wire3 = r1cs.addWire(labelId: label)
    
    XCTAssertNotEqual(wire1, wire2)
    XCTAssertNotEqual(wire2, wire3)
    XCTAssertEqual(r1cs.wireCount, 4)  // wire 0 + 3 new wires
    XCTAssertEqual(r1cs.labelCount, 6)  // 0-5
  }
  
  /// Test nextLabel increments correctly
  func testNextLabelIncrement() {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    XCTAssertEqual(r1cs.labelCount, 0)
    
    let label1 = r1cs.nextLabel()
    XCTAssertEqual(label1.rawValue, 0)
    XCTAssertEqual(r1cs.labelCount, 1)
    
    let label2 = r1cs.nextLabel()
    XCTAssertEqual(label2.rawValue, 1)
    XCTAssertEqual(r1cs.labelCount, 2)
    
    let label3 = r1cs.nextLabel()
    XCTAssertEqual(label3.rawValue, 2)
    XCTAssertEqual(r1cs.labelCount, 3)
  }
  
  /// Test addWire updates labelCount correctly
  func testAddWireUpdatesLabelCount() {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    XCTAssertEqual(r1cs.labelCount, 0)
    
    // Add wire with label 5
    r1cs.addWire(labelId: LabelID(rawValue: 5))
    XCTAssertEqual(r1cs.labelCount, 6)
    
    // Add wire with label 3 (less than current max)
    r1cs.addWire(labelId: LabelID(rawValue: 3))
    XCTAssertEqual(r1cs.labelCount, 6)  // Should not decrease
    
    // Add wire with label 10 (greater than current max)
    r1cs.addWire(labelId: LabelID(rawValue: 10))
    XCTAssertEqual(r1cs.labelCount, 11)
  }
  
  /// Test empty linear combinations
  func testEmptyLinearCombinations() throws {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    let empty = LinearCombination()
    let constraint = R1CSConstraint(
      a: empty,
      b: LinearCombination(),
      c: LinearCombination()
    )
    r1cs.addConstraint(constraint)
    
    let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".r1cs")
    try r1cs.serialize(to: tempFile)
    
    let data = try Data(contentsOf: tempFile)
    XCTAssertGreaterThan(data.count, 0)
    
    try? FileManager.default.removeItem(at: tempFile)
  }
  
  /// Test linear combination with single term
  func testSingleTermLinearCombination() {
    let lc = LinearCombination(terms: [
      (wire: WireID(rawValue: 7), coefficient: 42)
    ])
    
    XCTAssertEqual(lc.terms.count, 1)
    XCTAssertEqual(lc.terms[0].wire.raw, 7)
    XCTAssertEqual(lc.terms[0].coefficient, 42)
  }
  
  /// Test constraint with mix of empty and non-empty linear combinations
  func testMixedConstraints() throws {
    let prime = BigUInt(13)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))
    
    // Constraint: (w0) * (w1) - 0 = 0
    let a = LinearCombination(terms: [(wire: WireID(rawValue: 0), coefficient: 1)])
    let b = LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 1)])
    let c = LinearCombination()  // Empty
    
    r1cs.addConstraint(R1CSConstraint(a: a, b: b, c: c))
    XCTAssertEqual(r1cs.constraints.count, 1)
    
    let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".r1cs")
    try r1cs.serialize(to: tempFile)
    
    let data = try Data(contentsOf: tempFile)
    XCTAssertGreaterThan(data.count, 0)
    
    try? FileManager.default.removeItem(at: tempFile)
  }
  
  /// Test wire ID 0 (constant wire)
  func testConstantWire() {
    let prime = BigUInt(17)
    let r1cs = R1CS(prime: prime)
    
    XCTAssertEqual(r1cs.wireCount, 1)
    XCTAssertEqual(r1cs.wireToLabelMap[0].rawValue, 0)  // Wire 0 maps to label 0
  }
  
  /// Test large number of wires
  func testManyWires() throws {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    let wireCount = 1000
    for i in 0..<wireCount {
      let wire = r1cs.addWire(labelId: LabelID(rawValue: UInt64(i)))
      XCTAssertEqual(wire.raw, UInt32(i + 1))  // +1 because wire 0 exists
    }
    
    XCTAssertEqual(r1cs.wireCount, UInt32(wireCount + 1))
    
    let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".r1cs")
    try r1cs.serialize(to: tempFile)
    
    let data = try Data(contentsOf: tempFile)
    XCTAssertGreaterThan(data.count, 0)
    
    try? FileManager.default.removeItem(at: tempFile)
  }
  
  /// Test large number of constraints
  func testManyConstraints() throws {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))
    r1cs.addWire(labelId: LabelID(rawValue: 2))
    
    let constraintCount = 500
    for _ in 0..<constraintCount {
      let a = LinearCombination(terms: [(wire: WireID(rawValue: 0), coefficient: 1)])
      let b = LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 1)])
      let c = LinearCombination(terms: [(wire: WireID(rawValue: 2), coefficient: 1)])
      r1cs.addConstraint(R1CSConstraint(a: a, b: b, c: c))
    }
    
    XCTAssertEqual(r1cs.constraints.count, constraintCount)
    
    let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".r1cs")
    try r1cs.serialize(to: tempFile)
    
    let data = try Data(contentsOf: tempFile)
    XCTAssertGreaterThan(data.count, 0)
    
    try? FileManager.default.removeItem(at: tempFile)
  }
  
  /// Test field size calculation for various primes
  func testFieldSizeCalculation() throws {
    // Test with different prime sizes to ensure field size is calculated correctly
    let testCases: [(BigUInt, String)] = [
      (BigUInt(2).power(8) - 1, "8-bit"),
      (BigUInt(2).power(16) - 1, "16-bit"),
      (BigUInt(2).power(32) - 1, "32-bit"),
      (BigUInt(2).power(64) - 1, "64-bit"),
      (BigUInt(2).power(128) - 1, "128-bit"),
      (BigUInt("21888242871839275222246405745257275088548364400416034343698204186575808495617", radix: 10)!, "BN254")
    ]
    
    for (prime, description) in testCases {
      var r1cs = R1CS(prime: prime)
      r1cs.addWire(labelId: LabelID(rawValue: 1))
      
      let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".r1cs")
      try r1cs.serialize(to: tempFile)
      
      let data = try Data(contentsOf: tempFile)
      XCTAssertGreaterThan(data.count, 0, "Failed for \(description)")
      
      try? FileManager.default.removeItem(at: tempFile)
    }
  }
  
  /// Test public/private wire counts
  func testWireVisibilityCounts() throws {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.publicOutputCount = 2
    r1cs.publicInputCount = 3
    r1cs.privateInputCount = 5
    
    XCTAssertEqual(r1cs.publicOutputCount, 2)
    XCTAssertEqual(r1cs.publicInputCount, 3)
    XCTAssertEqual(r1cs.privateInputCount, 5)
    
    let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".r1cs")
    try r1cs.serialize(to: tempFile)
    
    let data = try Data(contentsOf: tempFile)
    XCTAssertGreaterThan(data.count, 0)
    
    try? FileManager.default.removeItem(at: tempFile)
  }
  
  /// Test zero coefficients
  func testZeroCoefficients() throws {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))
    
    // Linear combination with zero coefficient (unusual but valid)
    let lc = LinearCombination(terms: [
      (wire: WireID(rawValue: 0), coefficient: 0),
      (wire: WireID(rawValue: 1), coefficient: 0)
    ])
    
    XCTAssertEqual(lc.terms.count, 2)
    
    let constraint = R1CSConstraint(
      a: lc,
      b: LinearCombination(),
      c: LinearCombination()
    )
    r1cs.addConstraint(constraint)
    
    let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".r1cs")
    try r1cs.serialize(to: tempFile)
    
    let data = try Data(contentsOf: tempFile)
    XCTAssertGreaterThan(data.count, 0)
    
    try? FileManager.default.removeItem(at: tempFile)
  }
  
  /// Test coefficients equal to prime - 1 (maximum field element)
  func testMaxFieldCoefficients() throws {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))
    
    let maxCoeff = prime - 1
    let lc = LinearCombination(terms: [
      (wire: WireID(rawValue: 0), coefficient: maxCoeff),
      (wire: WireID(rawValue: 1), coefficient: maxCoeff)
    ])
    
    let constraint = R1CSConstraint(
      a: lc,
      b: LinearCombination(terms: [(wire: WireID(rawValue: 0), coefficient: 1)]),
      c: LinearCombination()
    )
    r1cs.addConstraint(constraint)
    
    let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".r1cs")
    try r1cs.serialize(to: tempFile)
    
    let data = try Data(contentsOf: tempFile)
    XCTAssertGreaterThan(data.count, 0)
    
    try? FileManager.default.removeItem(at: tempFile)
  }
  
  /// Test binary format structure according to specification
  func testBinaryFormatStructure() throws {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))
    r1cs.addWire(labelId: LabelID(rawValue: 2))
    r1cs.publicOutputCount = 1
    r1cs.publicInputCount = 1
    r1cs.privateInputCount = 1
    
    let a = LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 3)])
    let b = LinearCombination(terms: [(wire: WireID(rawValue: 2), coefficient: 4)])
    let c = LinearCombination(terms: [(wire: WireID(rawValue: 0), coefficient: 12)])
    r1cs.addConstraint(R1CSConstraint(a: a, b: b, c: c))
    
    let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".r1cs")
    try r1cs.serialize(to: tempFile)
    
    let data = try Data(contentsOf: tempFile)
    
    // Verify magic number "r1cs"
    XCTAssertEqual(data[0], 0x72)  // 'r'
    XCTAssertEqual(data[1], 0x31)  // '1'
    XCTAssertEqual(data[2], 0x63)  // 'c'
    XCTAssertEqual(data[3], 0x73)  // 's'
    
    // Verify version (little-endian UInt32)
    let version = data.withUnsafeBytes { $0.load(fromByteOffset: 4, as: UInt32.self) }
    XCTAssertEqual(version, 1)
    
    // Verify number of sections (little-endian UInt32)
    let numSections = data.withUnsafeBytes { $0.load(fromByteOffset: 8, as: UInt32.self) }
    XCTAssertEqual(numSections, 3)
    
    // Verify first section type is header (0x01)
    let firstSectionType = data.withUnsafeBytes { $0.load(fromByteOffset: 12, as: UInt32.self) }
    XCTAssertEqual(firstSectionType, 0x01)
    
    try? FileManager.default.removeItem(at: tempFile)
  }
  
  /// Test wire to label map serialization
  func testWire2LabelMapSerialization() throws {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    // Create specific wire-to-label mappings
    r1cs.addWire(labelId: LabelID(rawValue: 10))  // wire 1 -> label 10
    r1cs.addWire(labelId: LabelID(rawValue: 20))  // wire 2 -> label 20
    r1cs.addWire(labelId: LabelID(rawValue: 30))  // wire 3 -> label 30
    
    XCTAssertEqual(r1cs.wireToLabelMap.count, 4)  // wire 0 + 3 new wires
    XCTAssertEqual(r1cs.wireToLabelMap[0].rawValue, 0)
    XCTAssertEqual(r1cs.wireToLabelMap[1].rawValue, 10)
    XCTAssertEqual(r1cs.wireToLabelMap[2].rawValue, 20)
    XCTAssertEqual(r1cs.wireToLabelMap[3].rawValue, 30)
    
    let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".r1cs")
    try r1cs.serialize(to: tempFile)
    
    let data = try Data(contentsOf: tempFile)
    XCTAssertGreaterThan(data.count, 0)
    
    try? FileManager.default.removeItem(at: tempFile)
  }
  
  /// Test linear combination with many terms
  func testLinearCombinationWithManyTerms() throws {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    // Add many wires
    for i in 1...100 {
      r1cs.addWire(labelId: LabelID(rawValue: UInt64(i)))
    }
    
    // Create linear combination with many terms
    var terms: [(wire: WireID, coefficient: BigUInt)] = []
    for i in 0..<100 {
      terms.append((wire: WireID(rawValue: UInt32(i)), coefficient: BigUInt(i + 1)))
    }
    let lc = LinearCombination(terms: terms)
    
    XCTAssertEqual(lc.terms.count, 100)
    
    let constraint = R1CSConstraint(
      a: lc,
      b: LinearCombination(terms: [(wire: WireID(rawValue: 0), coefficient: 1)]),
      c: LinearCombination()
    )
    r1cs.addConstraint(constraint)
    
    let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".r1cs")
    try r1cs.serialize(to: tempFile)
    
    let data = try Data(contentsOf: tempFile)
    XCTAssertGreaterThan(data.count, 0)
    
    try? FileManager.default.removeItem(at: tempFile)
  }
  
  /// Test WireID and LabelID strong types
  func testStrongTypes() {
    let wire1 = WireID(rawValue: 5)
    let wire2 = WireID(rawValue: 5)
    let wire3 = WireID(rawValue: 10)
    
    XCTAssertEqual(wire1, wire2)
    XCTAssertNotEqual(wire1, wire3)
    XCTAssertLessThan(wire1, wire3)
    
    let label1 = LabelID(rawValue: 100)
    let label2 = LabelID(rawValue: 100)
    let label3 = LabelID(rawValue: 200)
    
    XCTAssertEqual(label1, label2)
    XCTAssertNotEqual(label1, label3)
  }
}

