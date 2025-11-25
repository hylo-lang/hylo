import XCTest
import BigInt
import R1CS

final class DebugDescriptionTests: XCTestCase {
  
  /// Test debug description output for a complete R1CS system
  func testR1CSDebugDescription() {
    let prime = BigUInt("21888242871839275222246405745257275088548364400416034343698204186575808495617", radix: 10)!
    var r1cs = R1CS(prime: prime)
    
    // Add some wires
    let label1 = r1cs.nextLabel()
    let label2 = r1cs.nextLabel()
    let label3 = r1cs.nextLabel()
    
    let wire1 = r1cs.addWire(labelId: label1)
    let wire2 = r1cs.addWire(labelId: label2)
    let _ = r1cs.addWire(labelId: label3)
    
    r1cs.publicOutputCount = 1
    r1cs.publicInputCount = 1
    r1cs.privateInputCount = 1
    
    // Add constraint: (3w1 + 2w2) * (1) = (3w1 + 2w2)
    var a = LinearCombination()
    a.addTerm(wire: wire1, coefficient: 3)
    a.addTerm(wire: wire2, coefficient: 2)
    
    var b = LinearCombination()
    b.addTerm(wire: r1cs.addWire(labelId: r1cs.nextLabel()), coefficient: 1)  // Add dummy wire for w0-like behavior
    
    var c = LinearCombination()
    c.addTerm(wire: wire1, coefficient: 3)
    c.addTerm(wire: wire2, coefficient: 2)
    
    r1cs.addConstraint(R1CSConstraint(a: a, b: b, c: c))
    
    let description = r1cs.debugDescription
    
    // Verify key information is present
    XCTAssertTrue(description.contains("R1CS Constraint System"))
    XCTAssertTrue(description.contains("Field:"))
    XCTAssertTrue(description.contains("Wires:"))
    XCTAssertTrue(description.contains("Constraints:"))
    XCTAssertTrue(description.contains("w0"))
    XCTAssertTrue(description.contains("w1"))
    
    print("\n" + description)
  }
  
  /// Test debug description for constraint
  func testConstraintDebugDescription() {
    var r1cs = R1CS(prime: BigUInt(17))
    
    let wire1 = r1cs.addWire(labelId: r1cs.nextLabel())
    let wire2 = r1cs.addWire(labelId: r1cs.nextLabel())
    let wire3 = r1cs.addWire(labelId: r1cs.nextLabel())
    
    var a = LinearCombination()
    a.addTerm(wire: wire1, coefficient: 5)
    
    var b = LinearCombination()
    b.addTerm(wire: wire2, coefficient: 3)
    
    var c = LinearCombination()
    c.addTerm(wire: wire3, coefficient: 15)
    
    let constraint = R1CSConstraint(a: a, b: b, c: c)
    let description = constraint.debugDescription
    
    XCTAssertTrue(description.contains("A:"))
    XCTAssertTrue(description.contains("B:"))
    XCTAssertTrue(description.contains("C:"))
    
    print("\nConstraint:\n" + description)
  }
  
  /// Test debug description for linear combination
  func testLinearCombinationDebugDescription() {
    var r1cs = R1CS(prime: BigUInt(17))
    
    let wire0 = r1cs.addWire(labelId: r1cs.nextLabel())
    let wire5 = r1cs.addWire(labelId: r1cs.nextLabel())
    let wire10 = r1cs.addWire(labelId: r1cs.nextLabel())
    
    var lc = LinearCombination()
    lc.addTerm(wire: wire0, coefficient: 1)
    lc.addTerm(wire: wire5, coefficient: 3)
    lc.addTerm(wire: wire10, coefficient: 7)
    
    let description = lc.debugDescription
    XCTAssertTrue(description.contains("w"))
    XCTAssertTrue(description.contains("·"))  // Multiplication dot
    
    print("\nLinear Combination:\n" + description)
  }
  
  /// Test debug description for empty linear combination
  func testEmptyLinearCombinationDebugDescription() {
    let lc = LinearCombination()
    let description = lc.debugDescription
    XCTAssertEqual(description, "0")
  }
  
  /// Test debug description for large R1CS with many constraints
  func testLargeR1CSDebugDescription() {
    let prime = BigUInt(101)
    var r1cs = R1CS(prime: prime)
    
    // Add many wires
    var wires: [WireID] = []
    for _ in 1...15 {
      wires.append(r1cs.addWire(labelId: r1cs.nextLabel()))
    }
    
    // Add several constraints
    for i in 0..<5 {
      var a = LinearCombination()
      a.addTerm(wire: wires[i], coefficient: BigUInt(i + 2))
      
      var b = LinearCombination()
      b.addTerm(wire: wires[0], coefficient: 1)
      
      var c = LinearCombination()
      c.addTerm(wire: wires[i], coefficient: BigUInt(i + 2))
      
      r1cs.addConstraint(R1CSConstraint(a: a, b: b, c: c))
    }
    
    let description = r1cs.debugDescription
    
    print("\n" + description)
    
    // Should show wire mapping with ellipsis for many wires  
    XCTAssertTrue(description.contains("Wire → Label Mapping:"), "Missing wire mapping header")
    XCTAssertTrue(description.contains("Constraints:") && description.contains("5 total"), "Missing constraint count")
    XCTAssertTrue(description.contains("more"), "Missing ellipsis with 'more'")
  }
  
  /// Test WireID debug description
  func testWireIDDebugDescription() {
    var r1cs = R1CS(prime: BigUInt(17))
    
    // Create wires
    let wire0 = r1cs.addWire(labelId: r1cs.nextLabel())
    let wire5 = r1cs.addWire(labelId: r1cs.nextLabel())
    
    let desc0 = wire0.debugDescription
    let desc5 = wire5.debugDescription
    
    XCTAssertTrue(desc0.contains("w"))
    XCTAssertTrue(desc5.contains("w"))
  }
  
  /// Test LabelID debug description
  func testLabelIDDebugDescription() {
    var r1cs = R1CS(prime: BigUInt(17))
    
    let label = r1cs.nextLabel()
    let description = label.debugDescription
    XCTAssertTrue(description.contains("l"))
  }
}
