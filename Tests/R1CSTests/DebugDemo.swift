// Demo of R1CS debug descriptions
// Run with: swift test --filter testDebugDemo

import XCTest
import BigInt
@testable import R1CS

final class DebugDemo: XCTestCase {
  
  /// Demo showing the colorful debug output
  func testDebugDemo() {
    print("\n\n=== R1CS Debug Description Demo ===\n")
    
    // Create a simple multiplication circuit: x * y = z
    let prime = BigUInt("21888242871839275222246405745257275088548364400416034343698204186575808495617", radix: 10)!
    var r1cs = R1CS(prime: prime)
    
    // Allocate labels
    let xLabel = r1cs.nextLabel()  // l0: input x
    let yLabel = r1cs.nextLabel()  // l1: input y  
    let zLabel = r1cs.nextLabel()  // l2: output z
    
    // Add wires
    let xWire = r1cs.addWire(labelId: xLabel)
    let yWire = r1cs.addWire(labelId: yLabel)
    let zWire = r1cs.addWire(labelId: zLabel)
    
    r1cs.publicInputCount = 2   // x and y
    r1cs.publicOutputCount = 1  // z
    
    // Constraint: x * y = z
    let a = LinearCombination(terms: [(wire: xWire, coefficient: 1)])
    let b = LinearCombination(terms: [(wire: yWire, coefficient: 1)])
    let c = LinearCombination(terms: [(wire: zWire, coefficient: 1)])
    r1cs.addConstraint(R1CSConstraint(a: a, b: b, c: c))
    
    print(r1cs.debugDescription)
    
    // Add a more complex constraint: (x + y) * (x + y) = z
    let xPlusY = LinearCombination(terms: [
      (wire: xWire, coefficient: 1),
      (wire: yWire, coefficient: 1)
    ])
    r1cs.addConstraint(R1CSConstraint(a: xPlusY, b: xPlusY, c: c))
    
    print("\n=== After adding second constraint ===\n")
    print(r1cs.debugDescription)
    
    print("\n=== Individual Components ===\n")
    print("WireID examples:")
    print("  - \(R1CS.unitWire.debugDescription)")
    print("  - \(WireID(rawValue: 5).debugDescription)")
    
    print("\nLabelID example:")
    print("  - \(LabelID(rawValue: 42).debugDescription)")
    
    print("\nLinear Combination examples:")
    let lc1 = LinearCombination(terms: [(wire: R1CS.unitWire, coefficient: 1)])
    print("  Constant 1: \(lc1.debugDescription(compact: true))")
    
    let lc2 = LinearCombination(terms: [
      (wire: WireID(rawValue: 1), coefficient: 3),
      (wire: WireID(rawValue: 2), coefficient: 5)
    ])
    print("  3w1 + 5w2: \(lc2.debugDescription(compact: true))")
    
    print("\nConstraint equation format:")
    let demo = R1CSConstraint(a: xPlusY, b: lc1, c: c)
    print("  \(demo.debugDescription(showEquation: true))")
    
    print("\n")
  }
}
