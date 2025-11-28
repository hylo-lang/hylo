import XCTest
import BigInt
@testable import R1CS

final class WitnessValidatorTests: XCTestCase {
  
  /// Test validating a simple satisfied constraint
  func testSimpleValidWitness() {
    // Create a simple R1CS: w1 * w2 = w3
    // With witness: [1, 3, 4, 12]
    // This should satisfy the constraint: 3 * 4 = 12
    
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))  // w1
    r1cs.addWire(labelId: LabelID(rawValue: 2))  // w2
    r1cs.addWire(labelId: LabelID(rawValue: 3))  // w3
    
    // Constraint: w1 * w2 = w3
    let constraint = R1CSConstraint(
      a: LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 1)]),
      b: LinearCombination(terms: [(wire: WireID(rawValue: 2), coefficient: 1)]),
      c: LinearCombination(terms: [(wire: WireID(rawValue: 3), coefficient: 1)])
    )
    r1cs.addConstraint(constraint)
    
    // Valid witness
    let witness: [BigUInt] = [1, 3, 4, 12]
    
    let validator = WitnessValidator(r1cs: r1cs, witness: witness)
    let result = validator.validate()
    
    XCTAssertTrue(result.isValid)
    XCTAssertEqual(result.constraintResults.count, 1)
    XCTAssertTrue(result.constraintResults[0].isSatisfied)
    XCTAssertEqual(result.constraintResults[0].aValue, 3)
    XCTAssertEqual(result.constraintResults[0].bValue, 4)
    XCTAssertEqual(result.constraintResults[0].cValue, 12)
  }
  
  /// Test validating a constraint that fails
  func testInvalidWitness() {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))  // w1
    r1cs.addWire(labelId: LabelID(rawValue: 2))  // w2
    r1cs.addWire(labelId: LabelID(rawValue: 3))  // w3
    
    // Constraint: w1 * w2 = w3
    let constraint = R1CSConstraint(
      a: LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 1)]),
      b: LinearCombination(terms: [(wire: WireID(rawValue: 2), coefficient: 1)]),
      c: LinearCombination(terms: [(wire: WireID(rawValue: 3), coefficient: 1)])
    )
    r1cs.addConstraint(constraint)
    
    // Invalid witness: 3 * 4 != 13
    let witness: [BigUInt] = [1, 3, 4, 13]
    
    let validator = WitnessValidator(r1cs: r1cs, witness: witness)
    let result = validator.validate()
    
    XCTAssertFalse(result.isValid)
    XCTAssertEqual(result.constraintResults.count, 1)
    XCTAssertFalse(result.constraintResults[0].isSatisfied)
    XCTAssertEqual(result.constraintResults[0].actualProduct, 12)
    XCTAssertEqual(result.constraintResults[0].cValue, 13)
  }
  
  /// Test with modular arithmetic
  func testModularArithmetic() {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))  // w1
    r1cs.addWire(labelId: LabelID(rawValue: 2))  // w2
    r1cs.addWire(labelId: LabelID(rawValue: 3))  // w3
    
    // Constraint: w1 * w2 = w3
    let constraint = R1CSConstraint(
      a: LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 1)]),
      b: LinearCombination(terms: [(wire: WireID(rawValue: 2), coefficient: 1)]),
      c: LinearCombination(terms: [(wire: WireID(rawValue: 3), coefficient: 1)])
    )
    r1cs.addConstraint(constraint)
    
    // Witness: 10 * 10 = 100 = 15 (mod 17)
    let witness: [BigUInt] = [1, 10, 10, 15]
    
    let validator = WitnessValidator(r1cs: r1cs, witness: witness)
    let result = validator.validate()
    
    XCTAssertTrue(result.isValid)
    XCTAssertEqual(result.constraintResults[0].actualProduct, 15)
  }
  
  /// Test constraint with constant wire
  func testConstraintWithConstant() {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))  // w1
    
    // Constraint: w1 * 1 = 5  (i.e., w1 = 5)
    let constraint = R1CSConstraint(
      a: LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 1)]),
      b: LinearCombination(terms: [(wire: WireID(rawValue: 0), coefficient: 1)]),  // constant 1
      c: LinearCombination(terms: [(wire: WireID(rawValue: 0), coefficient: 5)])   // constant 5
    )
    r1cs.addConstraint(constraint)
    
    // Valid witness: w1 = 5
    let validWitness: [BigUInt] = [1, 5]
    let validator1 = WitnessValidator(r1cs: r1cs, witness: validWitness)
    let result1 = validator1.validate()
    XCTAssertTrue(result1.isValid)
    
    // Invalid witness: w1 = 6
    let invalidWitness: [BigUInt] = [1, 6]
    let validator2 = WitnessValidator(r1cs: r1cs, witness: invalidWitness)
    let result2 = validator2.validate()
    XCTAssertFalse(result2.isValid)
  }
  
  /// Test with linear combinations having multiple terms
  func testLinearCombinationConstraint() {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))  // w1
    r1cs.addWire(labelId: LabelID(rawValue: 2))  // w2
    r1cs.addWire(labelId: LabelID(rawValue: 3))  // w3
    
    // Constraint: (2*w1 + 3*w2) * w3 = 5
    let constraint = R1CSConstraint(
      a: LinearCombination(terms: [
        (wire: WireID(rawValue: 1), coefficient: 2),
        (wire: WireID(rawValue: 2), coefficient: 3)
      ]),
      b: LinearCombination(terms: [(wire: WireID(rawValue: 3), coefficient: 1)]),
      c: LinearCombination(terms: [(wire: WireID(rawValue: 0), coefficient: 5)])
    )
    r1cs.addConstraint(constraint)
    
    // Witness: w1=1, w2=1, w3=5
    // (2*1 + 3*1) * 5 = 5 * 5 = 25 = 8 (mod 17)... not 5
    // Let's try: w1=1, w2=0, w3=2.5... but we need integers
    // (2*1 + 3*0) * w3 = 5 => 2 * w3 = 5
    // w3 needs to be 5/2 (mod 17) = 5 * 9 (mod 17) = 45 = 11 (mod 17)
    // since 2 * 9 = 18 = 1 (mod 17), so 9 is the inverse of 2
    
    let witness: [BigUInt] = [1, 1, 0, 11]
    
    let validator = WitnessValidator(r1cs: r1cs, witness: witness)
    let result = validator.validate()
    
    XCTAssertTrue(result.isValid)
    XCTAssertEqual(result.constraintResults[0].aValue, 2)
    XCTAssertEqual(result.constraintResults[0].bValue, 11)
    XCTAssertEqual(result.constraintResults[0].actualProduct, 5)
  }
  
  /// Test multiple constraints
  func testMultipleConstraints() {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))  // w1
    r1cs.addWire(labelId: LabelID(rawValue: 2))  // w2
    r1cs.addWire(labelId: LabelID(rawValue: 3))  // w3
    r1cs.addWire(labelId: LabelID(rawValue: 4))  // w4
    
    // Constraint 0: w1 * w2 = w3
    r1cs.addConstraint(R1CSConstraint(
      a: LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 1)]),
      b: LinearCombination(terms: [(wire: WireID(rawValue: 2), coefficient: 1)]),
      c: LinearCombination(terms: [(wire: WireID(rawValue: 3), coefficient: 1)])
    ))
    
    // Constraint 1: w3 * w1 = w4
    r1cs.addConstraint(R1CSConstraint(
      a: LinearCombination(terms: [(wire: WireID(rawValue: 3), coefficient: 1)]),
      b: LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 1)]),
      c: LinearCombination(terms: [(wire: WireID(rawValue: 4), coefficient: 1)])
    ))
    
    // Witness: w1=2, w2=3, w3=6, w4=12
    let witness: [BigUInt] = [1, 2, 3, 6, 12]
    
    let validator = WitnessValidator(r1cs: r1cs, witness: witness)
    let result = validator.validate()
    
    XCTAssertTrue(result.isValid)
    XCTAssertEqual(result.constraintResults.count, 2)
    XCTAssertTrue(result.constraintResults[0].isSatisfied)
    XCTAssertTrue(result.constraintResults[1].isSatisfied)
  }
  
  /// Test with one failing constraint among multiple
  func testPartialFailure() {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))  // w1
    r1cs.addWire(labelId: LabelID(rawValue: 2))  // w2
    r1cs.addWire(labelId: LabelID(rawValue: 3))  // w3
    r1cs.addWire(labelId: LabelID(rawValue: 4))  // w4
    
    // Constraint 0: w1 * w2 = w3
    r1cs.addConstraint(R1CSConstraint(
      a: LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 1)]),
      b: LinearCombination(terms: [(wire: WireID(rawValue: 2), coefficient: 1)]),
      c: LinearCombination(terms: [(wire: WireID(rawValue: 3), coefficient: 1)])
    ))
    
    // Constraint 1: w3 * w1 = w4
    r1cs.addConstraint(R1CSConstraint(
      a: LinearCombination(terms: [(wire: WireID(rawValue: 3), coefficient: 1)]),
      b: LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 1)]),
      c: LinearCombination(terms: [(wire: WireID(rawValue: 4), coefficient: 1)])
    ))
    
    // Witness: w1=2, w2=3, w3=6, w4=13 (should be 12)
    let witness: [BigUInt] = [1, 2, 3, 6, 13]
    
    let validator = WitnessValidator(r1cs: r1cs, witness: witness)
    let result = validator.validate()
    
    XCTAssertFalse(result.isValid)
    XCTAssertEqual(result.constraintResults.count, 2)
    XCTAssertTrue(result.constraintResults[0].isSatisfied)
    XCTAssertFalse(result.constraintResults[1].isSatisfied)
  }
  
  /// Test witness structure validation
  func testWitnessStructureValidation() {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))
    r1cs.addWire(labelId: LabelID(rawValue: 2))
    
    // Test wrong length
    let wrongLength: [BigUInt] = [1, 5]  // Too short
    let validator1 = WitnessValidator(r1cs: r1cs, witness: wrongLength)
    let error1 = validator1.validateWitnessStructure()
    XCTAssertNotNil(error1)
    XCTAssertTrue(error1!.contains("length mismatch"))
    
    // Test wrong constant wire value
    let wrongConstant: [BigUInt] = [0, 5, 10]
    let validator2 = WitnessValidator(r1cs: r1cs, witness: wrongConstant)
    let error2 = validator2.validateWitnessStructure()
    XCTAssertNotNil(error2)
    XCTAssertTrue(error2!.contains("constant wire"))
    
    // Test value exceeds prime
    let exceedsPrime: [BigUInt] = [1, 5, 20]
    let validator3 = WitnessValidator(r1cs: r1cs, witness: exceedsPrime)
    let error3 = validator3.validateWitnessStructure()
    XCTAssertNotNil(error3)
    XCTAssertTrue(error3!.contains("exceeds field prime"))
    
    // Test valid structure
    let valid: [BigUInt] = [1, 5, 10]
    let validator4 = WitnessValidator(r1cs: r1cs, witness: valid)
    let error4 = validator4.validateWitnessStructure()
    XCTAssertNil(error4)
  }
  
  /// Test parsing witness from JSON
  func testWitnessFromJSON() throws {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    r1cs.addWire(labelId: LabelID(rawValue: 1))
    r1cs.addWire(labelId: LabelID(rawValue: 2))
    
    let json = """
    ["1", "5", "10"]
    """
    
    let validator = try WitnessValidator.fromJSON(r1cs: r1cs, witnessJSON: json)
    XCTAssertEqual(validator.witness.count, 3)
    XCTAssertEqual(validator.witness[0], 1)
    XCTAssertEqual(validator.witness[1], 5)
    XCTAssertEqual(validator.witness[2], 10)
  }
  
  /// Test parsing witness with large numbers
  func testWitnessFromJSONWithLargeNumbers() throws {
    let bn254Prime = BigUInt("21888242871839275222246405745257275088548364400416034343698204186575808495617", radix: 10)!
    var r1cs = R1CS(prime: bn254Prime)
    r1cs.addWire(labelId: LabelID(rawValue: 1))
    r1cs.addWire(labelId: LabelID(rawValue: 2))
    
    let json = """
    ["1", "123456789012345678901234567890", "987654321098765432109876543210"]
    """
    
    let validator = try WitnessValidator.fromJSON(r1cs: r1cs, witnessJSON: json)
    XCTAssertEqual(validator.witness.count, 3)
    XCTAssertEqual(validator.witness[0], 1)
    XCTAssertEqual(validator.witness[1], BigUInt("123456789012345678901234567890", radix: 10)!)
    XCTAssertEqual(validator.witness[2], BigUInt("987654321098765432109876543210", radix: 10)!)
  }
  
  /// Test empty linear combination (should evaluate to 0)
  func testEmptyLinearCombination() {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))
    
    // Constraint: w1 * 0 = 0
    let constraint = R1CSConstraint(
      a: LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 1)]),
      b: LinearCombination(),  // Empty = 0
      c: LinearCombination()   // Empty = 0
    )
    r1cs.addConstraint(constraint)
    
    let witness: [BigUInt] = [1, 5]  // Any value for w1 should work
    
    let validator = WitnessValidator(r1cs: r1cs, witness: witness)
    let result = validator.validate()
    
    XCTAssertTrue(result.isValid)
    XCTAssertEqual(result.constraintResults[0].bValue, 0)
    XCTAssertEqual(result.constraintResults[0].cValue, 0)
  }
  
  /// Test with BN254 field (common in ZK proofs)
  func testWithBN254Field() {
    let bn254Prime = BigUInt("21888242871839275222246405745257275088548364400416034343698204186575808495617", radix: 10)!
    var r1cs = R1CS(prime: bn254Prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))
    r1cs.addWire(labelId: LabelID(rawValue: 2))
    r1cs.addWire(labelId: LabelID(rawValue: 3))
    
    // Constraint: w1 * w2 = w3
    let constraint = R1CSConstraint(
      a: LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 1)]),
      b: LinearCombination(terms: [(wire: WireID(rawValue: 2), coefficient: 1)]),
      c: LinearCombination(terms: [(wire: WireID(rawValue: 3), coefficient: 1)])
    )
    r1cs.addConstraint(constraint)
    
    // Test with large numbers
    let largeNum1 = BigUInt("12345678901234567890", radix: 10)!
    let largeNum2 = BigUInt("98765432109876543210", radix: 10)!
    let product = (largeNum1 * largeNum2) % bn254Prime
    
    let witness: [BigUInt] = [1, largeNum1, largeNum2, product]
    
    let validator = WitnessValidator(r1cs: r1cs, witness: witness)
    let result = validator.validate()
    
    XCTAssertTrue(result.isValid)
  }
  
  /// Test debugging output format
  func testDebugOutput() {
    let prime = BigUInt(17)
    var r1cs = R1CS(prime: prime)
    
    r1cs.addWire(labelId: LabelID(rawValue: 1))
    r1cs.addWire(labelId: LabelID(rawValue: 2))
    
    let constraint = R1CSConstraint(
      a: LinearCombination(terms: [(wire: WireID(rawValue: 1), coefficient: 1)]),
      b: LinearCombination(terms: [(wire: WireID(rawValue: 0), coefficient: 1)]),
      c: LinearCombination(terms: [(wire: WireID(rawValue: 0), coefficient: 5)])
    )
    r1cs.addConstraint(constraint)
    
    let witness: [BigUInt] = [1, 5, 0]
    
    let validator = WitnessValidator(r1cs: r1cs, witness: witness)
    let result = validator.validate()
    
    let debugOutput = result.debugDescription
    XCTAssertTrue(debugOutput.contains("Witness Validation Results"))
    XCTAssertTrue(debugOutput.contains("✓"))
  }
}
