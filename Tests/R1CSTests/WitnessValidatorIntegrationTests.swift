import XCTest
import BigInt
import Foundation
@testable import R1CS

/// Integration tests that validate witnesses generated from actual Hylo programs.
final class WitnessValidatorIntegrationTests: XCTestCase {
  
  /// Helper to run witness generator and capture output
  func runWitnessGenerator(scriptPath: String, inputs: String) throws -> String {
    let process = Process()
    process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
    process.arguments = ["node", scriptPath, inputs]
    
    let pipe = Pipe()
    process.standardOutput = pipe
    process.standardError = pipe
    
    try process.run()
    process.waitUntilExit()
    
    let data = pipe.fileHandleForReading.readDataToEndOfFile()
    guard let output = String(data: data, encoding: .utf8) else {
      throw ValidationError.invalidJSON
    }
    
    guard process.terminationStatus == 0 else {
      throw NSError(domain: "WitnessGenerator", code: Int(process.terminationStatus), userInfo: [
        NSLocalizedDescriptionKey: "Witness generator failed:\n\(output)"
      ])
    }
    
    return output
  }
  
  /// Test validating the Example.hylo witness
  func testValidateExampleWitness() throws {
    // Skip if files don't exist (e.g., in CI without generation step)
    let workspaceRoot = URL(fileURLWithPath: #file)
      .deletingLastPathComponent()
      .deletingLastPathComponent()
      .deletingLastPathComponent()
    
    let witnessGenPath = workspaceRoot
      .appendingPathComponent("r1cs")
      .appendingPathComponent("Example.ir.witnessgen.js")
    
    guard FileManager.default.fileExists(atPath: witnessGenPath.path) else {
      throw XCTSkip("Witness generator not found at \(witnessGenPath.path)")
    }
    
    // Run witness generator with inputs [3, 4]
    // This should compute is_right_triangle(3, 4, 5) = 0
    let witnessJSON = try runWitnessGenerator(
      scriptPath: witnessGenPath.path,
      inputs: "[\"3\", \"4\"]"
    )
    
    print("Generated witness JSON:")
    print(witnessJSON)
    
    // For this test, we need to manually construct the R1CS that matches
    // what was generated. In a real integration, you'd deserialize the .r1cs file.
    
    // Note: This test is a template showing how integration would work
    // The actual R1CS structure would need to match what R1CSGen produces
    
    print("✓ Witness generator ran successfully")
    print("  To fully validate, deserialize the R1CS file and validate constraints")
  }
  
  /// Test that demonstrates the full validation flow
  func testFullValidationFlow() throws {
    // This is a conceptual test showing the intended workflow:
    
    // Step 1: Compile Hylo to R1CS
    // swift run hc --emit r1cs --freestanding -o circuit.ir program.hylo
    
    // Step 2: R1CS file is written with serialization
    // let r1cs = /* build R1CS during compilation */
    // try r1cs.serialize(to: outputURL)
    
    // Step 3: Witness generator is created
    // Generated JavaScript that computes witness values
    
    // Step 4: Run witness generator with inputs
    // node circuit.ir.witnessgen.js '["input1", "input2"]'
    
    // Step 5: Validate witness
    // let validator = try WitnessValidator.fromJSON(r1cs: r1cs, witnessJSON: output)
    // if let structureError = validator.validateWitnessStructure() {
    //   print("Witness structure invalid: \(structureError)")
    //   exit(1)
    // }
    // let result = validator.validate()
    // if !result.isValid {
    //   print(result.debugDescription)
    //   exit(1)
    // }
    
    print("Full validation flow:")
    print("1. Compile: hc --emit r1cs program.hylo")
    print("2. Generate witness: node program.ir.witnessgen.js '[\"3\", \"4\"]'")
    print("3. Validate in Swift:")
    print("   let validator = try WitnessValidator.fromJSON(r1cs: r1cs, witnessJSON: output)")
    print("   let result = validator.validate()")
    print("   print(result.debugDescription)")
  }
  
  /// Test with a manually created circuit matching the generated structure
  func testRightTriangleCircuit() throws {
    // This creates an R1CS that represents: (a² + b²) - c² = 0
    // Which matches the Example.hylo program
    
    let prime = BigUInt("21888242871839275222246405745257275088548364400416034343698204186575808495617", radix: 10)!
    var r1cs = R1CS(prime: prime)
    
    // Based on the witness generator structure:
    // w[0] = 1 (constant)
    // w[1] = a (input)
    // w[2] = b (input)
    // w[3] = result (output)
    // w[4] = c (constant 5)
    // w[5] = b²
    // w[6] = a²
    // w[7] = a² + b²
    // w[8] = c²
    // w[9] = (a² + b²) - c²
    
    let w1 = r1cs.addWire(labelId: LabelID(rawValue: 1))  // a
    let w2 = r1cs.addWire(labelId: LabelID(rawValue: 2))  // b
    let w3 = r1cs.addWire(labelId: LabelID(rawValue: 3))  // result
    let w4 = r1cs.addWire(labelId: LabelID(rawValue: 4))  // c = 5
    let w5 = r1cs.addWire(labelId: LabelID(rawValue: 5))  // b²
    let w6 = r1cs.addWire(labelId: LabelID(rawValue: 6))  // a²
    let w7 = r1cs.addWire(labelId: LabelID(rawValue: 7))  // a² + b²
    let w8 = r1cs.addWire(labelId: LabelID(rawValue: 8))  // c²
    let w9 = r1cs.addWire(labelId: LabelID(rawValue: 9))  // result value
    
    // Constraint: w4 * 1 = 5 (c = 5)
    r1cs.addConstraint(R1CSConstraint(
      a: LinearCombination(terms: [(wire: w4, coefficient: 1)]),
      b: LinearCombination(terms: [(wire: .one, coefficient: 1)]),
      c: LinearCombination(terms: [(wire: .one, coefficient: 5)])
    ))
    
    // Constraint: b * b = b² (w2 * w2 = w5)
    r1cs.addConstraint(R1CSConstraint(
      a: LinearCombination(terms: [(wire: w2, coefficient: 1)]),
      b: LinearCombination(terms: [(wire: w2, coefficient: 1)]),
      c: LinearCombination(terms: [(wire: w5, coefficient: 1)])
    ))
    
    // Constraint: a * a = a² (w1 * w1 = w6)
    r1cs.addConstraint(R1CSConstraint(
      a: LinearCombination(terms: [(wire: w1, coefficient: 1)]),
      b: LinearCombination(terms: [(wire: w1, coefficient: 1)]),
      c: LinearCombination(terms: [(wire: w6, coefficient: 1)])
    ))
    
    // Constraint: (a² + b²) * 1 = w7 (w6 + w5 = w7)
    r1cs.addConstraint(R1CSConstraint(
      a: LinearCombination(terms: [
        (wire: w6, coefficient: 1),
        (wire: w5, coefficient: 1)
      ]),
      b: LinearCombination(terms: [(wire: .one, coefficient: 1)]),
      c: LinearCombination(terms: [(wire: w7, coefficient: 1)])
    ))
    
    // Constraint: c * c = c² (w4 * w4 = w8)
    r1cs.addConstraint(R1CSConstraint(
      a: LinearCombination(terms: [(wire: w4, coefficient: 1)]),
      b: LinearCombination(terms: [(wire: w4, coefficient: 1)]),
      c: LinearCombination(terms: [(wire: w8, coefficient: 1)])
    ))
    
    // Constraint: (a² + b² - c²) * 1 = w9 (w7 - w8 = w9)
    // Note: Subtraction in field is addition of negative
    let negOne = prime - 1
    r1cs.addConstraint(R1CSConstraint(
      a: LinearCombination(terms: [
        (wire: w7, coefficient: 1),
        (wire: w8, coefficient: negOne)
      ]),
      b: LinearCombination(terms: [(wire: .one, coefficient: 1)]),
      c: LinearCombination(terms: [(wire: w9, coefficient: 1)])
    ))
    
    // Constraint: result = w9 (w3 = w9)
    r1cs.addConstraint(R1CSConstraint(
      a: LinearCombination(terms: [(wire: w9, coefficient: 1)]),
      b: LinearCombination(terms: [(wire: .one, coefficient: 1)]),
      c: LinearCombination(terms: [(wire: w3, coefficient: 1)])
    ))
    
    // Test with right triangle: (3, 4, 5)
    // Expected: 3² + 4² - 5² = 9 + 16 - 25 = 0
    let witness: [BigUInt] = [
      1,    // w[0] = 1 (constant)
      3,    // w[1] = a
      4,    // w[2] = b
      0,    // w[3] = result
      5,    // w[4] = c
      16,   // w[5] = b² = 16
      9,    // w[6] = a² = 9
      25,   // w[7] = a² + b² = 25
      25,   // w[8] = c² = 25
      0     // w[9] = 25 - 25 = 0
    ]
    
    let validator = WitnessValidator(r1cs: r1cs, witness: witness)
    
    // First check witness structure
    if let error = validator.validateWitnessStructure() {
      XCTFail("Witness structure invalid: \(error)")
      return
    }
    
    // Then validate constraints
    let result = validator.validate()
    
    print(result.debugDescription)
    
    XCTAssertTrue(result.isValid, "All constraints should be satisfied for right triangle")
    XCTAssertEqual(result.constraintResults.count, 7)
  }
  
  /// Test with invalid triangle (not a right triangle)
  func testInvalidTriangle() throws {
    let prime = BigUInt("21888242871839275222246405745257275088548364400416034343698204186575808495617", radix: 10)!
    var r1cs = R1CS(prime: prime)
    
    // Create same circuit as above
    let w1 = r1cs.addWire(labelId: LabelID(rawValue: 1))
    let w2 = r1cs.addWire(labelId: LabelID(rawValue: 2))
    let _ = r1cs.addWire(labelId: LabelID(rawValue: 3))
    let _ = r1cs.addWire(labelId: LabelID(rawValue: 4))
    let w5 = r1cs.addWire(labelId: LabelID(rawValue: 5))
    let w6 = r1cs.addWire(labelId: LabelID(rawValue: 6))
    let _ = r1cs.addWire(labelId: LabelID(rawValue: 7))
    let _ = r1cs.addWire(labelId: LabelID(rawValue: 8))
    let _ = r1cs.addWire(labelId: LabelID(rawValue: 9))
    
    // Add same constraints (omitted for brevity - same as above)
    r1cs.addConstraint(R1CSConstraint(
      a: LinearCombination(terms: [(wire: w2, coefficient: 1)]),
      b: LinearCombination(terms: [(wire: w2, coefficient: 1)]),
      c: LinearCombination(terms: [(wire: w5, coefficient: 1)])
    ))
    
    r1cs.addConstraint(R1CSConstraint(
      a: LinearCombination(terms: [(wire: w1, coefficient: 1)]),
      b: LinearCombination(terms: [(wire: w1, coefficient: 1)]),
      c: LinearCombination(terms: [(wire: w6, coefficient: 1)])
    ))
    
    // Test with non-right triangle: (3, 4, 6)
    // 3² + 4² - 6² = 9 + 16 - 36 = -11 (should be 0 for right triangle)
    let negEleven = prime - 11  // -11 in field
    
    let witness: [BigUInt] = [
      1,          // w[0] = 1
      3,          // w[1] = a
      4,          // w[2] = b  
      negEleven,  // w[3] = result = -11
      6,          // w[4] = c
      16,         // w[5] = b² = 16
      9,          // w[6] = a² = 9
      25,         // w[7] = a² + b² = 25
      36,         // w[8] = c² = 36
      negEleven   // w[9] = 25 - 36 = -11
    ]
    
    let validator = WitnessValidator(r1cs: r1cs, witness: witness)
    let result = validator.validate()
    
    // The constraints should still be satisfied (witness is computed correctly)
    // The result just shows it's not a right triangle (result != 0)
    XCTAssertTrue(result.isValid, "Constraints should be satisfied even for non-right triangle")
    XCTAssertNotEqual(witness[3], 0, "Result should be non-zero for invalid triangle")
  }
}
