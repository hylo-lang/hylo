import BigInt
import Foundation

/// Validates that a witness satisfies an R1CS constraint system.
public struct WitnessValidator {
  /// The R1CS constraint system to validate against.
  public let r1cs: R1CS
  
  /// The witness values indexed by wire ID.
  /// Wire 0 should always be 1 (the constant wire).
  public let witness: [BigUInt]
  
  /// Result of validating a witness against the constraint system.
  public struct ValidationResult {
    /// Whether all constraints are satisfied.
    public let isValid: Bool
    
    /// Details about each constraint's validation.
    public let constraintResults: [ConstraintResult]
    
    /// Summary of validation.
    public var summary: String {
      let passed = constraintResults.filter { $0.isSatisfied }.count
      let total = constraintResults.count
      
      if isValid {
        return "✓ All \(total) constraints satisfied"
      } else {
        let failed = total - passed
        return "✗ \(failed) of \(total) constraints FAILED"
      }
    }
  }
  
  /// Result of validating a single constraint.
  public struct ConstraintResult {
    /// The index of this constraint.
    public let index: Int
    
    /// Whether this constraint is satisfied.
    public let isSatisfied: Bool
    
    /// The evaluated value of A (left side of multiplication).
    public let aValue: BigUInt
    
    /// The evaluated value of B (right side of multiplication).
    public let bValue: BigUInt
    
    /// The evaluated value of C (expected result).
    public let cValue: BigUInt
    
    /// The actual product A × B.
    public let actualProduct: BigUInt
    
    /// Error message if constraint is not satisfied.
    public var errorMessage: String? {
      guard !isSatisfied else { return nil }
      return "Constraint \(index): (\(aValue)) × (\(bValue)) = \(actualProduct), expected \(cValue)"
    }
  }
  
  /// Initializes a witness validator.
  ///
  /// - Parameters:
  ///   - r1cs: The R1CS constraint system.
  ///   - witness: The witness values. Should have length equal to `r1cs.wireCount`.
  public init(r1cs: R1CS, witness: [BigUInt]) {
    self.r1cs = r1cs
    self.witness = witness
  }
  
  /// Validates the witness against all constraints.
  ///
  /// - Returns: A validation result containing details about each constraint.
  public func validate() -> ValidationResult {
    var constraintResults: [ConstraintResult] = []
    
    for (index, constraint) in r1cs.constraints.enumerated() {
      let result = validateConstraint(constraint, at: index)
      constraintResults.append(result)
    }
    
    let allSatisfied = constraintResults.allSatisfy { $0.isSatisfied }
    
    return ValidationResult(
      isValid: allSatisfied,
      constraintResults: constraintResults
    )
  }
  
  /// Validates a single constraint.
  ///
  /// - Parameters:
  ///   - constraint: The constraint to validate.
  ///   - index: The index of this constraint (for error reporting).
  /// - Returns: The result of validating this constraint.
  private func validateConstraint(_ constraint: R1CSConstraint, at index: Int) -> ConstraintResult {
    // Evaluate each linear combination
    let aValue = evaluateLinearCombination(constraint.a)
    let bValue = evaluateLinearCombination(constraint.b)
    let cValue = evaluateLinearCombination(constraint.c)
    
    // Compute A × B mod prime
    let product = (aValue * bValue) % r1cs.prime
    
    // Check if A × B = C (mod prime)
    let isSatisfied = product == cValue
    
    return ConstraintResult(
      index: index,
      isSatisfied: isSatisfied,
      aValue: aValue,
      bValue: bValue,
      cValue: cValue,
      actualProduct: product
    )
  }
  
  /// Evaluates a linear combination given the current witness.
  ///
  /// - Parameter lc: The linear combination to evaluate.
  /// - Returns: The value of the linear combination mod prime.
  private func evaluateLinearCombination(_ lc: LinearCombination) -> BigUInt {
    var result = BigUInt(0)
    
    for term in lc.terms {
      let wireID = Int(term.wire.raw)
      
      // Ensure wire ID is within bounds
      guard wireID < witness.count else {
        fatalError("Wire ID \(wireID) out of bounds (witness has \(witness.count) wires)")
      }
      
      let wireValue = witness[wireID]
      let contribution = (term.coefficient * wireValue) % r1cs.prime
      result = (result + contribution) % r1cs.prime
    }
    
    return result
  }
  
  /// Validates that the witness has the correct structure.
  ///
  /// - Returns: An error message if validation fails, nil otherwise.
  public func validateWitnessStructure() -> String? {
    // Check witness length
    if witness.count != Int(r1cs.wireCount) {
      return "Witness length mismatch: got \(witness.count), expected \(r1cs.wireCount)"
    }
    
    // Check that wire 0 is 1
    if witness[0] != 1 {
      return "Wire 0 (constant wire) must be 1, but got \(witness[0])"
    }
    
    // Check that all values are within field
    for (index, value) in witness.enumerated() {
      if value >= r1cs.prime {
        return "Wire \(index) value \(value) exceeds field prime \(r1cs.prime)"
      }
    }
    
    return nil
  }
}

// MARK: - Convenience methods for loading witnesses

extension WitnessValidator {
  /// Creates a validator from a witness JSON file (array of string numbers).
  ///
  /// - Parameters:
  ///   - r1cs: The R1CS constraint system.
  ///   - witnessPath: Path to the JSON file containing the witness.
  /// - Returns: A witness validator.
  /// - Throws: If the file cannot be read or parsed.
  public static func fromJSONFile(r1cs: R1CS, witnessPath: URL) throws -> WitnessValidator {
    let data = try Data(contentsOf: witnessPath)
    let witnessStrings = try JSONDecoder().decode([String].self, from: data)
    
    let witness = try witnessStrings.map { str -> BigUInt in
      guard let value = BigUInt(str, radix: 10) else {
        throw ValidationError.invalidWitnessValue(str)
      }
      return value
    }
    
    return WitnessValidator(r1cs: r1cs, witness: witness)
  }
  
  /// Creates a validator from witness output (the stdout from witness generator).
  ///
  /// - Parameters:
  ///   - r1cs: The R1CS constraint system.
  ///   - witnessJSON: JSON string containing array of numeric strings.
  /// - Returns: A witness validator.
  /// - Throws: If the JSON cannot be parsed.
  public static func fromJSON(r1cs: R1CS, witnessJSON: String) throws -> WitnessValidator {
    guard let data = witnessJSON.data(using: .utf8) else {
      throw ValidationError.invalidJSON
    }
    
    let witnessStrings = try JSONDecoder().decode([String].self, from: data)
    
    let witness = try witnessStrings.map { str -> BigUInt in
      guard let value = BigUInt(str, radix: 10) else {
        throw ValidationError.invalidWitnessValue(str)
      }
      return value
    }
    
    return WitnessValidator(r1cs: r1cs, witness: witness)
  }
}

/// Errors that can occur during witness validation.
public enum ValidationError: Error, CustomStringConvertible {
  case invalidJSON
  case invalidWitnessValue(String)
  case witnessStructureInvalid(String)
  
  public var description: String {
    switch self {
    case .invalidJSON:
      return "Invalid JSON format"
    case .invalidWitnessValue(let str):
      return "Invalid witness value: \(str)"
    case .witnessStructureInvalid(let msg):
      return "Witness structure invalid: \(msg)"
    }
  }
}

// MARK: - Debug output

extension WitnessValidator.ValidationResult: CustomDebugStringConvertible {
  public var debugDescription: String {
    var output = ""
    
    // ANSI color codes
    let reset = "\u{001B}[0m"
    let bold = "\u{001B}[1m"
    let green = "\u{001B}[32m"
    let red = "\u{001B}[31m"
    let cyan = "\u{001B}[36m"
    
    // Header
    output += "\(bold)\(cyan)╔═══════════════════════════════════════════════════════════════╗\(reset)\n"
    output += "\(bold)\(cyan)║\(reset)  \(bold)Witness Validation Results\(reset)                              \(bold)\(cyan)║\(reset)\n"
    output += "\(bold)\(cyan)╚═══════════════════════════════════════════════════════════════╝\(reset)\n\n"
    
    // Summary
    if isValid {
      output += "\(bold)\(green)✓ \(summary)\(reset)\n\n"
    } else {
      output += "\(bold)\(red)✗ \(summary)\(reset)\n\n"
    }
    
    // Constraint details
    output += "\(bold)Constraint Details:\(reset)\n"
    
    for result in constraintResults {
      let status = result.isSatisfied ? "\(green)✓\(reset)" : "\(red)✗\(reset)"
      output += "\(status) [\(result.index)] "
      
      if result.isSatisfied {
        output += "\(result.aValue) × \(result.bValue) = \(result.cValue)\n"
      } else {
        output += "\(red)\(result.errorMessage ?? "Unknown error")\(reset)\n"
      }
    }
    
    return output
  }
}
