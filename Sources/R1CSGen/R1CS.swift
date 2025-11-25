import Foundation
import BigInt

/// A unique identifier for a wire in an R1CS constraint system.
public struct WireID: Equatable, Hashable, Comparable {
  internal let rawValue: UInt32
  
  internal init(rawValue: UInt32) {
    self.rawValue = rawValue
  }
  
  public static func < (lhs: WireID, rhs: WireID) -> Bool {
    lhs.rawValue < rhs.rawValue
  }
}

/// A unique identifier for a label in the circuit.
public struct LabelID: Equatable, Hashable {
  internal let rawValue: UInt64
  
  internal init(rawValue: UInt64) {
    self.rawValue = rawValue
  }
}

/// A mutable R1CS constraint system that can be built incrementally.
public struct R1CS {
  /// The prime field modulus.
  public var prime: BigUInt
  
  /// Number of wires including the constant ONE wire at index 0.
  public var wireCount: UInt32 = 1  // Start with wire 0 (constant 1)
  
  /// Number of public output wires.
  public var publicOutputCount: UInt32 = 0
  
  /// Number of public input wires.
  public var publicInputCount: UInt32 = 0
  
  /// Number of private input wires.
  public var privateInputCount: UInt32 = 0
  
  /// Total number of labels in the circuit.
  public var labelCount: UInt64 = 0
  
  /// The constraints in the system.
  public var constraints: [R1CSConstraint] = []
  
  /// Mapping from wire ID to label ID.
  /// The label for a wire with ID `w` is stored at `wireToLabelMap[w]`.
  /// Wire 0 (the constant 1) maps to label 0.
  internal var wireToLabelMap: [LabelID] = [LabelID(rawValue: 0)]  // Wire 0 maps to label 0
  
  /// Initializes a new R1CS constraint system with the given prime field.
  ///
  /// - Parameter prime: The prime modulus for the field.
  public init(prime: BigUInt) {
    self.prime = prime
  }
  
  /// Allocates and returns a new unique label ID.
  ///
  /// - Returns: A new label ID that can be used for circuit labels.
  public mutating func nextLabel() -> LabelID {
    let label = LabelID(rawValue: labelCount)
    labelCount += 1
    return label
  }
  
  /// Adds a new wire to the system.
  ///
  /// - Parameter labelId: The label ID to map this wire to.
  /// - Returns: The wire ID of the newly added wire.
  @discardableResult
  public mutating func addWire(labelId: LabelID) -> WireID {
    let wireId = WireID(rawValue: wireCount)
    wireCount += 1
    wireToLabelMap.append(labelId)
    
    if labelId.rawValue >= labelCount {
      labelCount = labelId.rawValue + 1
    }
    
    return wireId
  }
  
  /// Adds a constraint to the system.
  ///
  /// - Parameter constraint: The constraint to add.
  public mutating func addConstraint(_ constraint: R1CSConstraint) {
    constraints.append(constraint)
  }
}

/// Represents a single R1CS constraint of the form: A * B - C = 0
public struct R1CSConstraint {
  /// The A linear combination.
  public var a: LinearCombination
  
  /// The B linear combination.
  public var b: LinearCombination
  
  /// The C linear combination.
  public var c: LinearCombination
  
  /// Initializes a new constraint.
  public init(a: LinearCombination, b: LinearCombination, c: LinearCombination) {
    self.a = a
    self.b = b
    self.c = c
  }
}

/// A linear combination of wires with field coefficients.
public struct LinearCombination {
  /// The terms in the linear combination, mapping wire ID to coefficient.
  public var terms: [(wireId: WireID, coefficient: BigUInt)]
  
  /// Initializes an empty linear combination.
  public init() {
    self.terms = []
  }
  
  /// Initializes a linear combination with the given terms.
  ///
  /// - Parameter terms: The terms.
  public init(terms: [(wireId: WireID, coefficient: BigUInt)]) {
    self.terms = terms
  }
  
  /// Adds a term to the linear combination.
  ///
  /// - Parameters:
  ///   - wireId: The wire ID.
  ///   - coefficient: The coefficient for this wire.
  public mutating func addTerm(wireId: WireID, coefficient: BigUInt) {
    terms.append((wireId, coefficient))
  }
}
