import BigInt
import Foundation

/// A unique identifier for a wire in an R1CS constraint system.
public struct WireID: Equatable, Hashable, Comparable, Sendable {
  public let raw: UInt32

  internal init(rawValue: UInt32) {
    self.raw = rawValue
  }

  public static func < (lhs: WireID, rhs: WireID) -> Bool {
    lhs.raw < rhs.raw
  }

  public static let one = WireID(rawValue: 0)
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
  /// The wire that is always set to 1 (the constant wire).
  public static let unitWire = WireID.one

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
    let wire = WireID(rawValue: wireCount)
    wireCount += 1
    wireToLabelMap.append(labelId)

    if labelId.rawValue >= labelCount {
      labelCount = labelId.rawValue + 1
    }

    return wire
  }

  /// Adds a new wire to the system with a newly allocated label.
  @discardableResult
  public mutating func addWire() -> WireID {
    let labelId = nextLabel()
    return addWire(labelId: labelId)
  }

  /// Adds a constraint to the system.
  ///
  /// - Parameter constraint: The constraint to add.
  public mutating func addConstraint(_ constraint: R1CSConstraint) {
    let normalizedConstraint = R1CSConstraint(
      a: normalizeLinearCombination(constraint.a),
      b: normalizeLinearCombination(constraint.b),
      c: normalizeLinearCombination(constraint.c)
    )
    constraints.append(normalizedConstraint)
  }
  
  /// Normalizes a linear combination by collapsing duplicate wires and applying modular arithmetic.
  private func normalizeLinearCombination(_ lc: LinearCombination) -> LinearCombination {
    // Collapse duplicate wires by summing their coefficients
    var wireCoefficients: [WireID: BigUInt] = [:]
    for term in lc.terms {
      if let existingCoeff = wireCoefficients[term.wire] {
        let sum = existingCoeff + term.coefficient
        wireCoefficients[term.wire] = sum % prime
      } else {
        wireCoefficients[term.wire] = term.coefficient % prime
      }
    }
    
    // Convert back to array of terms, filtering out zero coefficients
    let normalizedTerms = wireCoefficients.compactMap { (wire, coefficient) in
      coefficient == 0 ? nil : (wire: wire, coefficient: coefficient)
    }.sorted { $0.wire < $1.wire }
    
    return LinearCombination(terms: normalizedTerms)
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

  /// Constraints a given wire to equal a constant value.
  public static func constant(wire: WireID, value: BigUInt) -> R1CSConstraint {
    .init(
      a: LinearCombination(terms: [(wire: wire, coefficient: 1)]),
      b: LinearCombination(terms: [(wire: .one, coefficient: 1)]),
      c: LinearCombination(terms: [(wire: .one, coefficient: .init(value))])
    )
  }
}

/// A linear combination of wires with field coefficients.
public struct LinearCombination: Sendable {
  /// The terms in the linear combination, mapping wire ID to coefficient.
  public var terms: [(wire: WireID, coefficient: BigUInt)]

  /// Initializes an empty linear combination.
  public init() {
    self.terms = []
  }

  public static let one = LinearCombination(terms: [(wire: .one, coefficient: 1)])
  public static let zero = LinearCombination(terms: [])

  public static func constant(_ value: BigUInt) -> LinearCombination {
    LinearCombination(terms: [(wire: .one, coefficient: value)])
  }

  public static func wire(_ value: WireID) -> LinearCombination {
    LinearCombination(terms: [(wire: value, coefficient: 1)])
  }

  /// Initializes a linear combination with the given terms.
  ///
  /// - Parameter terms: The terms.
  public init(terms: [(wire: WireID, coefficient: BigUInt)]) {
    self.terms = terms
  }

  /// Adds a term to the linear combination.
  ///
  /// - Parameters:
  ///   - wire: The wire ID.
  ///   - coefficient: The coefficient for this wire.
  public mutating func addTerm(wire: WireID, coefficient: BigUInt) {
    terms.append((wire, coefficient))
  }
}

extension R1CS {
  public func numberToField(_ number: BigInt) -> BigUInt {
    var number = number
    while number < 0 {
      number = BigInt(prime) + number
    }
    return BigUInt(number)
  }
}
// MARK: - Debug Descriptions

extension R1CS: CustomDebugStringConvertible {
  public var debugDescription: String {
    var output = ""

    // ANSI color codes
    let reset = "\u{001B}[0m"
    let bold = "\u{001B}[1m"
    let cyan = "\u{001B}[36m"
    let green = "\u{001B}[32m"
    let yellow = "\u{001B}[33m"
    let blue = "\u{001B}[34m"
    let dim = "\u{001B}[2m"

    // Header
    output +=
      "\(bold)\(cyan)╔════════════════════════════════════════╗\(reset)\n"
    output +=
      "\(bold)\(cyan)║\(reset)  \(bold)R1CS Constraint System\(reset)                \(bold)\(cyan)║\(reset)\n"
    output +=
      "\(bold)\(cyan)╚════════════════════════════════════════╝\(reset)\n\n"

    // Field information
    output += "\(bold)\(green)Field:\(reset)\n"
    output += "  Prime: \(prime)\n"
    let primeBits = prime.bitWidth
    output += "  \(dim)(\(primeBits) bits)\(reset)\n\n"

    // Wire statistics
    output += "\(bold)\(green)Wires:\(reset)\n"
    output += "  Total:          \(wireCount)\n"
    output += "  Public Output:  \(publicOutputCount)\n"
    output += "  Public Input:   \(publicInputCount)\n"
    output += "  Private Input:  \(privateInputCount)\n"
    output += "  Labels:         \(labelCount)\n\n"

    // Wire to Label mapping (show first few and last few if many)
    output += "\(bold)\(green)Wire → Label Mapping:\(reset)\n"
    let maxWiresToShow = 10
    if wireToLabelMap.count <= maxWiresToShow {
      for (wire, labelId) in wireToLabelMap.enumerated() {
        let wireStr = wire == 0 ? "\(yellow)w\(wire) (constant 1)\(reset)" : "w\(wire)"
        output += "  \(wireStr) → l\(labelId.rawValue)\n"
      }
    } else {
      for wire in 0..<5 {
        let labelId = wireToLabelMap[wire]
        let wireStr = wire == 0 ? "\(yellow)w\(wire) (constant 1)\(reset)" : "w\(wire)"
        output += "  \(wireStr) → l\(labelId.rawValue)\n"
      }
      output += "  \(dim)... (\(wireToLabelMap.count - 10) more) ...\(reset)\n"
      for wire in (wireToLabelMap.count - 5)..<wireToLabelMap.count {
        let labelId = wireToLabelMap[wire]
        output += "  w\(wire) → l\(labelId.rawValue)\n"
      }
    }
    output += "\n"

    // Constraints
    output += "\(bold)\(green)Constraints:\(reset) \(constraints.count) total\n\n"

    for (index, constraint) in constraints.enumerated() {
      output += "\(bold)\(blue)[\(index)]\(reset) "
      output += "\(constraint.debugDescription(showEquation: true))\n\n"
    }

    if constraints.isEmpty {
      output += "  \(dim)(no constraints)\(reset)\n\n"
    }

    return output
  }
}

extension R1CSConstraint: CustomDebugStringConvertible {
  public var debugDescription: String {
    debugDescription(showEquation: false)
  }

  func debugDescription(showEquation: Bool) -> String {
    var output = ""

    let reset = "\u{001B}[0m"
    let magenta = "\u{001B}[35m"
    let cyan = "\u{001B}[36m"

    if showEquation {
      // Show as equation: A * B = C
      let aStr = a.debugDescription(compact: true)
      let bStr = b.debugDescription(compact: true)
      let cStr = c.debugDescription(compact: true)

      output += "(\(magenta)\(aStr)\(reset)) × (\(cyan)\(bStr)\(reset)) = (\(cStr))"
    } else {
      let bold = "\u{001B}[1m"
      output += "\(bold)A:\(reset) \(a.debugDescription(compact: false))\n"
      output += "\(bold)B:\(reset) \(b.debugDescription(compact: false))\n"
      output += "\(bold)C:\(reset) \(c.debugDescription(compact: false))"
    }

    return output
  }
}

extension LinearCombination: CustomDebugStringConvertible {
  public var debugDescription: String {
    debugDescription(compact: false)
  }

  func debugDescription(compact: Bool) -> String {
    if terms.isEmpty {
      return "0"
    }

    let reset = "\u{001B}[0m"
    let yellow = "\u{001B}[33m"
    let green = "\u{001B}[32m"
    let dim = "\u{001B}[2m"
    let magenta = "\u{001B}[35m"

    if compact {
      // Compact form for equations
      var parts: [String] = []
      for term in terms {
        let wireStr =
          term.wire.raw == 0 ? "\(yellow)1\(reset)" : "\(green)w\(term.wire.raw)\(reset)"
        if term.coefficient == 1 {
          // Omit coefficient when it's 1
          parts.append(wireStr)
        } else {
          // Show coefficient in magenta
          parts.append("\(magenta)\(term.coefficient)\(reset)\(dim)·\(reset)\(wireStr)")
        }
      }
      return parts.joined(separator: " \(dim)+\(reset) ")
    } else {
      // Detailed form
      var parts: [String] = []
      for term in terms {
        let wireStr =
          term.wire.raw == 0
          ? "\(yellow)w\(term.wire.raw) (1)\(reset)" : "\(green)w\(term.wire.raw)\(reset)"
        parts.append("\(magenta)\(term.coefficient)\(reset) \(dim)·\(reset) \(wireStr)")
      }
      return parts.joined(separator: " \(dim)+\(reset) ")
    }
  }
}

extension WireID: CustomDebugStringConvertible {
  public var debugDescription: String {
    let yellow = "\u{001B}[33m"
    let green = "\u{001B}[32m"
    let reset = "\u{001B}[0m"

    if raw == 0 {
      return "\(yellow)w\(raw) (constant 1)\(reset)"
    } else {
      return "\(green)w\(raw)\(reset)"
    }
  }
}

extension LabelID: CustomDebugStringConvertible {
  public var debugDescription: String {
    "l\(rawValue)"
  }
}
