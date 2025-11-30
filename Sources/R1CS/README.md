# R1CS

A Swift library for generating R1CS (Rank-1 Constraint System) files in the standard binary format.

## Overview

R1CS (Rank-1 Constraint System) is a representation of algebraic circuits used in zero-knowledge proof systems like zkSNARKs, Bulletproofs, and Aurora. This library provides a type-safe API for building R1CS constraint systems and serializing them to the standard `.r1cs` binary format.

## Features

- **Incremental Builder Pattern**: Build constraint systems step-by-step with `R1CSBuilder`
- **Type-Safe API**: Leverages Swift's type system to prevent common errors
- **Standard Binary Format**: Outputs `.r1cs` files compatible with zkSNARK toolchains
- **Support for Large Fields**: Works with arbitrary-precision field elements via BigInt
- **Efficient Serialization**: Generates compact binary representations

## Usage

### Basic Example

```swift
import BigInt
import R1CS

// Create an R1CS constraint system with a prime field
let prime = BigUInt("21888242871839275222246405745257275088548364400416034343698204186575808495617", radix: 10)!
var r1cs = R1CS(prime: prime)

// Allocate label IDs for circuit signals
let label1 = r1cs.nextLabel()
let label2 = r1cs.nextLabel()
let label3 = r1cs.nextLabel()

// Add wires (wire 0 is automatically created as the constant 1)
let wire1 = r1cs.addWire(labelId: label1)
let wire2 = r1cs.addWire(labelId: label2)
let wire3 = r1cs.addWire(labelId: label3)

// Create a constraint: (3 * wire1) * (2 * wire2) - (6 * wire3) = 0
let a = LinearCombination(terms: [(wireId: wire1, coefficient: 3)])
let b = LinearCombination(terms: [(wireId: wire2, coefficient: 2)])
let c = LinearCombination(terms: [(wireId: wire3, coefficient: 6)])

r1cs.addConstraint(R1CSConstraint(a: a, b: b, c: c))

// Set visibility of wires
r1cs.publicOutputCount = 1
r1cs.publicInputCount = 1
r1cs.privateInputCount = 1

// Write to file
try r1cs.write(to: URL(fileURLWithPath: "output.r1cs"))
```

### Building Linear Combinations

```swift
// Empty linear combination
var lc = LinearCombination()

// Add terms incrementally
lc.addTerm(wireId: WireID(rawValue: 0), coefficient: 1)  // 1 * wire0
lc.addTerm(wireId: WireID(rawValue: 5), coefficient: 3)  // + 3 * wire5

// Or create with initial terms (automatically sorted by wire ID)
let lc2 = LinearCombination(terms: [
  (wireId: WireID(rawValue: 10), coefficient: 7),
  (wireId: WireID(rawValue: 2), coefficient: 4),
  (wireId: WireID(rawValue: 5), coefficient: 9)
])
```

### Creating Constraints

Each constraint represents the equation: A * B - C = 0

```swift
let constraint = R1CSConstraint(
  a: LinearCombination(terms: [(wireId: WireID(rawValue: 1), coefficient: 5)]),
  b: LinearCombination(terms: [(wireId: WireID(rawValue: 2), coefficient: 8)]),
  c: LinearCombination(terms: [(wireId: WireID(rawValue: 3), coefficient: 40)])
)
// This represents: (5 * wire1) * (8 * wire2) - (40 * wire3) = 0
// Which simplifies to: 40 * wire1 * wire2 - 40 * wire3 = 0
```

## File Format

The library outputs files in the standard R1CS binary format with:

- **Magic number**: "r1cs" (0x72316373)
- **Version**: 1
- **Sections**:
  - Header (type 0x01): Field parameters, wire counts, constraint count
  - Constraints (type 0x02): All constraint equations
  - Wire2Label Map (type 0x03): Mapping from wires to circuit labels

For detailed format specification, see the [R1CS Binary Format Standard](https://github.com/iden3/r1csfile).

## Architecture

### `WireID`

A strong type for wire identifiers that prevents accidental confusion with other integer types. The raw value is only accessible internally.

### `LabelID`

A strong type for label identifiers in the circuit. The raw value is only accessible internally.

### `R1CS`

Mutable constraint system for incrementally building R1CS. Allows:
- Allocating unique label IDs via `nextLabel()`
- Adding wires with label mappings
- Adding constraints
- Setting public/private wire counts
- Writing directly to `.r1cs` binary format

**Wire-to-Label Mapping:**
The internal mapping from wires to labels is represented as an array where the index corresponds to the wire ID (raw value), and the value at that index is the associated label ID. For example:
- Wire 0 (constant 1) → Label 0 (stored at index 0)
- Wire 1 → Label stored at `wireToLabelMap[1]`
- Wire N → Label stored at `wireToLabelMap[N]`

This array-based representation provides O(1) lookup during serialization and ensures the wire-to-label relationship is preserved in the binary output.

### `R1CSConstraint`

Represents a single constraint equation of the form A * B - C = 0, where A, B, and C are linear combinations.

### `LinearCombination`

A sparse representation of a linear combination of wires with field coefficients. Terms are automatically sorted by wire ID for efficient serialization.

## Testing

The library includes comprehensive tests covering:
- Basic builder operations
- Linear combination manipulation
- Constraint creation
- Binary serialization
- Compatibility with the R1CS specification example

Run tests with:
```bash
swift test --filter R1CSTests
```

## Dependencies

- **BigInt**: For arbitrary-precision arithmetic required for large field elements
