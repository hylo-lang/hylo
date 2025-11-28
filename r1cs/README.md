# R1CS Testing Guide

```bash
swift run hc --emit r1cs --freestanding -o /workspaces/hylo/r1cs/Example /workspaces/hylo/r1cs/Example.hylo
```

```bash
node ./Example.witnessgen.js [3,4]  1> Example.witness.json
```

```bash
node ./verify
```



This directory contains R1CS (Rank-1 Constraint System) files generated from Hylo programs for zero-knowledge proof circuits.

## Files

- `Example.hylo` - Simple example with no parameters (constants only)
- `Example.hylo` - Parameterized function for ZK proofs
- `*.ir` - Intermediate representation (IR) files
- `*.r1cs` - Binary R1CS files (iden3 format)
- `*.r1cs.ansi` - Human-readable ANSI-colored R1CS output

## Generating R1CS

```bash
# From the repository root
swift run hc --emit r1cs --freestanding -o r1cs/Example.ir r1cs/Example.hylo
```

## Testing with External Tools

### Option 1: Using snarkjs (Recommended)

[snarkjs](https://github.com/iden3/snarkjs) is the most popular tool for working with R1CS files.

#### Installation
```bash
npm install -g snarkjs
```

#### Basic Usage

**1. View R1CS Information:**
```bash
snarkjs r1cs info ./Example.r1cs
```

**2. Print Constraints:**
```bash
snarkjs r1cs print r1cs/Example.ir.r1cs
```

**3. Export to JSON:**
```bash
snarkjs r1cs export json r1cs/Example.ir.r1cs r1cs/circuit.json
```

**4. Generate witness and proof:**
```bash
# Create input file (input.json)
cat > input.json << EOF
{
    "a": "3",
    "b": "4",
    "c": "5"
}
EOF

# Calculate witness
snarkjs wtns calculate r1cs/Example.ir.r1cs input.json witness.wtns

# Setup (using Groth16)
snarkjs groth16 setup r1cs/Example.ir.r1cs pot12_final.ptau circuit_0000.zkey

# Generate proof
snarkjs groth16 prove circuit_0000.zkey witness.wtns proof.json public.json

# Verify proof
snarkjs groth16 verify verification_key.json public.json proof.json
```

#### Using the Test Script
```bash
chmod +x r1cs/test_r1cs.sh
./r1cs/test_r1cs.sh r1cs/Example.ir.r1cs
```

### Option 2: Using circom ecosystem

```bash
# Install circom
git clone https://github.com/iden3/circom.git
cd circom
cargo build --release
cargo install --path circom
```

### Option 3: Python tools

```bash
# Install py_r1cs
pip install py-r1cs

# Use in Python
python3 << EOF
from r1cs_reader import R1CS
r1cs = R1CS.from_file('r1cs/Example.ir.r1cs')
print(f"Constraints: {r1cs.num_constraints}")
print(f"Variables: {r1cs.num_variables}")
print(f"Public inputs: {r1cs.num_inputs}")
EOF
```

### Option 4: Binary inspection

```bash
# View hex dump
xxd r1cs/Example.ir.r1cs | head -20

# Check magic bytes (should be "r1cs")
head -c 4 r1cs/Example.ir.r1cs | xxd -p
# Expected: 72316373

# Check version (should be 1)
xxd -s 4 -l 4 r1cs/Example.ir.r1cs
```

## R1CS File Format

The binary files follow the [iden3 R1CS format specification](https://github.com/iden3/r1csfile/blob/master/doc/r1cs_bin_format.md):

- Magic: "r1cs" (0x72316373)
- Version: 1
- Sections: Header, Constraints, Wire2Label map

### Understanding the Output

For `Example.hylo`, which computes `(a*a + b*b) - c*c`:

- **Public Inputs (3)**: a, b, c
- **Public Outputs (1)**: result
- **Private Inputs (0)**: none
- **Constraints (5)**: 
  1. w2 × w2 = w4 (a²)
  2. w1 × w1 = w5 (b²)
  3. w6 × 1 = w5 + w4 (a² + b²)
  4. w3 × w3 = w7 (c²)
  5. w8 × 1 = w6 - w7 (result)

## Expected Results

### Example (Right Triangle Check)

With inputs a=3, b=4, c=5:
- a² = 9
- b² = 16
- c² = 25
- (a² + b²) - c² = 25 - 25 = 0

The circuit proves that (a,b,c) forms a right triangle.

### Example (No Parameters)

Hardcoded computation: `2 + is_right_triangle(3, 4, 5)`
- Expected output: 2 + 0 = 2

## Troubleshooting

### "Invalid R1CS file"
- Check the magic bytes are correct
- Verify file wasn't corrupted during transfer
- Ensure you're using the `.r1cs` file, not the `.ansi` text file

### "snarkjs: command not found"
```bash
npm install -g snarkjs
```

### Field too small errors
The current implementation uses a 37-bit prime (87178291199). For production use, switch to a standard ZK-friendly prime like BN254 or BLS12-381.

## Witness Validation

The Hylo compiler includes built-in witness validation. To validate a witness:

### Using Swift API

```swift
import R1CS
import BigInt

// Generate witness
let witness = try generateWitness(inputs: ["3", "4"])

// Validate
let validator = WitnessValidator(r1cs: r1cs, witness: witness)

// Check structure
if let error = validator.validateWitnessStructure() {
    print("❌ Invalid witness: \(error)")
    exit(1)
}

// Validate constraints
let result = validator.validate()
if result.isValid {
    print("✓ All \(result.constraintResults.count) constraints satisfied")
} else {
    print("❌ Validation failed:")
    print(result.debugDescription)
}
```

### Demo Script

Run the witness validation demo:

```bash
./r1cs/validate_witness_demo.sh
```

This demonstrates:
1. Generating a witness from inputs
2. Analyzing witness structure
3. Manually verifying constraints
4. Testing with invalid inputs

### What Gets Validated

1. **Witness Structure**
   - Correct length (matches wire count)
   - Wire 0 = 1 (constant wire)
   - All values within field bounds

2. **Constraint Satisfaction**
   - For each constraint `A × B = C`:
     - Evaluates A, B, C using witness values
     - Verifies: `(A × B) mod prime == C mod prime`

### Example Output

```
╔═══════════════════════════════════════════════════════════════╗
║  Witness Validation Results                                   ║
╚═══════════════════════════════════════════════════════════════╝

✓ All 7 constraints satisfied

Constraint Details:
✓ [0] 1 × 5 = 5
✓ [1] 4 × 4 = 16
✓ [2] 3 × 3 = 9
✓ [3] 25 × 1 = 25
✓ [4] 5 × 5 = 25
✓ [5] 0 × 1 = 0
✓ [6] 0 × 1 = 0
```

### Documentation

See [../Docs/WitnessValidation.md](../Docs/WitnessValidation.md) for complete documentation:
- API reference
- Use cases and examples
- Error handling
- Performance considerations
- Testing guide

### Tests

Run witness validator tests:

```bash
swift test --filter WitnessValidatorTests
```

Test files:
- `Tests/R1CSTests/WitnessValidatorTests.swift` - Unit tests
- `Tests/R1CSTests/WitnessValidatorIntegrationTests.swift` - Integration tests

## Further Reading

- [Witness Validation Documentation](../Docs/WitnessValidation.md) - Complete validation guide
- [snarkjs documentation](https://github.com/iden3/snarkjs)
- [R1CS file format spec](https://github.com/iden3/r1csfile/blob/master/doc/r1cs_bin_format.md)
- [ZK-SNARKs explained](https://z.cash/technology/zksnarks/)
- [circom documentation](https://docs.circom.io/)
