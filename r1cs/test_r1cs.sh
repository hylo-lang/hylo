#!/bin/bash
# Test script to verify R1CS output using external tools

set -e

echo "================================"
echo "R1CS Testing with snarkjs"
echo "================================"
echo

# Check if snarkjs is installed
if ! command -v snarkjs &> /dev/null; then
    echo "snarkjs not found. Installing..."
    npm install -g snarkjs
fi

# File to test
R1CS_FILE="${1:-/workspaces/hylo/r1cs/ExampleWithParams.ir.r1cs.bin}"

if [ ! -f "$R1CS_FILE" ]; then
    echo "Error: R1CS file not found: $R1CS_FILE"
    echo "Usage: $0 <path-to-r1cs-file>"
    exit 1
fi

echo "Testing R1CS file: $R1CS_FILE"
echo

# Print R1CS info
echo "=== R1CS Information ==="
snarkjs r1cs info "$R1CS_FILE" || {
    echo "Failed to read R1CS file. File may be corrupted or in wrong format."
    exit 1
}
echo

# Print constraints
echo "=== R1CS Constraints (first 10) ==="
snarkjs r1cs print "$R1CS_FILE" | head -50
echo

# Export to JSON for inspection
JSON_FILE="${R1CS_FILE%.bin}.json"
echo "=== Exporting to JSON: $JSON_FILE ==="
snarkjs r1cs export json "$R1CS_FILE" "$JSON_FILE"
echo "Exported successfully!"
echo

# Show the JSON structure
if command -v jq &> /dev/null; then
    echo "=== JSON Structure (summary) ==="
    jq '{
        prime: .prime,
        nVars: .nVars,
        nOutputs: .nOutputs,
        nPubInputs: .nPubInputs,
        nPrvInputs: .nPrvInputs,
        nLabels: .nLabels,
        nConstraints: .nConstraints,
        constraints_sample: .constraints[:2]
    }' "$JSON_FILE"
else
    echo "Install 'jq' for better JSON viewing: sudo apt-get install jq"
    head -20 "$JSON_FILE"
fi
echo

echo "=== Testing Complete ==="
echo "Summary:"
echo "  - R1CS file is valid and can be read by snarkjs"
echo "  - Full details exported to: $JSON_FILE"
echo
