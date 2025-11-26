import BigInt
import R1CS

struct JavaScriptWitnessGeneratorGen : WitnessGeneratorGen {
    var buffer: String = ""
    
    init(prime: BigUInt, parameterCount: Int) {
        // 1. Header: Setup BigInts and array
        self.buffer = """
        const fs = require('fs');
        
        // The Scalar Field Prime (BN254)
        const P = \(prime)n;
        
        // Helper for modular arithmetic
        const add = (a, b) => (a + b) % P;
        const sub = (a, b) => { let r = (a - b) % P; return r < 0n ? r + P : r; };
        const mul = (a, b) => (a * b) % P;

        // The Witness Array (1-indexed mapping to R1CS wires)
        // Index 0 is always the constant 1
        const w = [];
        w[0] = 1n;
        
        // Parse and validate inputs from CLI
        // Usage: node witness.js '["3", "4"]'
        let args = [];
        try {
            if (!process.argv[2]) {
                throw new Error(`Missing input argument. Usage: node witness.js '["value1", "value2", ...]'`);
            }
            
            const rawInput = process.argv[2];
            const parsed = JSON.parse(rawInput);
            
            if (!Array.isArray(parsed)) {
                throw new Error('Input must be a JSON array of numeric values');
            }
            
            if (parsed.length !== \(parameterCount)) {
                throw new Error(`Expected \(parameterCount) input(s), but received ${parsed.length}`);
            }
            
            args = parsed.map((val, idx) => {
                try {
                    // Convert to BigInt, handling both string and number inputs
                    const bigIntVal = BigInt(val);
                    
                    // Validate that the value is in the valid field range [0, P)
                    if (bigIntVal < 0n) {
                        throw new Error(`Input at index ${idx} must be non-negative, got: ${val}`);
                    }
                    if (bigIntVal >= P) {
                        throw new Error(`Input at index ${idx} exceeds field prime. Value: ${val}, Prime: ${P}`);
                    }
                    
                    return bigIntVal;
                } catch (e) {
                    if (e.message.includes('Cannot convert')) {
                        throw new Error(`Invalid numeric value at index ${idx}: "${val}"`);
                    }
                    throw e;
                }
            });
        } catch (error) {
            console.error('Error parsing inputs:', error.message);
            process.exit(1);
        }
        
        // BEGINNING OF PROGRAM
        
        """
    }
    
    /// Reads from the CLI args array into the correct wire index
    mutating func recordInput(wire: WireID, argIndex: Int) {
        buffer += "w[\(wire.raw)] = args[\(argIndex)]; // Public Input #\(argIndex)\n"
    }
    
    mutating func recordConstant(wire: WireID, value: BigUInt) {
        buffer += "w[\(wire.raw)] = \(value)n;\n"
    }
    
    mutating func recordAssignment(destination: WireID, source: WireID) {
        buffer += "w[\(destination.raw)] = w[\(source.raw)];\n"
    }
    
    mutating func recordAdd(destination: WireID, a: WireID, b: WireID) {
        buffer += "w[\(destination.raw)] = add(w[\(a.raw)], w[\(b.raw)]);\n"
    }
    
    mutating func recordSub(destination: WireID, a: WireID, b: WireID) {
        buffer += "w[\(destination.raw)] = sub(w[\(a.raw)], w[\(b.raw)]);\n"
    }
    
    mutating func recordMul(destination: WireID, a: WireID, b: WireID) {
        buffer += "w[\(destination.raw)] = mul(w[\(a.raw)], w[\(b.raw)]);\n"
    }
    
    func generateCode() -> String {
        return buffer + """
        // END OF PROGRAM

        // Validate witness array integrity
        for (let i = 0; i < w.length; i++) {
            if (w[i] === undefined || w[i] === null) {
                console.error(`Error: Wire ${i} was not assigned a value`);
                process.exit(1);
            }
            if (typeof w[i] !== 'bigint') {
                console.error(`Error: Wire ${i} has invalid type: ${typeof w[i]}`);
                process.exit(1);
            }
        }
        
        // Output result as JSON string of strings (for Arkworks/SnarkJS)
        const output = w.map(val => val.toString());
        console.log(JSON.stringify(output));
        """
    }
}