import BigInt
import R1CS

fileprivate extension Value {
    var js: String {
        switch self {
        case .compileTime(let const):
            return "\(const)n"
        case .runtime(let wire):
            return "w[\(wire.raw)]"
        }
    }
}
struct JavaScriptWitnessGeneratorGen : WitnessGeneratorGen {
    var buffer: String = ""
    
    init(prime: BigUInt, parameterCount: Int) {
        // 1. Header: Setup BigInts and arithmetic helpers
        self.buffer = """
        const fs = require('fs');
        
        // The Scalar Field Prime (BN254)
        const P = \(prime)n;
        
        // Helper for modular arithmetic
        const add = (a, b) => (a + b) % P;
        const sub = (a, b) => { let r = (a - b) % P; return r < 0n ? r + P : r; };
        const mul = (a, b) => (a * b) % P;
        
        // Main witness generation function (can be called programmatically)
        function generateWitness(inputArray) {
            // Validate inputs
            if (!Array.isArray(inputArray)) {
                throw new Error('Input must be an array of numeric values');
            }
            
            if (inputArray.length !== \(parameterCount)) {
                throw new Error(`Expected \(parameterCount) input(s), but received ${inputArray.length}`);
            }
        
            // Parse and normalize inputs
            const args = inputArray.map((val, idx) => {
                try {
                    // Convert to BigInt, handling both string and number inputs
                    let bigIntVal = BigInt(val);
                    
                    // Normalize negative values to be in the valid field range [0, P)
                    if (bigIntVal < 0n) {
                        // For negative values, normalize by adding multiples of P until positive
                        bigIntVal = bigIntVal % P;
                        if (bigIntVal < 0n) {
                            bigIntVal += P;
                        }
                    } else if (bigIntVal >= P) {
                        // For values >= P, use modular reduction
                        bigIntVal = bigIntVal % P;
                    }
                    
                    return bigIntVal;
                } catch (e) {
                    throw new Error(`Input at index ${idx} is not a valid number: ${val}`);
                }
            });
        
            // The Witness Array (1-indexed mapping to R1CS wires)
            // Index 0 is always the constant 1
            const w = [];
            w[0] = 1n;
        
            // BEGINNING OF PROGRAM - Full circuit implementation
        
        """
    }
    
    /// Reads from the args array into the correct wire index
    mutating func recordInput(wire: WireID, argIndex: Int) {
        buffer += "        w[\(wire.raw)] = args[\(argIndex)]; // Public Input #\(argIndex)\n"
    }
    
    mutating func recordConstant(wire: WireID, value: BigUInt) {
        buffer += "        w[\(wire.raw)] = \(value)n;\n"
    }
    
    mutating func recordAdd(destination: WireID, a: Value, b: Value) {
        buffer += "        w[\(destination.raw)] = add(\(a.js), \(b.js));\n"
    }
    
    mutating func recordSub(destination: WireID, a: Value, b: Value) {
        buffer += "        w[\(destination.raw)] = sub(\(a.js), \(b.js));\n"
    }
    
    mutating func recordMul(destination: WireID, a: Value, b: Value) {
        buffer += "        w[\(destination.raw)] = mul(\(a.js), \(b.js));\n"
    }
    
    func generateCode(outputWire: WireID) -> String {
        return buffer + """
            // END OF PROGRAM
            
            const outputWire = \(outputWire.raw);
            
            // Validate witness array integrity
            for (let i = 0; i < w.length; i++) {
                if (w[i] === undefined || w[i] === null) {
                    throw new Error(`Wire ${i} was not assigned a value`);
                }
                if (typeof w[i] !== 'bigint') {
                    throw new Error(`Wire ${i} has invalid type: ${typeof w[i]}`);
                }
            }
            
            const returnValue = w[outputWire];
            
            return {
                witness: w.map(val => val.toString()),
                returnValue: returnValue.toString(),
                outputWire
            };
        }
        
        // CLI handling - only run if this file is executed directly
        if (require.main === module) {
            try {
                if (!process.argv[2]) {
                    throw new Error(`Missing input argument. Usage: node witness.js '["value1", "value2", ...]'`);
                }
                
                const rawInput = process.argv[2];
                const parsed = JSON.parse(rawInput);
                
                const result = generateWitness(parsed);
                
                if (process.argv[3] === 'nice') {
                    let niceWitness = "{";
                    for (let i = 0; i < result.witness.length; i++) {
                        if (i === result.outputWire) {
                            niceWitness += `\\u001b[33mw${i}\\u001b[0m: \\u001b[35m${result.witness[i]}\\u001b[0m`;
                        } else {
                            niceWitness += `\\u001b[32mw${i}\\u001b[0m: \\u001b[35m${result.witness[i]}\\u001b[0m`;
                        }
                        if (i < result.witness.length - 1) {
                            niceWitness += ", ";
                        }
                    }
                    niceWitness += "}";
                    console.log("Witness: " + niceWitness);
                    console.log(`Return Value (Wire ${result.outputWire}): ${result.returnValue}`);
                } else {
                    // Output result as JSON string of strings (for Arkworks/SnarkJS)
                    console.log(JSON.stringify(result.witness));
                }
            } catch (error) {
                console.error('Error parsing inputs:', error.message);
                process.exit(1);
            }
        }
        
        // Export for programmatic use
        module.exports = { generateWitness, P, add, sub, mul };
        """
    }
}