#!/usr/bin/env node

/**
 * R1CS Constraint Verification Script
 * 
 * This script:
 * 1. Loads an R1CS file in binary format (.r1cs)
 * 2. Loads a witness in JSON format
 * 3. Verifies that all R1CS constraints are satisfied
 * 
 * Usage: node verify_r1cs.js [r1cs_file] [witness_file]
 */

const fs = require('fs');
const path = require('path');
const snarkjs = require('snarkjs');

async function loadR1CS(filename, verbose = true) {
    if (verbose) console.log('Loading R1CS file using snarkjs...');
    const r1cs = await snarkjs.r1cs.info(filename, verbose ? console : null);
    const r1csData = await snarkjs.r1cs.exportJson(filename);
    
    if (verbose) {
        console.log('\nHeader:');
        console.log(`  Prime: ${r1csData.prime}`);
        console.log(`  Total wires: ${r1csData.nVars}`);
        console.log(`  Public outputs: ${r1csData.nOutputs}`);
        console.log(`  Public inputs: ${r1csData.nPubInputs}`);
        console.log(`  Private inputs: ${r1csData.nPrvInputs}`);
        console.log(`  Number of constraints: ${r1csData.nConstraints}`);
    }
    
    return r1csData;
}

function evaluateLC(lc, witness, prime) {
    let sum = 0n;
    for (const [wireId, coefficient] of Object.entries(lc)) {
        const wireValue = witness[parseInt(wireId)] || 0n;
        const coeff = BigInt(coefficient);
        sum = (sum + (coeff * wireValue)) % prime;
    }
    return sum;
}

function verifyConstraints(r1csData, witness, verbose = true) {
    const prime = BigInt(r1csData.prime);
    const constraints = r1csData.constraints;

    if (verbose) {
        console.log(`\n${'═'.repeat(70)}`);
        console.log('Verifying Constraints');
        console.log(`${'═'.repeat(70)}\n`);
    }

    let allValid = true;
    const results = [];

    for (let i = 0; i < constraints.length; i++) {
        const constraint = constraints[i];

        // Evaluate each linear combination (A, B, C)
        const aVal = evaluateLC(constraint[0], witness, prime);
        const bVal = evaluateLC(constraint[1], witness, prime);
        const cVal = evaluateLC(constraint[2], witness, prime);

        // Check: A × B = C (mod prime)
        const product = (aVal * bVal) % prime;
        const isValid = product === cVal;

        results.push({
            index: i,
            isValid,
            aVal,
            bVal,
            cVal,
            product
        });

        if (verbose) {
            const status = isValid ? '✓' : '✗';
            const color = isValid ? '\x1b[32m' : '\x1b[31m'; // Green or red
            const reset = '\x1b[0m';

            console.log(`${color}${status}${reset} Constraint ${i}: ${aVal} × ${bVal} = ${cVal} ${isValid ? '' : `(got ${product})`}`);
        }

        if (!isValid) {
            allValid = false;
        }
    }

    if (verbose) {
        console.log(`\n${'═'.repeat(70)}`);
        if (allValid) {
            console.log('\x1b[32m✓ All constraints satisfied!\x1b[0m');
        } else {
            console.log('\x1b[31m✗ Some constraints failed!\x1b[0m');
        }
        console.log(`${'═'.repeat(70)}\n`);
    }

    return { allValid, results };
}

// Programmatic verification function
async function verifyWitness(r1csFile, witnessArray, verbose = false) {
    // Load R1CS
    const r1csData = await loadR1CS(r1csFile, verbose);

    // Convert witness to BigInt array
    const witness = witnessArray.map(val => BigInt(val));

    if (verbose) {
        console.log('\nLoaded witness:');
        console.log(`Witness length: ${witness.length}`);
        console.log(`Witness values: [${witness.slice(0, Math.min(10, witness.length)).join(', ')}${witness.length > 10 ? ', ...' : ''}]`);
    }

    // Verify witness length matches
    if (witness.length !== r1csData.nVars) {
        throw new Error(`Witness length (${witness.length}) doesn't match wire count (${r1csData.nVars})`);
    }

    // Verify wire 0 is 1 (constant wire)
    if (witness[0] !== 1n) {
        throw new Error(`Wire 0 must be 1 (constant wire), got ${witness[0]}`);
    }

    // Verify constraints
    const { allValid, results } = verifyConstraints(r1csData, witness, verbose);
    
    return {
        valid: allValid,
        results,
        r1csData,
        witness
    };
}

async function main() {
    // Parse command line arguments
    const args = process.argv.slice(2);
    const r1csFile = args[0] || path.join(__dirname, 'Example.r1cs');
    
    // Parse inputs from command line instead of witness file
    if (args.length < 2) {
        console.error('Usage: node verify.js [r1cs_file] input1 input2 input3 ...');
        console.error('Example: node verify.js Example.r1cs -5 3 10');
        process.exit(1);
    }
    
    const inputs = args.slice(1);

    console.log('R1CS Constraint Verification');
    console.log(`${'═'.repeat(70)}`);
    console.log(`R1CS file: ${r1csFile}`);
    console.log(`Inputs: [${inputs.join(', ')}]`);
    console.log(`${'═'.repeat(70)}\n`);

    // Check R1CS file exists
    if (!fs.existsSync(r1csFile)) {
        console.error(`Error: R1CS file not found: ${r1csFile}`);
        process.exit(1);
    }

    try {
        // Generate witness using witnessgen
        const { generateWitness } = require('./Example.witnessgen.js');
        console.log('Generating witness from inputs...');
        const witnessResult = generateWitness(inputs);
        
        console.log(`Generated witness with ${witnessResult.witness.length} elements`);
        console.log(`Witness values: [${witnessResult.witness.slice(0, Math.min(10, witnessResult.witness.length)).join(', ')}${witnessResult.witness.length > 10 ? ', ...' : ''}]`);

        // Verify the generated witness
        const result = await verifyWitness(r1csFile, witnessResult.witness, true);
        process.exit(result.valid ? 0 : 1);
    } catch (error) {
        console.error(`\nError: ${error.message}`);
        process.exit(1);
    }
}

if (require.main === module) {
    main().catch(err => {
        console.error('Error:', err.message);
        process.exit(1);
    });
}

module.exports = { loadR1CS, evaluateLC, verifyConstraints, verifyWitness };
