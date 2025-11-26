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

async function loadR1CS(filename) {
    console.log('Loading R1CS file using snarkjs...');
    const r1cs = await snarkjs.r1cs.info(filename, console);
    const r1csData = await snarkjs.r1cs.exportJson(filename);
    
    console.log('\nHeader:');
    console.log(`  Prime: ${r1csData.prime}`);
    console.log(`  Total wires: ${r1csData.nVars}`);
    console.log(`  Public outputs: ${r1csData.nOutputs}`);
    console.log(`  Public inputs: ${r1csData.nPubInputs}`);
    console.log(`  Private inputs: ${r1csData.nPrvInputs}`);
    console.log(`  Number of constraints: ${r1csData.nConstraints}`);
    
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

function verifyConstraints(r1csData, witness) {
    const prime = BigInt(r1csData.prime);
    const constraints = r1csData.constraints;

    console.log(`\n${'═'.repeat(70)}`);
    console.log('Verifying Constraints');
    console.log(`${'═'.repeat(70)}\n`);

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

        const status = isValid ? '✓' : '✗';
        const color = isValid ? '\x1b[32m' : '\x1b[31m'; // Green or red
        const reset = '\x1b[0m';

        console.log(`${color}${status}${reset} Constraint ${i}: ${aVal} × ${bVal} = ${cVal} ${isValid ? '' : `(got ${product})`}`);

        if (!isValid) {
            allValid = false;
        }
    }

    console.log(`\n${'═'.repeat(70)}`);
    if (allValid) {
        console.log('\x1b[32m✓ All constraints satisfied!\x1b[0m');
    } else {
        console.log('\x1b[31m✗ Some constraints failed!\x1b[0m');
    }
    console.log(`${'═'.repeat(70)}\n`);

    return { allValid, results };
}

async function main() {
    // Parse command line arguments
    const args = process.argv.slice(2);
    const r1csFile = args[0] || path.join(__dirname, 'Example.r1cs');
    const witnessFile = args[1] || path.join(__dirname, 'Example.witness.json');

    console.log('R1CS Constraint Verification');
    console.log(`${'═'.repeat(70)}`);
    console.log(`R1CS file: ${r1csFile}`);
    console.log(`Witness file: ${witnessFile}`);
    console.log(`${'═'.repeat(70)}\n`);

    // Check files exist
    if (!fs.existsSync(r1csFile)) {
        console.error(`Error: R1CS file not found: ${r1csFile}`);
        process.exit(1);
    }
    if (!fs.existsSync(witnessFile)) {
        console.error(`Error: Witness file not found: ${witnessFile}`);
        process.exit(1);
    }

    // Load R1CS
    const r1csData = await loadR1CS(r1csFile);

    // Load witness
    console.log('\nLoading witness file...');
    const witnessData = fs.readFileSync(witnessFile, 'utf8');
    const witnessArray = JSON.parse(witnessData);
    
    // Convert witness to BigInt array
    const witness = witnessArray.map(w => BigInt(w));
    
    console.log(`Witness length: ${witness.length}`);
    console.log(`Witness values: [${witness.slice(0, Math.min(10, witness.length)).join(', ')}${witness.length > 10 ? ', ...' : ''}]`);

    // Verify witness length matches
    if (witness.length !== r1csData.nVars) {
        console.error(`\nError: Witness length (${witness.length}) doesn't match wire count (${r1csData.nVars})`);
        process.exit(1);
    }

    // Verify wire 0 is 1 (constant wire)
    if (witness[0] !== 1n) {
        console.error(`\nError: Wire 0 must be 1 (constant wire), got ${witness[0]}`);
        process.exit(1);
    }

    // Verify constraints
    const { allValid, results } = verifyConstraints(r1csData, witness);

    // Exit with appropriate code
    process.exit(allValid ? 0 : 1);
}

if (require.main === module) {
    main().catch(err => {
        console.error('Error:', err.message);
        process.exit(1);
    });
}

module.exports = { loadR1CS, evaluateLC, verifyConstraints };
