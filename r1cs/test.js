const fs = require('fs');
const path = require('path');
const snarkjs = require('snarkjs');
const { generateWitness } = require('./Example.witnessgen.js');
const { verifyWitness } = require('./verify.js');

const r1csFile = path.join(__dirname, 'Example.r1cs');

// Generate random integer between min and max (inclusive)
function randomInt(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Generate an array of random inputs
function generateRandomInputs(count) {
    const inputs = [];
    for (let i = 0; i < count; i++) {
        // Generate random numbers including negative values (witness generator now accepts negative inputs)
        inputs.push(randomInt(-100, 100).toString());
    }
    return inputs;
}

// Generate witness programmatically
async function generateWitnessForInputs(inputs) {
    try {
        const result = generateWitness(inputs);
        return { success: true, witness: result.witness };
    } catch (error) {
        return { success: false, error: error.message };
    }
}

// Verify witness programmatically
async function verifyWitnessArray(witnessArray) {
    try {
        const result = await verifyWitness(r1csFile, witnessArray, false);
        return { success: result.valid, result };
    } catch (error) {
        return { success: false, error: error.message };
    }
}

async function main() {
    try {
        // Get R1CS info using snarkjs
        console.log('Loading R1CS file to get number of public inputs...');
        const r1csData = await snarkjs.r1cs.info(r1csFile);
        const numberOfInputs = r1csData.nPubInputs;
        
        console.log(`R1CS has ${numberOfInputs} public inputs.`);
        console.log(`Total constraints: ${r1csData.nConstraints}`);
        console.log(`Total wires: ${r1csData.nVars}`);
        console.log('');

        const numTests = 10; // Number of random test cases
        let successCount = 0;
        let failureCount = 0;

        console.log(`Running ${numTests} tests with random inputs...\n`);

        for (let i = 1; i <= numTests; i++) {
            process.stdout.write(`Test ${i}/${numTests}: `);
            
            // Generate random inputs
            const inputs = generateRandomInputs(numberOfInputs);
            process.stdout.write(`inputs=[${inputs.join(', ')}] `);
            
            // Generate witness
            const witnessResult = await generateWitnessForInputs(inputs);
            if (!witnessResult.success) {
                console.log(`❌ Witness generation failed: ${witnessResult.error}`);
                failureCount++;
                continue;
            }
            
            // Verify witness
            const verifyResult = await verifyWitnessArray(witnessResult.witness);
            if (verifyResult.success) {
                console.log('✅ Verified');
                successCount++;
            } else {
                console.log(`❌ Verification failed: ${verifyResult.error || 'Constraints not satisfied'}`);
                failureCount++;
            }
        }

        console.log('\n=== Test Results ===');
        console.log(`Total tests: ${numTests}`);
        console.log(`Successful: ${successCount}`);
        console.log(`Failed: ${failureCount}`);
        console.log(`Success rate: ${((successCount / numTests) * 100).toFixed(1)}%`);

        process.exit(failureCount);

    } catch (error) {
        console.error('Error:', error.message);
        process.exit(1);
    }
}

main()