const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');
const snarkjs = require('snarkjs');

const r1csFile = path.join(__dirname, 'Example.r1cs');
const witnessGenScript = path.join(__dirname, 'Example.witnessgen.js');
const verifyScript = path.join(__dirname, 'verify.js');
const witnessFile = path.join(__dirname, 'Example.witness.json');

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

// Call witness generator with given inputs
async function callWitnessGen(inputs) {
    try {
        const inputArg = JSON.stringify(inputs);
        const command = `node ${witnessGenScript} '${inputArg}' 1> ${witnessFile}`;
        
        const output = execSync(command, { encoding: 'utf-8', stdio: ['pipe', 'pipe', 'pipe'] });
        return true;
    } catch (error) {
        console.error(`Witness generation failed: ${error.message}`);
        return false;
    }
}

// Run verification script
async function runVerification() {
    try {
        const command = `node ${verifyScript} ${r1csFile} ${witnessFile}`;
        const output = execSync(command, { encoding: 'utf-8', stdio: ['pipe', 'pipe', 'pipe'] });
        return true;
    } catch (error) {
        // verify.js exits with code 1 on failure, which throws an error in execSync
        return false;
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

        const numTests = 20; // Number of random test cases
        let successCount = 0;
        let failureCount = 0;

        console.log(`Running ${numTests} tests with random inputs...\n`);

        for (let i = 1; i <= numTests; i++) {
            process.stdout.write(`Test ${i}/${numTests}: `);
            
            // Generate random inputs
            const inputs = generateRandomInputs(numberOfInputs);
            process.stdout.write(`inputs=[${inputs.join(', ')}] `);
            
            // Generate witness
            const witnessSuccess = await callWitnessGen(inputs);
            if (!witnessSuccess) {
                console.log('❌ Witness generation failed');
                failureCount++;
                continue;
            }
            
            // Verify witness
            const verifySuccess = await runVerification();
            if (verifySuccess) {
                console.log('✅ Verified');
                successCount++;
            } else {
                console.log('❌ Verification failed');
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