import BigInt
import Foundation
import FrontEnd
import IR
import R1CS
import Utils

let red = "\u{001B}[0;31m"
let reset = "\u{001B}[0;0m"

typealias OperandID = InstructionID

// Represents what a register holds in the compiler's mind.
enum AbstractValue {
    case wire(WireID)
    case pointer(base: Operand, offset: Int)
}

/// Generates R1CS output for the given program and module.
///
/// - Parameters:
///   - ir: The IR program to process (will be modified by inlining).
///   - sourceModule: The module to generate R1CS for.
///   - program: The typed program for AST access.
///   - outputURL: The URL to write the output to.
///   - verbose: Whether to print verbose logging.
/// - Returns: The generated R1CS output as a string.
/// - Throws: If no entry function is found or output writing fails.
public func generateR1CS(
    ir: inout IR.Program,
    sourceModule: ModuleDecl.ID,
    program: TypedProgram,
    outputURL: URL?,
    productName: String,
    verbose: Bool = false
) throws -> String {
    if verbose {
        print("begin R1CS generation pass.")
    }

    // Find the problem declaration (currently unused, kept for future use)
    struct FindProblemDecl: ASTWalkObserver {
        var problemDecl: FunctionDecl.ID? = nil
        mutating func willEnter(_ n: AnyNodeID, in ast: AST) -> Bool {
            if n.kind == FunctionDecl.self,
                let d = FunctionDecl.ID(n),
                ast[d].identifier?.value == "problem"
            {
                problemDecl = d
                return false
            }
            return true
        }
    }

    // // Get the entry function
    // guard let entryFunction = ir.modules[sourceModule]?.entryFunction else {
    //     throw R1CSGenerationError.noEntryFunction
    // }

    // Inline calls to simplify the IR
    ir.inlineCalls(in: sourceModule, where: .hasNoControlFlow)

    guard
        let entryFunction = ir.modules[sourceModule]?.functions.first(where: { f in
            f.value.site.text.contains("public fun problem")
        })
    else {
        throw R1CSGenerationError.noEntryFunction
    }

    if verbose {
        print(
            "entry function found at \(ir.modules[sourceModule]!.functions[entryFunction.key]!.site)"
        )
    }

    // Generate the output
    let irSourceModule = ir.modules[sourceModule]!
    let output = irSourceModule.coloredDescribeBlocksWithCalleeIdentifiers(in: entryFunction.key)

    // Write to file
    let outputFile = outputURL ?? URL(fileURLWithPath: "\(productName)")
    try output.write(to: outputFile.appendingPathExtension("ir.ansi"), atomically: true, encoding: .utf8)

    // BN254 (alt_bn128) curve prime - standard for zkSNARKs
    // This is the scalar field order of the BN254 curve used by Ethereum, snarkjs, circom, etc.
    // 21888242871839275222246405745257275088548364400416034343698204186575808495617
    let bn254Prime: BigUInt =
        "21888242871839275222246405745257275088548364400416034343698204186575808495617"
    var r1cs = R1CS(prime: bn254Prime)

    let entryBlockId = irSourceModule.blocks(in: entryFunction.key).first!
    let entryBlock = irSourceModule[entryBlockId]

    guard entryFunction.value.blocks.count == 1 else {
        throw R1CSGenerationError.moreThanOneBlockInEntryFunction
    }

    var witnessGen: WitnessGeneratorGen = JavaScriptWitnessGeneratorGen(
        prime: r1cs.prime,
        parameterCount: entryBlock.inputs.count - 1)  // Last param is return pointer

    /// Maps SSA Register ID -> Value (Wire) OR Address (Pointer)
    var operandValues: [Operand: AbstractValue] = [:]

    /// Maps a Base Allocation ID (e.g. %i0.0) -> (Offset -> WireID)
    typealias PhysicalMemory = [Operand: [Int: WireID]]
    var memory: PhysicalMemory = [:]

    // Track public input wires (function parameters)
    var publicInputWires: [WireID] = []

    // In Hylo IR, function parameters come as pointers. The last parameter is always
    // the return value pointer. All others are input parameters.
    // For ZK proofs, we need to:
    // 1. Create wires for the input values
    // 2. Set up memory so those pointers point to the input wires
    // 3. Mark them as public inputs

    // Process all parameters except the last one (return pointer)
    for index in entryBlock.inputs.indices.dropLast() {
        let paramOperand = Operand.parameter(entryBlockId, index)

        // Create a wire for this input parameter's value
        let paramWire = r1cs.addWire()
        publicInputWires.append(paramWire)

        // The parameter itself is a pointer
        operandValues[paramOperand] = .pointer(base: paramOperand, offset: 0)
        witnessGen.recordInput(wire: paramWire, argIndex: index)

        // Initialize memory at this pointer location with the input wire
        memory[paramOperand] = [0: paramWire]

        print("Parameter \(index): wire \(paramWire) marked as public input")
    }

    // Update the public input count in R1CS
    r1cs.publicInputCount = UInt32(publicInputWires.count)

    // Last parameter is the pointer to the return value
    let returnValueParam = Operand.parameter(entryBlockId, entryBlock.inputs.count - 1)
    let returnValueWire = r1cs.addWire()
    memory[returnValueParam] = [:]
    memory[returnValueParam]![0] = returnValueWire
    operandValues[returnValueParam] = .pointer(base: returnValueParam, offset: 0)

    // Extract the return value wire (public output in ZK sense)
    // The return value is stored in memory at the return pointer location
    if let returnWire = memory[returnValueParam]?[0] {
        r1cs.publicOutputCount = 1
        print("Return value wire: \(returnWire) marked as public output")
    } else {
        fatalError("Warning: No return value found in memory")
    }

    // Helper to extract a wire from a register or parameter
    func getWire(for operand: Operand) -> WireID {
        guard let val = operandValues[operand], case .wire(let w) = val else {
            fatalError(
                "Expected register \(operand) to hold a Wire, but found \(String(describing: operandValues[operand]))"
            )
        }
        return w
    }

    // Helper to extract a pointer from a register
    func getPointer(for operand: Operand) -> (base: Operand, offset: Int) {
        guard let val = operandValues[operand], case .pointer(let b, let o) = val else {
            fatalError("Expected register \(operand) to hold a Pointer")
        }
        return (b, o)
    }

    for instructionId in irSourceModule.instructions(
        in: irSourceModule.blocks(in: entryFunction.key).first!)
    {
        let instruction = irSourceModule[instructionId]

        switch instruction {
        case _ as IR.AllocStack:
            operandValues[.register(instructionId)] = .pointer(
                base: .register(instructionId), offset: 0)
            memory[.register(instructionId)] = nil  // Allocate as uninitialized

        case let subfieldView as IR.SubfieldView:
            // Read previous pointer, add offset, store new pointer.

            guard let firstOperand = subfieldView.operands.first else {
                fatalError("SubfieldView missing operand")
            }

            // Get the base pointer from the source register
            let (base, currentOffset) = getPointer(for: firstOperand)

            // Determine the added offset.
            // Note: Real implementation needs to look at type layout.
            // For now, assuming an array of Words (Ints) so index adds 1.
            // If subfield indices are bytes, you might need type info here.
            // let addedOffset = subfieldView.subfield  // TODO: Parse indices to get real offset

            operandValues[.register(instructionId)] = .pointer(
                base: base, offset: currentOffset + 0)
        case let store as IR.Store:
            let valueWire: WireID

            switch store.object {
            case .constant(let c):
                guard let value = c as? IntegerConstant else {
                    fatalError("Unsupported constant type for store: \(c.type)")
                }

                // todo avoid adding extra wire here
                valueWire = r1cs.addWire()
                let fieldValue = r1cs.numberToField(BigInt(value.value))
                r1cs.addConstraint(
                    .constant(wire: valueWire, value: fieldValue))
                witnessGen.recordConstant(wire: valueWire, value: fieldValue)

            case .register(let instructionID):
                valueWire = getWire(for: .register(instructionID))
            case .parameter(let blockId, let index):
                valueWire = getWire(for: .parameter(blockId, index))
            }
            // Resolve the Address
            let (targetBase, offset) = getPointer(for: store.target)

            // Todo avoid extra wire here
            if targetBase == returnValueParam {
                witnessGen.recordAssignment(destination: returnValueWire, source: valueWire)
                r1cs.addConstraint(
                    .init(
                        a: LinearCombination(terms: [(wire: returnValueWire, coefficient: 1)]),
                        b: LinearCombination(terms: [(wire: .unit, coefficient: 1)]),
                        c: LinearCombination(terms: [(wire: valueWire, coefficient: 1)])
                    ))
            }

            // Update Physical Memory
            if memory[targetBase] == nil {
                memory[targetBase] = [:]  // Initialize new base if needed (why is this needed)
            }
            memory[targetBase]![offset] = valueWire
        case let access as IR.Access:
            // Resolve pointer
            let (base, offset) = getPointer(for: access.source)

            // The result points to the same location, this is just an alias
            operandValues[.register(instructionId)] = .pointer(base: base, offset: offset)

        case let load as IR.Load:
            // guard case let .register(sourceReg) = load.source else { fatalError("Load source invalid") }

            // Resolve pointer
            let (base, offset) = getPointer(for: load.source)

            // Retrieve wire from memory
            guard let loadedWire = memory[base]?[offset] else {
                fatalError("Reading from uninitialized memory at \(base) + \(offset)")
            }

            // The result of a Load instruction is a Value (Wire)
            operandValues[.register(instructionId)] = .wire(loadedWire)

        case let memcpy as IR.MemoryCopy:
            let sourcePointer = getPointer(for: memcpy.source)
            let targetPointer = getPointer(for: memcpy.target)

            let sourcePointee = memory[sourcePointer.base]![sourcePointer.offset]!

            // Update memory at target pointer to point to source wire
            if memory[targetPointer.base] == nil {
                memory[targetPointer.base] = [:]
            }
            memory[targetPointer.base]![targetPointer.offset] = sourcePointee


            // Todo avoid extra wire here
            if targetPointer.base == returnValueParam {
                witnessGen.recordAssignment(destination: returnValueWire, source: sourcePointee)
                r1cs.addConstraint(
                    .init(
                        a: LinearCombination(terms: [(wire: returnValueWire, coefficient: 1)]),
                        b: LinearCombination(terms: [(wire: .unit, coefficient: 1)]),
                        c: LinearCombination(terms: [(wire: sourcePointee, coefficient: 1)])
                    ))
            }

        case let callBuiltin as IR.CallBuiltinFunction:
            /// Result wire
            let x = r1cs.addWire()
            operandValues[.register(instructionId)] = .wire(x)

            switch callBuiltin.callee {
            case .add(_, _), .mul(_, _), .sub(_, _):
                guard callBuiltin.operands.count == 2 else {
                    fatalError("Add builtin must have exactly 2 operands")
                }

                let leftOperand = callBuiltin.operands[0]
                let rightOperand = callBuiltin.operands[1]

                let a = getWire(for: leftOperand)
                let b = getWire(for: rightOperand)

                switch callBuiltin.callee {
                case .add(_, _):
                    // HYLO IR: x := a + b
                    // x - a - b = 0
                    // (x) * (1) - (a + b)
                    // A: x
                    // B: 1
                    // C: a, b
                    r1cs.addConstraint(
                        .init(
                            a: LinearCombination(terms: [(wire: x, coefficient: 1)]),
                            b: LinearCombination(terms: [(wire: R1CS.unitWire, coefficient: 1)]),
                            c: LinearCombination(terms: [
                                (wire: a, coefficient: 1),
                                (wire: b, coefficient: 1),
                            ])))
                    witnessGen.recordAdd(destination: x, a: a, b: b)
                case .sub(_, _):
                    // HYLO IR: x := a - b
                    // x - a + b = 0
                    // (x) * (1) + (-1a + 1b) = 0
                    // (x) * (1) - (1a + -1b) = 0
                    // A: x
                    // B: 1
                    // C: a + -1b
                    r1cs.addConstraint(
                        .init(
                            a: LinearCombination(terms: [(wire: x, coefficient: 1)]),
                            b: LinearCombination(terms: [(wire: R1CS.unitWire, coefficient: 1)]),
                            c: LinearCombination(terms: [
                                (wire: a, coefficient: 1),
                                (wire: b, coefficient: r1cs.prime - 1),
                            ])))
                    witnessGen.recordSub(destination: x, a: a, b: b)
                case .mul(_, _):
                    // HYLO IR: x := a * b
                    // (a) * (b) - (x) = 0
                    // A: a
                    // B: b
                    // C: x
                    r1cs.addConstraint(
                        .init(
                            a: LinearCombination(terms: [(wire: a, coefficient: 1)]),
                            b: LinearCombination(terms: [(wire: b, coefficient: 1)]),
                            c: LinearCombination(terms: [(wire: x, coefficient: 1)])))
                    witnessGen.recordMul(destination: x, a: a, b: b)

                // case .div(, _):
                //     // HYLO IR: x := a / b
                //     // (x) * (b) - (a) = 0
                //     // A: x
                //     // B: b
                //     // C: a
                //     r1cs.addConstraint(
                //         .init(
                //             a: LinearCombination(terms: [(wire: x, coefficient: 1)]),
                //             b: LinearCombination(terms: [(wire: b, coefficient: 1)]),
                //             c: LinearCombination(terms: [(wire: a, coefficient: 1)])))
                // }

                default:
                    print("\(red)Unsupported builtin function: \(callBuiltin.callee)\(reset)")
                }
            default:
                print("\(red)Unsupported builtin function: \(callBuiltin.callee)\(reset)")
            }
        case _ as IR.EndAccess, _ as IR.DeallocStack, _ as IR.MarkState, _ as IR.Return:
            // No action needed for R1CS generation
            break
        default:
            print("   - ignored instruction \(instruction)")
        }
    }

    print("Memory: \(memory)")

    try String(describing: r1cs).write(
        to: outputURL!.appendingPathExtension("r1cs.ansi"), atomically: true, encoding: .utf8)

    try r1cs.serialize(to: outputURL!.appendingPathExtension("r1cs"))

    try witnessGen.generateCode().write(
        to: outputURL!.appendingPathExtension("witnessgen.js"),
        atomically: true,
        encoding: .utf8)

    return output
}

/// Errors that can occur during R1CS generation.
public enum R1CSGenerationError: Error {
    case noEntryFunction
    case moreThanOneBlockInEntryFunction
}
