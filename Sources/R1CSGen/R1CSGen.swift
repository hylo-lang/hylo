import BigInt
import Foundation
import FrontEnd
import IR
import R1CS
import Utils

let red = "\u{001B}[0;31m"
let reset = "\u{001B}[0;0m"
let green = "\u{001B}[0;32m"
let grey = "\u{001B}[0;90m"
let gray = grey
let magenta = "\u{001B}[0;35m"

typealias OperandID = InstructionID

enum Value {
    case runtime(WireID)
    case compileTime(BigUInt)
}

// Represents what a register holds in the compiler's mind.
enum AbstractValue {
    case value(Value)
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
) throws {
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

    // Depolymorphize first - this creates monomorphized versions of generic functions
    ir.depolymorphize()

    // Inline calls to simplify the IR
    ir.inlineCalls(in: sourceModule, where: .hasNoControlFlow)

    let irSourceModule = ir.modules[sourceModule]!
    // var d = DiagnosticSet()
    // for f in irSourceModule.functions.keys {
    //     irSourceModule.removeDeadCode(in: f, diagnostics: &d)
    // }
    // ir.modules[sourceModule] = irSourceModule

    // if !d.isEmpty {
    //     print("Dead code elimination completed with diagnostics: \(d)")
    //     return
    // }

    guard
        let entryFunction = irSourceModule.functions.first(where: { f in
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
    let output = irSourceModule.coloredDescribeBlocksWithCalleeIdentifiers(in: entryFunction.key)

    // Write to file
    let outputFile = outputURL ?? URL(fileURLWithPath: "\(productName)")
    try output.write(
        to: outputFile.appendingPathExtension("ir.ansi"), atomically: true, encoding: .utf8)

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
    typealias PhysicalMemory = [Operand: [Int: Value]]
    var memory: PhysicalMemory = [:]

    // Track public input wires (function parameters)
    var publicInputWires: [WireID] = []

    // In Hylo IR, function parameters come as pointers. The last parameter is always
    // the return value pointer. All others are input parameters.

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
        memory[paramOperand] = [0: .runtime(paramWire)]

        print("Parameter \(index): wire \(paramWire) marked as public input")
    }

    // Update the public input count in R1CS
    r1cs.publicInputCount = UInt32(publicInputWires.count)

    // Last parameter is the pointer to the return value
    let returnValueParam = Operand.parameter(entryBlockId, entryBlock.inputs.count - 1)
    operandValues[returnValueParam] = .pointer(base: returnValueParam, offset: 0)

    // Helper to extract a wire from a register or parameter
    func getValue(for operand: Operand) -> Value {
        guard let val = operandValues[operand], case .value(let w) = val else {
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
            // If subfield indices are bytes, we might need type info here.
            // let addedOffset = subfieldView.subfield  // TODO: Parse indices to get real offset

            operandValues[.register(instructionId)] = .pointer(
                base: base, offset: currentOffset + 0)
        case let store as IR.Store:
            let valueToStore: Value

            switch store.object {
            case .constant(let c):
                guard let value = c as? IntegerConstant else {
                    fatalError("Unsupported constant type for store: \(c.type)")
                }

                valueToStore = Value.compileTime(r1cs.numberToField(BigInt(value.value)))

            case .register(let instructionID):
                valueToStore = getValue(for: .register(instructionID))
            case .parameter(let blockId, let index):
                valueToStore = getValue(for: .parameter(blockId, index))
            }
            // Resolve the Address
            let (targetBase, offset) = getPointer(for: store.target)

            // Update Physical Memory
            if memory[targetBase] == nil {
                memory[targetBase] = [:]  // Initialize new base if needed (why is this needed)
            }
            memory[targetBase]![offset] = valueToStore
        case let access as IR.Access:
            // Resolve pointer
            let (base, offset) = getPointer(for: access.source)

            // The result points to the same location, this is just an alias
            operandValues[.register(instructionId)] = .pointer(base: base, offset: offset)

        case let load as IR.Load:
            // Resolve pointer
            let (base, offset) = getPointer(for: load.source)

            // Retrieve wire from memory
            guard let loadedValue = memory[base]?[offset] else {
                fatalError("Reading from uninitialized memory at \(base) + \(offset)")
            }

            // The result of a Load instruction is a Value
            operandValues[.register(instructionId)] = .value(loadedValue)

        case let memcpy as IR.MemoryCopy:
            let sourcePointer = getPointer(for: memcpy.source)
            let targetPointer = getPointer(for: memcpy.target)

            let sourcePointee = memory[sourcePointer.base]![sourcePointer.offset]!

            // Update memory at target pointer to point to source wire
            if memory[targetPointer.base] == nil {
                memory[targetPointer.base] = [:]
            }
            memory[targetPointer.base]![targetPointer.offset] = sourcePointee

        case let callBuiltin as IR.CallBuiltinFunction:
            let resultOperand = Operand.register(instructionId)

            switch callBuiltin.callee {
            case .add(_, _), .mul(_, _), .sub(_, _):
                precondition(
                    callBuiltin.operands.count == 2,
                    "\(callBuiltin.callee) must have exactly 2 operands")

                let leftOperand = callBuiltin.operands[0]
                let rightOperand = callBuiltin.operands[1]

                let a = getValue(for: leftOperand)
                let b = getValue(for: rightOperand)

                if case .compileTime(let aConst) = a,
                    case .compileTime(let bConst) = b
                {
                    let resultConst: BigUInt
                    switch callBuiltin.callee {
                    case .add(_, _):
                        resultConst = add(comptimeA: aConst, comptimeB: bConst, r1cs: &r1cs)
                    case .sub(_, _):
                        resultConst = subtract(comptimeA: aConst, comptimeB: bConst, r1cs: &r1cs)
                    case .mul(_, _):
                        resultConst = multiply(comptimeA: aConst, comptimeB: bConst, r1cs: &r1cs)
                    default:
                        fatalError("Unsupported builtin function: \(callBuiltin.callee)")
                    }
                    operandValues[resultOperand] = .value(.compileTime(resultConst))
                } else if case .runtime(let aWire) = a, case .runtime(let bWire) = b {
                    let resultWire: WireID

                    switch callBuiltin.callee {
                    case .add(_, _):
                        resultWire = add(runtimeA: aWire, runtimeB: bWire, r1cs: &r1cs)
                        witnessGen.recordAdd(destination: resultWire, a: a, b: b)
                    case .sub(_, _):
                        resultWire = subtract(runtimeA: aWire, runtimeB: bWire, r1cs: &r1cs)
                        witnessGen.recordSub(destination: resultWire, a: a, b: b)
                    case .mul(_, _):
                        resultWire = multiply(runtimeA: aWire, runtimeB: bWire, r1cs: &r1cs)
                        witnessGen.recordMul(destination: resultWire, a: a, b: b)

                    default:
                        fatalError(
                            "\(red)Unsupported builtin function: \(callBuiltin.callee)\(reset)")
                    }

                    operandValues[resultOperand] = .value(.runtime(resultWire))
                } else if case .runtime(let aWire) = a, case .compileTime(let bConst) = b {
                    let resultValue: Value

                    switch callBuiltin.callee {
                    case .add(_, _):
                        resultValue = add(
                            runtimeA: aWire, comptimeB: bConst, r1cs: &r1cs, witnessGen: &witnessGen
                        )
                    case .sub(_, _):
                        resultValue = subtract(
                            runtimeA: aWire, comptimeB: bConst, r1cs: &r1cs, witnessGen: &witnessGen
                        )
                    case .mul(_, _):
                        resultValue = multiply(
                            runtimeA: aWire, comptimeB: bConst, r1cs: &r1cs, witnessGen: &witnessGen
                        )
                    default:
                        fatalError(
                            "\(red)Unsupported builtin function: \(callBuiltin.callee)\(reset)")
                    }

                    operandValues[resultOperand] = .value(resultValue)
                } else if case .compileTime(let aConst) = a, case .runtime(let bWire) = b {
                    let resultValue: Value

                    switch callBuiltin.callee {
                    case .add(_, _):
                        resultValue = add(
                            runtimeA: bWire, comptimeB: aConst, r1cs: &r1cs, witnessGen: &witnessGen
                        )
                    case .sub(_, _):
                        resultValue = .runtime(
                            subtract(
                                comptimeA: aConst, runtimeB: bWire, r1cs: &r1cs,
                                witnessGen: &witnessGen))
                    case .mul(_, _):
                        resultValue = multiply(
                            runtimeA: bWire, comptimeB: aConst, r1cs: &r1cs, witnessGen: &witnessGen
                        )
                    default:
                        fatalError(
                            "\(red)Unsupported builtin function: \(callBuiltin.callee)\(reset)")
                    }

                    operandValues[resultOperand] = .value(resultValue)
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

    print("\nFinal Memory:")
    for (base, offsets) in memory.sorted(by: { "\($0.key)" < "\($1.key)" }) {
        for (offset, value) in offsets.sorted(by: { $0.key < $1.key }) {
            let valueStr: String
            switch value {
            case .runtime(let wire):
                valueStr = "wire(\(wire))"
            case .compileTime(let constant):
                valueStr = "const(\(magenta)\(constant)\(reset))"
            }
            print("  \(base)\(grey)+\(offset)\(reset): \(valueStr)")
        }
    }

    guard let returnValue = memory[returnValueParam]?[0] else {
        fatalError("Warning: No return value found in memory")
    }

    let returnWire: WireID
    switch returnValue {
    case .compileTime(let constValue):
        print("\(green)Return value is compile-time constant: \(constValue)\(reset)")
        returnWire = r1cs.addWire()
        r1cs.addConstraint(.constant(wire: returnWire, value: constValue))
        witnessGen.recordConstant(wire: returnWire, value: constValue)
    case .runtime(let wire):
        returnWire = wire
    }

    print("Return value: \(returnWire) marked as public output")

    try String(describing: r1cs).write(
        to: outputURL!.appendingPathExtension("r1cs.ansi"), atomically: true, encoding: .utf8)

    try r1cs.serialize(to: outputURL!.appendingPathExtension("r1cs"))

    try witnessGen.generateCode(outputWire: returnWire).write(
        to: outputURL!.appendingPathExtension("witnessgen.js"),
        atomically: true,
        encoding: .utf8)
}

func add(comptimeA: BigUInt, comptimeB: BigUInt, r1cs: inout R1CS) -> BigUInt {
    return (comptimeA + comptimeB) % r1cs.prime
}
func add(
    runtimeA: WireID, comptimeB: BigUInt, r1cs: inout R1CS,
    witnessGen: inout some WitnessGeneratorGen
) -> Value {
    if comptimeB == 0 {
        return .runtime(runtimeA)
    }

    // x = a + b
    // x - a - b = 0
    // (x) * (1) - (a + b) = 0
    // A: x
    // B: 1
    // C: a, b
    let resultWire = r1cs.addWire()
    r1cs.addConstraint(
        .init(
            a: .wire(resultWire),
            b: .constant(1),
            c: LinearCombination(terms: [
                (wire: runtimeA, coefficient: 1),
                (wire: .one, coefficient: comptimeB),
            ])
        ))

    witnessGen.recordAdd(destination: resultWire, a: .runtime(runtimeA), b: .compileTime(comptimeB))

    return .runtime(resultWire)
}
func add(runtimeA: WireID, runtimeB: WireID, r1cs: inout R1CS) -> WireID {
    // x = a + b
    // x - a - b = 0
    // (x) * (1) - (a + b) = 0
    // A: x
    // B: 1
    // C: a, b
    let x = r1cs.addWire()
    r1cs.addConstraint(
        .init(
            a: .wire(x),
            b: .one,
            c: LinearCombination(terms: [
                (wire: runtimeA, coefficient: 1),
                (wire: runtimeB, coefficient: 1),
            ])
        ))
    return x
}
func subtract(comptimeA: BigUInt, comptimeB: BigUInt, r1cs: inout R1CS) -> BigUInt {
    return (comptimeA + (r1cs.prime - comptimeB)) % r1cs.prime
}
func subtract(runtimeA: WireID, runtimeB: WireID, r1cs: inout R1CS) -> WireID {
    // HYLO IR: x := a - b
    // x - a + b = 0
    // (x) * (1) + (-1a + 1b) = 0
    // (x) * (1) - (1a + -1b) = 0
    // A: x
    // B: 1
    // C: a + -1b
    let resultWire = r1cs.addWire()
    r1cs.addConstraint(
        .init(
            a: .wire(resultWire),
            b: .one,
            c: LinearCombination(terms: [
                (wire: runtimeA, coefficient: 1),
                (wire: runtimeB, coefficient: r1cs.prime - 1),
            ])))

    return resultWire
}
func subtract(
    runtimeA: WireID, comptimeB: BigUInt, r1cs: inout R1CS,
    witnessGen: inout some WitnessGeneratorGen
) -> Value {
    if comptimeB == 0 {
        return .runtime(runtimeA)
    }

    // x = a - b
    // (x) * (1) = (a + -1b)
    // A: x
    // B: 1
    // C: a + -1b
    let x = r1cs.addWire()
    r1cs.addConstraint(
        .init(
            a: .wire(x),
            b: .one,
            c: LinearCombination(terms: [
                (wire: runtimeA, coefficient: 1),
                (wire: .one, coefficient: r1cs.prime - comptimeB),
            ])
        ))
    witnessGen.recordSub(destination: x, a: .runtime(runtimeA), b: .compileTime(comptimeB))

    return .runtime(x)
}

func subtract(
    comptimeA: BigUInt, runtimeB: WireID, r1cs: inout R1CS,
    witnessGen: inout some WitnessGeneratorGen
) -> WireID {
    if comptimeA == 0 {
        // x = 0 - b
        // (x) * (1) = -1b
        // A: x
        // B: 1
        // C: -1b
        let x = r1cs.addWire()
        r1cs.addConstraint(
            .init(
                a: .wire(x),
                b: .one,
                c: LinearCombination(terms: [
                    (wire: runtimeB, coefficient: r1cs.prime - 1)
                ])
            ))

        witnessGen.recordSub(destination: x, a: .compileTime(comptimeA), b: .runtime(runtimeB))

        return x
    }

    // x = a - b
    // (x) * (1) = (a + -1b)
    // A: x
    // B: 1
    // C: a + -1b
    let x = r1cs.addWire()
    r1cs.addConstraint(
        .init(
            a: .wire(x),
            b: .one,
            c: LinearCombination(terms: [
                (wire: .one, coefficient: comptimeA),
                (wire: runtimeB, coefficient: r1cs.prime - 1),
            ])
        ))
    witnessGen.recordSub(destination: x, a: .compileTime(comptimeA), b: .runtime(runtimeB))

    return x
}
func multiply(
    runtimeA: WireID, comptimeB: BigUInt, r1cs: inout R1CS,
    witnessGen: inout some WitnessGeneratorGen
) -> Value {
    if comptimeB == 0 {
        return .compileTime(0)
    }
    if comptimeB == 1 {
        return .runtime(runtimeA)
    }

    let resultWire = r1cs.addWire()

    r1cs.addConstraint(
        .init(
            a: .wire(runtimeA),
            b: .constant(comptimeB),
            c: .wire(resultWire)
        ))
    witnessGen.recordMul(destination: resultWire, a: .runtime(runtimeA), b: .compileTime(comptimeB))
    return .runtime(resultWire)
}

func multiply(runtimeA: WireID, runtimeB: WireID, r1cs: inout R1CS) -> WireID {
    let resultWire = r1cs.addWire()

    r1cs.addConstraint(
        .init(
            a: .wire(runtimeA),
            b: .wire(runtimeB),
            c: .wire(resultWire)
        ))

    return resultWire
}

func multiply(comptimeA: BigUInt, comptimeB: BigUInt, r1cs: inout R1CS) -> BigUInt {
    return (comptimeA * comptimeB) % r1cs.prime
}

/// Errors that can occur during R1CS generation.
public enum R1CSGenerationError: Error {
    case noEntryFunction
    case moreThanOneBlockInEntryFunction
}
