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
    let output = irSourceModule.describeBlocksWithCalleeIdentifiers(in: entryFunction.key)

    print("described IR ")
    print(output)

    print("----------------------------")

    // Write to file
    let outputFile = outputURL ?? URL(fileURLWithPath: "\(productName).ir")
    try output.write(to: outputFile, atomically: true, encoding: .utf8)

    var r1cs = R1CS(prime: 87_178_291_199)

    let entryBlockId = irSourceModule.blocks(in: entryFunction.key).first!
    let entryBlock = irSourceModule[entryBlockId]

    guard entryFunction.value.blocks.count == 1 else {
        throw R1CSGenerationError.moreThanOneBlockInEntryFunction
    }

    // Maps SSA Register ID -> Value (Wire) OR Address (Pointer)
    var operandValues: [Operand: AbstractValue] = [:]

    // Maps a Base Allocation ID (e.g. %i0.0) -> (Offset -> WireID)
    typealias PhysicalMemory = [Operand: [Int: WireID]]
    var memory: PhysicalMemory = [:]


    // Add wires for all block parameters:
    for index in entryBlock.inputs.indices.dropLast() {
        let label = r1cs.nextLabel()
        let wire = r1cs.addWire(labelId: label)
        operandValues[.parameter(entryBlockId, index)] = .wire(wire)
    }

    // Last parameter is the pointer to the return value
    let returnValueParam = Operand.parameter(entryBlockId, entryBlock.inputs.count - 1)
    memory[returnValueParam] = nil
    operandValues[returnValueParam] = .pointer(base: returnValueParam, offset: 0)


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
            operandValues[.register(instructionId)] = .pointer(base: .register(instructionId), offset: 0)
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
            print("Store instruction: \(store)")

            let valueWire: WireID

            switch store.object {
            case .constant(let c):
                guard let value = c as? IntegerConstant else {
                    fatalError("Unsupported constant type for store: \(c.type)")
                }

                valueWire = r1cs.addWire(labelId: r1cs.nextLabel())
                r1cs.addConstraint(.constant(wire: valueWire, value: .init(value.value)))  // TODO: handle negative numbers
            case .register(let instructionID):
                valueWire = getWire(for: .register(instructionID))
            case .parameter(let blockId, let index):
                valueWire = getWire(for: .parameter(blockId, index))
            }
            // Resolve the Address
            let (base, offset) = getPointer(for: store.target)

            // Update Physical Memory
            if memory[base] == nil {
                memory[base] = [:]  // Initialize new base if needed (why is this needed)
            }
            memory[base]![offset] = valueWire
        case let access as IR.Access:
            print("Access instruction: \(access)")

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

        case let callBuiltin as IR.CallBuiltinFunction:
            print("Calling builtin function: \(callBuiltin.callee)")

            let resultLabel = r1cs.nextLabel()

            /// Result wire
            let x = r1cs.addWire(labelId: resultLabel)
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

        default:
            print("   -  \(instruction)")
        }
    }

    print("R1CS Output: ..............................................")
    print(r1cs)

    try String(describing: r1cs).write(
        to: outputURL!.appendingPathExtension("r1cs.ansi"), atomically: true, encoding: .utf8)

    return output
}

/// Errors that can occur during R1CS generation.
public enum R1CSGenerationError: Error {
    case noEntryFunction
    case moreThanOneBlockInEntryFunction
}
