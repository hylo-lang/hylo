import BigInt
import R1CS

protocol WitnessGeneratorGen {
    init(prime: BigUInt, parameterCount: Int)

    mutating func recordInput(wire: WireID, argIndex: Int)
    mutating func recordConstant(wire: WireID, value: BigUInt)
    mutating func recordAssignment(destination: WireID, source: WireID)
    mutating func recordAdd(destination: WireID, a: WireID, b: WireID)
    mutating func recordSub(destination: WireID, a: WireID, b: WireID)
    mutating func recordMul(destination: WireID, a: WireID, b: WireID)

    func generateCode(outputWire: WireID) -> String
}
