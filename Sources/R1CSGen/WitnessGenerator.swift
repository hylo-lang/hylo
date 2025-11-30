import BigInt
import R1CS

protocol WitnessGeneratorGen {
    init(prime: BigUInt, parameterCount: Int)

    mutating func recordInput(wire: WireID, argIndex: Int)
    mutating func recordAdd(destination: WireID, a: Value, b: Value)
    mutating func recordSub(destination: WireID, a: Value, b: Value)
    mutating func recordMul(destination: WireID, a: Value, b: Value)
    mutating func recordConstant(wire: WireID, value: BigUInt)

    func generateCode(outputWire: WireID) -> String
}
