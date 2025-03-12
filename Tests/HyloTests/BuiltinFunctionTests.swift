import FrontEnd
import Utils
import XCTest

final class BuiltinFunctionTests: XCTestCase {

  func testInvalid() {
    XCTAssertNil(BuiltinFunction("add"))
  }

  func testAdvancedByBytes() throws {
    let expectedType = { ArrowType(.builtin(.ptr), .builtin(.i($0)), to: .builtin(.ptr)) }
    try assertParse(
      instructions: ["advanced_by_bytes"],
      parameterizedBy: [["i16"]],
      createInstanceWithType: expectedType(16))
    try assertParse(
      instructions: ["advanced_by_bytes"],
      parameterizedBy: [["i64"]],
      createInstanceWithType: expectedType(64))
  }

  func testIntegerArithmetic() throws {
    let expectedType = ArrowType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      instructions: ["add", "sub", "mul"],
      parameterizedBy: [["i64"], ["nuw", "i64"], ["nsw", "i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerDivision() throws {
    let expectedType = ArrowType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      instructions: ["udiv", "sdiv"],
      parameterizedBy: [["i64"], ["exact", "i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerShiftLeft() throws {
    let expectedType = ArrowType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      instructions: ["shl"],
      parameterizedBy: [["i64"], ["nuw", "i64"], ["nsw", "i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerShiftRight() throws {
    let expectedType = ArrowType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      instructions: ["lshr", "ashr"],
      parameterizedBy: [["i64"], ["exact", "i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerRemainder() throws {
    let expectedType = ArrowType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      instructions: ["urem", "srem"],
      parameterizedBy: [["i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerLogic() throws {
    let expectedType = ArrowType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      instructions: ["and", "or", "xor"],
      parameterizedBy: [["i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerComparison() throws {
    let expectedType = ArrowType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(1)))
    try assertParse(
      instructions: ["icmp"],
      parameterizedBy: [
        ["eq", "i64"],
        ["ne", "i64"],
        ["ugt", "i64"],
        ["uge", "i64"],
        ["ult", "i64"],
        ["ule", "i64"],
        ["sgt", "i64"],
        ["sge", "i64"],
        ["slt", "i64"],
        ["sle", "i64"],
      ],
      createInstanceWithType: expectedType)
  }

  func testIntegerTruncate() throws {
    let expectedType = ArrowType(.builtin(.i(64)), to: .builtin(.i(32)))
    try assertParse(
      instructions: ["trunc"],
      parameterizedBy: [["i64", "i32"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerExtend() throws {
    let expectedType = ArrowType(.builtin(.i(32)), to: .builtin(.i(64)))
    try assertParse(
      instructions: ["zext", "sext"],
      parameterizedBy: [["i32", "i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerToFloat() throws {
    let expectedType = ArrowType(.builtin(.i(64)), to: .builtin(.float64))
    try assertParse(
      instructions: ["uitofp", "sitofp"],
      parameterizedBy: [["i64", "float64"]],
      createInstanceWithType: expectedType)
  }

  func testFloatArithmetic() throws {
    let expectedType = ArrowType(.builtin(.float64), .builtin(.float64), to: .builtin(.float64))
    try assertParse(
      instructions: ["fadd", "fsub", "fmul", "fdiv", "frem"],
      parameterizedBy: [
        ["float64"],
        ["fast", "float64"],
        ["ninf", "nnan", "float64"],
      ],
      createInstanceWithType: expectedType)
  }

  func testFloatComparison() throws {
    let expectedType = ArrowType(.builtin(.float64), .builtin(.float64), to: .builtin(.i(1)))
    try assertParse(
      instructions: ["fcmp"],
      parameterizedBy: [
        ["oeq", "float64"],
        ["ogt", "float64"],
        ["oge", "float64"],
        ["olt", "float64"],
        ["ole", "float64"],
        ["one", "float64"],
        ["ord", "float64"],
        ["ueq", "float64"],
        ["ugt", "float64"],
        ["uge", "float64"],
        ["ult", "float64"],
        ["ule", "float64"],
        ["une", "float64"],
        ["uno", "float64"],
        ["true", "float64"],
        ["false", "float64"],
        ["fast", "false", "float64"],
        ["ninf", "nnan", "false", "float64"],
      ],
      createInstanceWithType: expectedType)
  }

  func testFloatTruncate() throws {
    let expectedType = ArrowType(.builtin(.float64), to: .builtin(.float32))
    try assertParse(
      instructions: ["fptrunc"],
      parameterizedBy: [["float64", "float32"]],
      createInstanceWithType: expectedType)
  }

  func testFloatExtend() throws {
    let expectedType = ArrowType(.builtin(.float32), to: .builtin(.float64))
    try assertParse(
      instructions: ["fpext"],
      parameterizedBy: [["float32", "float64"]],
      createInstanceWithType: expectedType)
  }

  func testFloatToInteger() throws {
    let expectedType = ArrowType(.builtin(.float32), to: .builtin(.i(64)))
    try assertParse(
      instructions: ["fptoui", "fptosi"],
      parameterizedBy: [["float32", "i64"]],
      createInstanceWithType: expectedType)
  }

  func testCountOnes() throws {
    let expectedType = ArrowType(.builtin(.i(16)), to: .builtin(.i(16)))
    try assertParse(
      instructions: ["ctpop"],
      parameterizedBy: [["i16"]],
      createInstanceWithType: expectedType)
  }

  func testCountLeadingZeros() throws {
    let expectedType = ArrowType(.builtin(.i(16)), to: .builtin(.i(16)))
    try assertParse(
      instructions: ["ctlz"],
      parameterizedBy: [["i16"]],
      createInstanceWithType: expectedType)
  }

  func testCountTrailingZeros() throws {
    let expectedType = ArrowType(.builtin(.i(16)), to: .builtin(.i(16)))
    try assertParse(
      instructions: ["cttz"],
      parameterizedBy: [["i16"]],
      createInstanceWithType: expectedType)
  }

  func testZeroInitializer() throws {
    let expectedType = ArrowType(to: .builtin(.i(64)))
    try assertParse(
      instructions: ["zeroinitializer"],
      parameterizedBy: [["i64"]],
      createInstanceWithType: expectedType)
  }

  func testAtomicLoad() throws {
    let expectedType = ArrowType(.builtin(.ptr), to: .builtin(.i(64)))
    try assertParse(
      instructions: ["atomic_load"],
      parameterizedBy: [
        ["relaxed", "i64"],
        ["acquire", "i64"],
        ["seqcst", "i64"]
      ],
      createInstanceWithType: expectedType)
  }

  func testAtomicStore() throws {
    let expectedType = ArrowType(.builtin(.ptr), .builtin(.i(64)), to: .void)
    try assertParse(
      instructions: ["atomic_store"],
      parameterizedBy: [
        ["relaxed", "i64"],
        ["release", "i64"],
        ["seqcst", "i64"]
      ],
      createInstanceWithType: expectedType)
  }

  func testAtomicRMWForSignedIntegers() throws {
    let expectedType = ArrowType(.builtin(.ptr), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      instructions: [
        "atomic_swap",
        "atomic_add",
        "atomic_sub",
        "atomic_max",
        "atomic_min",
        "atomic_and",
        "atomic_nand",
        "atomic_or",
        "atomic_xor"
      ],
      parameterizedBy: [
        ["relaxed", "i64"],
        ["acquire", "i64"],
        ["release", "i64"],
        ["acqrel", "i64"],
        ["seqcst", "i64"]
      ],
      createInstanceWithType: expectedType)
  }

  func testAtomicRMWForUnsignedIntegers() throws {
    let expectedType = ArrowType(.builtin(.ptr), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      instructions: [
        "atomic_umax",
        "atomic_umin"
      ],
      parameterizedBy: [
        ["relaxed", "i64"],
        ["acquire", "i64"],
        ["release", "i64"],
        ["acqrel", "i64"],
        ["seqcst", "i64"]
      ],
      createInstanceWithType: expectedType)
  }

  func testAtomicRMWForFloatingNumbers() throws {
    let expectedType = ArrowType(.builtin(.ptr), .builtin(.float64), to: .builtin(.float64))
    try assertParse(
      instructions: [
        "atomic_swap",
        "atomic_fadd",
        "atomic_fsub",
        "atomic_fmax",
        "atomic_fmin"
      ],
      parameterizedBy: [
        ["relaxed", "float64"],
        ["acquire", "float64"],
        ["release", "float64"],
        ["acqrel", "float64"],
        ["seqcst", "float64"]
      ],
      createInstanceWithType: expectedType)
  }

  func testAtomicCompareExchange() throws {
    let expectedType = ArrowType(.builtin(.ptr), .builtin(.i(64)), .builtin(.i(64)), to: ^TupleType(types: [.builtin(.i(64)), .builtin(.i(1))]))
    try assertParse(
      instructions: [
        "atomic_cmpxchg",
        "atomic_cmpxchgweak"
      ],
      parameterizedBy: [
        ["relaxed", "relaxed", "i64"],
        ["relaxed", "acquire", "i64"],
        ["relaxed", "seqcst", "i64"],
        ["acquire", "relaxed", "i64"],
        ["acquire", "acquire", "i64"],
        ["acquire", "seqcst", "i64"],
        ["release", "relaxed", "i64"],
        ["release", "acquire", "i64"],
        ["release", "seqcst", "i64"],
        ["acqrel", "relaxed", "i64"],
        ["acqrel", "acquire", "i64"],
        ["acqrel", "seqcst", "i64"],
        ["seqcst", "relaxed", "i64"],
        ["seqcst", "acquire", "i64"],
        ["seqcst", "seqcst", "i64"]
      ],
      createInstanceWithType: expectedType)
  }

  func testAtomicFence() throws {
    let expectedType = ArrowType(to: .void)
    try assertParse(
      instructions: [
        "atomic_fence",
        "atomic_singlethreadfence"
      ],
      parameterizedBy: [
        ["acquire"],
        ["release"],
        ["acqrel"],
        ["seqcst"]
      ],
      createInstanceWithType: expectedType)
  }

  /// For each element in `instructions` and `parameters`, assert that parsing a built-in functions
  /// named after their concatenation creates an instance with the same stem and parameters, and
  /// whose type is `expectedType`.
  private func assertParse(
    instructions: [String],
    parameterizedBy parameters: [[String]],
    createInstanceWithType expectedType: ArrowType,
    file: StaticString = #filePath,
    line: UInt = #line
  ) throws {
    var i: UInt64 = 0
    func freshVariable() -> TypeVariable {
      i += 1
      return .init(i)
    }

    for s in instructions {
      for p in parameters {
        let n = "\(s)_\(list: p, joinedBy: "_")"
        let f = try XCTUnwrap(BuiltinFunction(n), file: file, line: line)
        XCTAssertEqual(f.description, n, file: file, line: line)
        XCTAssertEqual(
          f.type(makingFreshVariableWith: freshVariable), expectedType, file: file, line: line)
      }
    }
  }

}
