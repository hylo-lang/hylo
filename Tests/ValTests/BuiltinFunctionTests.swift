import Core
import Utils
import XCTest

final class BuiltinFunctionTests: XCTestCase {

  func testInvalid() {
    XCTAssertNil(BuiltinFunction(parsing: "add"))
  }

  func testIntegerArithmetic() throws {
    let expectedType = LambdaType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      stems: ["add", "sub", "mul"],
      parameterizedBy: [["i64"], ["nuw", "i64"], ["nsw", "i64"], ["nuw", "nsw", "i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerDivision() throws {
    let expectedType = LambdaType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      stems: ["udiv", "sdiv"],
      parameterizedBy: [["i64"], ["exact", "i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerShiftLeft() throws {
    let expectedType = LambdaType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      stems: ["shl"],
      parameterizedBy: [["i64"], ["nuw", "i64"], ["nsw", "i64"], ["nuw", "nsw", "i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerShiftRight() throws {
    let expectedType = LambdaType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      stems: ["lshr", "ashr"],
      parameterizedBy: [["i64"], ["exact", "i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerRemainder() throws {
    let expectedType = LambdaType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      stems: ["urem", "srem"],
      parameterizedBy: [["i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerLogic() throws {
    let expectedType = LambdaType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(64)))
    try assertParse(
      stems: ["and", "or", "xor"],
      parameterizedBy: [["i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerComparison() throws {
    let expectedType = LambdaType(.builtin(.i(64)), .builtin(.i(64)), to: .builtin(.i(1)))
    try assertParse(
      stems: ["icmp"],
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
        ["sle", "i64"]
      ],
      createInstanceWithType: expectedType)
  }

  func testIntegerTruncate() throws {
    let expectedType = LambdaType(.builtin(.i(64)), to: .builtin(.i(32)))
    try assertParse(
      stems: ["trunc"],
      parameterizedBy: [["i64", "i32"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerExtend() throws {
    let expectedType = LambdaType(.builtin(.i(32)), to: .builtin(.i(64)))
    try assertParse(
      stems: ["zext", "sext"],
      parameterizedBy: [["i32", "i64"]],
      createInstanceWithType: expectedType)
  }

  func testIntegerToFloat() throws {
    let expectedType = LambdaType(.builtin(.i(64)), to: .builtin(.double))
    try assertParse(
      stems: ["uitofp", "sitofp"],
      parameterizedBy: [["i64", "double"]],
      createInstanceWithType: expectedType)
  }

  func testFloatArithmetic() throws {
    let expectedType = LambdaType(.builtin(.double), .builtin(.double), to: .builtin(.double))
    try assertParse(
      stems: ["fadd", "fsub", "fmul", "fdiv", "frem"],
      parameterizedBy: [
        ["double"],
        ["fast", "double"],
        ["ninf", "nnan", "double"]
      ],
      createInstanceWithType: expectedType)
  }

  func testFloatComparison() throws {
    let expectedType = LambdaType(.builtin(.double), .builtin(.double), to: .builtin(.i(1)))
    try assertParse(
      stems: ["fcmp"],
      parameterizedBy: [
        ["oeq", "double"],
        ["ogt", "double"],
        ["oge", "double"],
        ["olt", "double"],
        ["ole", "double"],
        ["one", "double"],
        ["ord", "double"],
        ["ueq", "double"],
        ["ugt", "double"],
        ["uge", "double"],
        ["ult", "double"],
        ["ule", "double"],
        ["une", "double"],
        ["uno", "double"],
        ["true", "double"],
        ["false", "double"],
        ["fast", "false", "double"],
        ["ninf", "nnan", "false", "double"]
      ],
      createInstanceWithType: expectedType)
  }

  func testFloatTruncate() throws {
    let expectedType = LambdaType(.builtin(.double), to: .builtin(.float))
    try assertParse(
      stems: ["fptrunc"],
      parameterizedBy: [["double", "float"]],
      createInstanceWithType: expectedType)
  }

  func testFloatExtend() throws {
    let expectedType = LambdaType(.builtin(.float), to: .builtin(.double))
    try assertParse(
      stems: ["fpext"],
      parameterizedBy: [["float", "double"]],
      createInstanceWithType: expectedType)
  }

  func testFloatToInteger() throws {
    let expectedType = LambdaType(.builtin(.float), to: .builtin(.i(64)))
    try assertParse(
      stems: ["fptoui", "fptosi"],
      parameterizedBy: [["float", "i64"]],
      createInstanceWithType: expectedType)
  }

  func testZeroInitializer() throws {
    let expectedType = LambdaType(to: .builtin(.i(64)))
    try assertParse(
      stems: ["zeroinitializer"],
      parameterizedBy: [["i64"]],
      createInstanceWithType: expectedType)
  }

  /// For each element in `stems` and `parameters`, assert that parsing a built-in functions named
  /// after their concatenation creates an instance with the same stem and parameters, and whose
  /// type is `expectedType`.
  private func assertParse(
    stems: [String],
    parameterizedBy parameters: [[String]],
    createInstanceWithType expectedType: LambdaType,
    file: StaticString = #file,
    line: UInt = #line
  ) throws {
    for s in stems {
      for p in parameters {
        let f = try XCTUnwrap(
          BuiltinFunction(parsing: "\(s)_\(list: p, joinedBy: "_")"),
          file: file, line: line)

        XCTAssertEqual(f.baseName, s, file: file, line: line)
        XCTAssertEqual(f.type, expectedType, file: file, line: line)
        XCTAssert(
          zip(f.genericParameters, p).allSatisfy({ $0 == $1 }),
          "\(f.genericParameters) is not equal to \(p)",
          file: file, line: line)
      }
    }
  }

}
