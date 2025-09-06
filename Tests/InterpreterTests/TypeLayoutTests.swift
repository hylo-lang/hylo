import XCTest
import FrontEnd
@testable import Interpreter

func makeEmptyProgram() -> TypedProgram {
  var d = DiagnosticSet()
  return try! TypedProgram(annotating: ScopedProgram.init(AST()), reportingDiagnosticsTo: &d)
}

final class TypeLayoutTests: XCTestCase {

  let emptyProgram = makeEmptyProgram()

  func testBuiltinIntegers() throws {
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())
    let i8 = c[^BuiltinType.i(8)]
    XCTAssertEqual(i8.size, 1)
    XCTAssertEqual(i8.alignment, 1)
    XCTAssert(i8.components.isEmpty)

    let i16 = c[^BuiltinType.i(16)]
    XCTAssertEqual(i16.size, 2)
    XCTAssertEqual(i16.alignment, 2)
    XCTAssert(i16.components.isEmpty)

    let i32 = c[^BuiltinType.i(32)]
    XCTAssertEqual(i32.size, 4)
    XCTAssertEqual(i32.alignment, 4)
    XCTAssert(i32.components.isEmpty)

    let i64 = c[^BuiltinType.i(64)]
    XCTAssertEqual(i64.size, 8)
    XCTAssertEqual(i64.alignment, 8)
    XCTAssert(i64.components.isEmpty)
  }

  func testTrivialTuples() throws {
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())

    let void = c[.void]
    XCTAssertEqual(void.size, 0)
    XCTAssertEqual(void.alignment, 1)
    XCTAssert(void.components.isEmpty)


    let justI8 = c[^TupleType(types: [^BuiltinType.i(8)])]
    let i8 = c[^BuiltinType.i(8)]
    XCTAssertEqual(justI8.bytes, i8.bytes)
    XCTAssertEqual(justI8.components, [.init(name: "0", type: ^BuiltinType.i(8), offset: 0)])
  }

}
