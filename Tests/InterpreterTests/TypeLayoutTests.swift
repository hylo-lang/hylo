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
    XCTAssertEqual(i8.components.count, 0)

    let i16 = c[^BuiltinType.i(16)]
    XCTAssertEqual(i16.size, 2)
    XCTAssertEqual(i16.alignment, 2)
    XCTAssertEqual(i16.components.count, 0)
  }

}
