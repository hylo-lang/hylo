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

  func testPairs() throws {
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())

    let i8i64 = c[^TupleType(types: [^BuiltinType.i(8), ^BuiltinType.i(64)])]
    XCTAssertEqual(i8i64.bytes, .init(alignment: 8, size: 16))
    let i64i8 = c[^TupleType(types: [^BuiltinType.i(64), ^BuiltinType.i(8)])]
    XCTAssertEqual(i64i8.bytes, .init(alignment: 8, size: 9))

    XCTAssertEqual(
      i8i64.components,
      [ .init(name: "0", type: ^BuiltinType.i(8), offset: 0),
        .init(name: "1", type: ^BuiltinType.i(64), offset: 8)])

    XCTAssertEqual(
      i64i8.components,
      [ .init(name: "0", type: ^BuiltinType.i(64), offset: 0),
        .init(name: "1", type: ^BuiltinType.i(8), offset: 8)])
  }

  func testTriple() {
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())

    let i8i16i32 = c[^TupleType(types: [^BuiltinType.i(8), ^BuiltinType.i(16), ^BuiltinType.i(32)])]
    XCTAssertEqual(i8i16i32.bytes, .init(alignment: 4, size: 8))

    XCTAssertEqual(
      i8i16i32.components,
      [ .init(name: "0", type: ^BuiltinType.i(8), offset: 0),
        .init(name: "1", type: ^BuiltinType.i(16), offset: 2),
        .init(name: "2", type: ^BuiltinType.i(32), offset: 4),
      ])
  }

  func testArrow() throws {
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())
    let a = c[
      ^ArrowType(
        receiverEffect: .set,
        environment: ^TupleType(types: [^BuiltinType.i(8)]),
        inputs: [CallableTypeParameter(type: ^BuiltinType.i(16))],
        output: ^BuiltinType.i(32))]

    XCTAssertEqual(a.bytes, .init(alignment: 8, size: 9))
    XCTAssertEqual(
      a.components,
      [ .init(name: "__f", type: ^BuiltinType.ptr, offset: 0),
        .init(name: "__e", type: ^TupleType(types: [^BuiltinType.i(8)]), offset: 8)
      ])
  }

  func testUnion() throws {
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())
    let u = c[^UnionType([^BuiltinType.i(8), ^BuiltinType.i(16)])]

    XCTAssertEqual(u.bytes, .init(alignment: 2, size: 3))
    XCTAssertEqual(
      u.components,
      [ .init(name: "i8", type: ^BuiltinType.i(8), offset: 0),
        .init(name: "i16", type: ^BuiltinType.i(16), offset: 0),
        .init(name: "discriminator", type: ^BuiltinType.i(8), offset: 2)
      ])
  }

  func testWideUnion() throws {
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())
    var components = [^BuiltinType.i(8)]
    while components.count <= 256 {
      components.append(^TupleType(types: [components.last!]))
    }

    let u = c[^UnionType(components)]
    XCTAssertEqual(u.bytes, .init(alignment: 2, size: 3))
    XCTAssertEqual(
      u.components[0], .init(name: "i8", type: ^BuiltinType.i(8), offset: 2))
    XCTAssertEqual(u.components[1].offset, 2)
    XCTAssertEqual(u.components.last!, .init(name: "discriminator", type: ^BuiltinType.i(16), offset: 0))
  }

}
