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
    let i1 = c[^BuiltinType.i(1)]
    XCTAssertEqual(i1.size, 1)
    XCTAssertEqual(i1.alignment, 1)
    XCTAssert(i1.parts.isEmpty)

    let i8 = c[^BuiltinType.i(8)]
    XCTAssertEqual(i8.size, 1)
    XCTAssertEqual(i8.alignment, 1)
    XCTAssert(i8.parts.isEmpty)

    let i16 = c[^BuiltinType.i(16)]
    XCTAssertEqual(i16.size, 2)
    XCTAssertEqual(i16.alignment, 2)
    XCTAssert(i16.parts.isEmpty)

    let i32 = c[^BuiltinType.i(32)]
    XCTAssertEqual(i32.size, 4)
    XCTAssertEqual(i32.alignment, 4)
    XCTAssert(i32.parts.isEmpty)

    let i64 = c[^BuiltinType.i(64)]
    XCTAssertEqual(i64.size, 8)
    XCTAssertEqual(i64.alignment, 8)
    XCTAssert(i64.parts.isEmpty)
  }

  func testTrivialTuples() throws {
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())

    let void = c[.void]
    XCTAssertEqual(void.size, 0)
    XCTAssertEqual(void.alignment, 1)
    XCTAssert(void.parts.isEmpty)


    let justI8 = c[^TupleType(types: [^BuiltinType.i(8)])]
    let i8 = c[^BuiltinType.i(8)]
    XCTAssertEqual(justI8.bytes, i8.bytes)
    XCTAssertEqual(justI8.parts, [.init(name: "0", type: ^BuiltinType.i(8), offset: 0)])
  }

  func testPairs() throws {
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())

    let i8i64 = c[^TupleType(types: [^BuiltinType.i(8), ^BuiltinType.i(64)])]
    XCTAssertEqual(i8i64.bytes, .init(alignment: 8, size: 16))
    let i64i8 = c[^TupleType(types: [^BuiltinType.i(64), ^BuiltinType.i(8)])]
    XCTAssertEqual(i64i8.bytes, .init(alignment: 8, size: 9))

    XCTAssertEqual(
      i8i64.parts,
      [ .init(name: "0", type: ^BuiltinType.i(8), offset: 0),
        .init(name: "1", type: ^BuiltinType.i(64), offset: 8)])

    XCTAssertEqual(
      i64i8.parts,
      [ .init(name: "0", type: ^BuiltinType.i(64), offset: 0),
        .init(name: "1", type: ^BuiltinType.i(8), offset: 8)])
  }

  func testTriple() {
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())

    let i8i16i32 = c[^TupleType(types: [^BuiltinType.i(8), ^BuiltinType.i(16), ^BuiltinType.i(32)])]
    XCTAssertEqual(i8i16i32.bytes, .init(alignment: 4, size: 8))

    XCTAssertEqual(
      i8i16i32.parts,
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
      a.parts,
      [ .init(name: "__f", type: ^BuiltinType.ptr, offset: 0),
        .init(name: "__e", type: ^TupleType(types: [^BuiltinType.i(8)]), offset: 8)
      ])
  }

  func testUnion() throws {
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())
    let u = c[^UnionType([^BuiltinType.i(8), ^BuiltinType.i(16)])]

    XCTAssertEqual(u.bytes, .init(alignment: 2, size: 3))
    XCTAssertEqual(
      u.parts,
      [ .init(name: "i8", type: ^BuiltinType.i(8), offset: 0),
        .init(name: "i16", type: ^BuiltinType.i(16), offset: 0),
        .init(name: "discriminator", type: ^BuiltinType.i(8), offset: 2)
      ])
  }

  func testWideUnion() throws {
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())
    var parts = [^BuiltinType.i(8)]
    while parts.count <= 256 {
      parts.append(^TupleType(types: [parts.last!]))
    }

    let u = c[^UnionType(parts)]
    XCTAssertEqual(u.bytes, .init(alignment: 2, size: 3))
    XCTAssertEqual(
      u.parts[0], .init(name: "i8", type: ^BuiltinType.i(8), offset: 2))
    XCTAssertEqual(u.parts[1].offset, 2)
    XCTAssertEqual(u.parts.last!, .init(name: "discriminator", type: ^BuiltinType.i(16), offset: 0))
  }

  func testSubFieldLayout() throws {
    var m = Memory()
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())
    let t = ^TupleType(types: [
      ^BuiltinType.i(32), ^TupleType(types: [^BuiltinType.i(32), ^BuiltinType.i(8)]),
    ])
    let a = m.allocate(c[t].size, bytesWithAlignment: c[t].alignment)
    let x = c.address(of: [1, 1], in: Address(startLocation: a, type: c[t]))
    XCTAssertEqual(
      x,
      Address(
        startLocation: .init(allocation: a.allocation, offset: 8),
        type: c[^BuiltinType.i(8)]
      )
    )
    // TODO: add test for union case.
  }

}
