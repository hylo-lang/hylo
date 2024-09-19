import FrontEnd
import IR
import StandardLibrary
import TestUtils
import Utils
import XCTest

final class ManglingTests: XCTestCase {

  func testDeclarations() throws {
    let text = """
      import Hylo

      namespace Stash {
        public type A {}

        trait Indexable {
          value size
          type Index
        }

        typealias Number = Int
      }

      type Angle {
        public var radians: Float64
        public memberwise init
        public init(degrees: Float64) {
          &self.radians = 0.0
        }
        public property degrees: Float64 {
          let { 0.0 }
          inout { var x = 0.0; yield &x }
        }
      }

      type B<X> {
        public memberwise init
      }

      fun français(_ x: Int, label y: Stash.A) -> Int {
        let (bar, ham) = (1, 2)
        return bar + ham
      }

      fun 複雑(サ: B<Int>, シ: Union<Int, Bool, B<Int>>) -> {ス: Int, セ: Bool} {}

      fun foo(y: Int) {
        let local = 0
        let lambda = fun[let capture = local](x: Int) { local + x }
        if true {
          let z = lambda(x: y)
        }

        extension B {
          fun hammer() {}
        }

        conformance Stash.A: Deinitializable {}
      }

      fun bar() {
        let f = fun (a: Int) -> Void {}
        let g = fun (a: Int) -> Void {}
      }
      """

    let input = SourceFile(synthesizedText: text)
    let (p, m) = try checkNoDiagnostic { (d) in
      var ast = try Utils.Host.hostedLibraryAST.get()
      let main = try ast.loadModule("Main", parsing: [input], reportingDiagnosticsTo: &d)
      let base = ScopedProgram(ast)
      return (try TypedProgram(annotating: base, reportingDiagnosticsTo: &d), main)
    }

    var o = SymbolCollector(forNodesIn: p)
    p.ast.walk(m, notifying: &o)

    var expected: Set = [
      "Main",
      "Main.Stash",
      "Main.Stash.A",
      "Main.Angle.init",
      "Main.Angle.degrees[].inout",
      "Main.français(_:label:).bar",
    ]

    for m in o.symbols.keys {
      let demangled = try XCTUnwrap(DemangledSymbol(m), "unable to demangle \"\(m)\"")
      expected.remove(demangled.description)
    }
    XCTAssert(expected.isEmpty, "symbols not found: \(list: expected)")
  }

  func testTypes() throws {
    let p = try checkNoDiagnostic { (d) in
      let ast = try Host.hostedLibraryAST.get()
      let base = ScopedProgram(ast)
      return try TypedProgram(annotating: base, reportingDiagnosticsTo: &d)
    }

    /// Asserts that demangling description of the mangled representation of `t` is `description`.
    func assertDemangledOfMangled<T: TypeProtocol>(
      _ t: T, is description: String, line: UInt = #line
    ) throws {
      let m = p.mangled(t)
      let d = try XCTUnwrap(DemangledSymbol(m), "unable to demangle \"\(m)\"", line: line)
      XCTAssertEqual(d.description, description, line: line)
    }

    try assertDemangledOfMangled(AnyType.any, is: "Any")
    try assertDemangledOfMangled(AnyType.never, is: "Never")
    try assertDemangledOfMangled(AnyType.void, is: "Void")
    try assertDemangledOfMangled(MetatypeType(of: AnyType.void), is: "Metatype<Void>")
    try assertDemangledOfMangled(p.ast.coreType("Int")!, is: "Hylo.Int")
    try assertDemangledOfMangled(
      ExistentialType(traits: [p.ast.core.movable.type], constraints: []),
      is: "any Hylo.Movable")
  }

  func testMonomorphized() {
    XCTAssertNotNil(DemangledSymbol(Self.inputCase))
  }

}

/// An AST visitation callback that collects mangled symbols, asserting that they are unique.
private struct SymbolCollector: ASTWalkObserver {

  /// The program containing the visited AST nodes.
  let program: TypedProgram

  /// The line at which this instance has been created.
  let failuresReportingLine: UInt

  /// A table mapping mangled symbols to their source.
  private(set) var symbols: [String: AnyDeclID] = [:]

  /// Creates an instance observing the nodes in `p` and reporting assertion failures as though
  /// they occurred at line `l` of this source file..
  init(forNodesIn p: TypedProgram, reportingFailuresAtLine l: UInt = #line) {
    self.program = p
    self.failuresReportingLine = l
  }

  mutating func willEnter(_ n: AnyNodeID, in ast: AST) -> Bool {
    // Binding declarations can't be mangled.
    if n.kind == BindingDecl.self {
      return true
    }

    if let d = AnyDeclID(n) {
      let s = program.mangled(d)
      let k = symbols.updateValue(d, forKey: s)
      XCTAssert(
        k == nil, "mangled representation of \(d.kind) collides with \(k!.kind)",
        line: failuresReportingLine)
    }

    return true
  }

}
