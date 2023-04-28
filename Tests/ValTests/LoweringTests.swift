import Core
import FrontEnd
import IR
import Utils
import XCTest

final class LoweringTests: XCTestCase {

  func testLowering() throws {
    try checkAnnotatedValFileDiagnostics(inSuiteAt: "TestCases/Lowering") { (source, log) in
      var ir = try lower(source, log: &log)
      try ir.applyMandatoryPasses(reportingDiagnosticsInto: &log)
    }
  }

  func testEmitter() throws {
    let testCases = try testSuite(at: "TestCases/Lowering")
    for source in testCases {
      let expectedFile = source.url
        .deletingLastPathComponent()
        .appendingPathComponent(source.baseName + ".vir")
      guard let expected = try? ParsedIR(String(contentsOf: expectedFile)) else { continue }

      var log = DiagnosticSet()
      let ir = try lower(source, log: &log)
      let observed = ParsedIR(ir.description)
      XCTAssertEqual(expected, observed)
    }
  }

  /// Returns the Val IR of `source`, reporting diagnostics to `log`.
  private func lower(_ source: SourceFile, log: inout DiagnosticSet) throws -> IR.Module {
    // Note: built-in module is visible so that we can test built-in function calls.
    var ast = AST.coreModule
    let module = try ast.makeModule(
      source.baseName, sourceCode: [source],
      builtinModuleAccess: true, diagnostics: &log)

    // Run the type checker
    let typedProgram = try TypedProgram(ast, diagnostics: &log)

    // Emit Val's IR.
    return try Module(lowering: module, in: typedProgram, diagnostics: &log)
  }

}

/// The global and function definitions of a Val IR module.
private struct ParsedIR: Equatable {

  /// The description of the module.
  var module: Incidental<String>

  /// The globals of the module.
  var globals: Set<Substring> = []

  /// The functions of the module.
  var functions: Set<Substring> = []

  /// Creates an instance parsing `module`.
  init(_ module: String) {
    self.module = Incidental(module)

    var i = module.startIndex
    while i != module.endIndex {
      // Parse a global.
      if module[i...].starts(with: "global") {
        let j = module[i...].firstIndex(where: \.isNewline) ?? module.endIndex
        globals.insert(module[i ..< j])
        i = j
        continue
      }

      // Parse a function
      if module[i...].starts(with: "fun") {
        let functionStart = i

        i = module[i...].firstIndex(where: \.isNewline)!
        var openedBraces = 1
        while openedBraces > 0 {
          i = module.index(after: i)
          switch module[i] {
          case "{": openedBraces += 1
          case "}": openedBraces -= 1
          default:
            break
          }
        }

        functions.insert(module[functionStart ... i])
        i = module.index(after: i)
        continue
      }

      // Move to the next character.
      i = module.index(after: i)
    }
  }

}
