import Core
import FrontEnd
import IR
import Utils
import XCTest

final class EmitterTests: XCTestCase {

  func testEmitter() throws {
    try checkAnnotatedValFileDiagnostics(
      in: Bundle.module.url(forResource: "TestCases/Lowering", withExtension: nil)!,
      { (source, diagnostics) in
        // Note: built-in module is visible so that we can test built-in function calls.
        var ast = AST.coreModule
        let module = try ast.makeModule(
          source.baseName, sourceCode: [source],
          builtinModuleAccess: true, diagnostics: &diagnostics)

        // Run the type checker
        let typedProgram = try TypedProgram(ast, diagnostics: &diagnostics)

        // Emit Val's IR.
        var irModule = try Module(lowering: module, in: typedProgram, diagnostics: &diagnostics)

        // Run mandatory IR analysis and transformation passes.
        let p = PassPipeline(withMandatoryPassesForModulesLoweredFrom: typedProgram)
        try p.apply(&irModule, reportingDiagnosticsInto: &diagnostics)
      })
  }

}
