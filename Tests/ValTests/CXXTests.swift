import CodeGenCXX
import Core
import FrontEnd
import Utils
import XCTest

final class CXXTests: XCTestCase {

  func testTranspiler() throws {
    // Prepare an AST with the core module loaded.
    var baseAST = AST()
    baseAST.importCoreModule()

    try checkAnnotatedValFiles(
      in: "TestCases/CXX",
      checkingAnnotationCommands: ["cpp", "h"],

      { (source, cxxAnnotations, diagnostics) in
        // Create a module for the input.
        var ast = baseAST
        let module = ast.insert(synthesized: ModuleDecl(name: source.baseName))

        // Parse the input.
        _ = try Parser.parse(source, into: module, in: &ast, diagnostics: &diagnostics)

        // Run the type checker.
        var checker = TypeChecker(program: ScopedProgram(ast))
        _ = checker.check(module: module)
        diagnostics.report(checker.diagnostics)
        try diagnostics.throwOnError()

        let typedProgram = TypedProgram(
          annotating: checker.program,
          declTypes: checker.declTypes,
          exprTypes: checker.exprTypes,
          implicitCaptures: checker.implicitCaptures,
          referredDecls: checker.referredDecls,
          foldedSequenceExprs: checker.foldedSequenceExprs)

        // TODO: Run IR transform passes

        // Transpile the module.
        var transpiler = CXXTranspiler(program: typedProgram)
        let cxxModule = transpiler.emit(module: typedProgram[module])

        let cxxHeader = cxxModule.emitHeader()
        let cxxSource = cxxModule.emitSource()

        return cxxAnnotations.compactMap { a in
          let expectedCXX = a.argument!.removingTrailingNewlines()
          let cxxSourceToSearch = a.command == "cpp" ? cxxSource : cxxHeader

          return cxxSourceToSearch.contains(expectedCXX)
            ? nil
            : a.failure(
              """
              transpiled code not found:
              \(expectedCXX)
              --- not found in ---
              \(cxxSourceToSearch)
              """)
        }
      })
  }
}
