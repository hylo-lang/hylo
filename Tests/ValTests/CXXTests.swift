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
        let codeWriter = CXXCodeWriter()
        let cxxModule = transpiler.emit(module: typedProgram[module])

        let cxxHeaderCode = codeWriter.emitHeaderCode(cxxModule)
        let cxxSourceCode = codeWriter.emitSourceCode(cxxModule)

        return cxxAnnotations.compactMap { a in
          let expectedCXX = a.argument!.removingTrailingNewlines()
          let cxxSourceToSearch = a.command == "cpp" ? cxxSourceCode : cxxHeaderCode

          return cxxSourceToSearch.essentialize().contains(expectedCXX.essentialize())
            ? nil
            : a.failure(
              """
              transpiled code not found:
              \(expectedCXX)
              --- not found in (after removing whitespaces) ---
              \(cxxSourceToSearch)
              """)
        }
      })
  }
}
