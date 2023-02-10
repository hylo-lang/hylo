import CodeGenCXX
import Core
import FrontEnd
import Utils
import XCTest

final class CXXTests: XCTestCase {

  func testTranspiler() throws {

    try checkAnnotatedValFiles(
      in: "TestCases/CXX",
      checkingAnnotationCommands: ["cpp", "h"],

      { (source, cxxAnnotations, diagnostics) in
        // Create a module for the input.
        var ast = AST.coreModule
        let module = ast.insert(synthesized: ModuleDecl(source.baseName))

        // Parse the input.
        _ = try Parser.parse(source, into: module, in: &ast, diagnostics: &diagnostics)

        let typedProgram = try TypedProgram(ast, diagnostics: &diagnostics)

        // TODO: Run IR transform passes

        // Transpile the module.
        let transpiler = CXXTranspiler(typedProgram)
        var codeWriter = CXXCodeWriter()
        let cxxCode = codeWriter.cxxCode(transpiler.transpile(typedProgram[module]))

        return cxxAnnotations.compactMap { a in
          let expectedCXX = a.argument!.removingTrailingNewlines()
          let cxxSourceToSearch = a.command == "cpp" ? cxxCode.sourceCode : cxxCode.headerCode

          return cxxSourceToSearch.canonicalize().contains(expectedCXX.canonicalize())
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

  func testStdLibGeneration() throws {
    let ast = AST.coreModule

    // Run the type checker.
    var checker = TypeChecker(program: ScopedProgram(ast), isBuiltinModuleVisible: true)
    checker.check(module: ast.corelib!)

    let typedProgram = TypedProgram(
      annotating: checker.program,
      declTypes: checker.declTypes,
      exprTypes: checker.exprTypes,
      implicitCaptures: checker.implicitCaptures,
      referredDecls: checker.referredDecls,
      foldedSequenceExprs: checker.foldedSequenceExprs)

    // Transpile the standard lib module.
    let transpiler = CXXTranspiler(typedProgram)
    var codeWriter = CXXCodeWriter()
    let cxxCode = codeWriter.cxxCode(transpiler.transpile(stdlib: typedProgram[ast.corelib!]))

    // Read test cases; use .val files just for convenience.
    try checkAnnotatedValFiles(
      in: "TestCases/CXXStdLib",
      checkingAnnotationCommands: ["cpp", "h"],
      { (source, cxxAnnotations, diagnostics) in
        return cxxAnnotations.compactMap { a in
          let expectedCXX = a.argument!.removingTrailingNewlines()
          let cxxSourceToSearch = a.command == "cpp" ? cxxCode.sourceCode : cxxCode.headerCode

          return cxxSourceToSearch.canonicalize().contains(expectedCXX.canonicalize())
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
