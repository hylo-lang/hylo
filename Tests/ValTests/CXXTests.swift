import CodeGenCXX
import Core
import FrontEnd
import Utils
import XCTest

final class CXXTests: XCTestCase {

  func testTranspiler() throws {
    try checkAnnotatedValFiles(
      inSuiteAt: "TestCases/CXX", checkingAnnotationCommands: ["cpp", "h"],
      { (source, cxxAnnotations, diagnostics) in
        // Create a module for the input.
        var ast = AST.coreModule
        let module = try ast.makeModule(
          source.baseName, sourceCode: [source], diagnostics: &diagnostics)

        let typedProgram = try TypedProgram(ast, diagnostics: &diagnostics)

        // TODO: Run IR transform passes

        // Transpile the module.
        let transpiler = CXXTranspiler(typedProgram)
        var codeWriter = CXXCodeWriter()
        let cxxCode = codeWriter.cxxCode(transpiler.cxx(typedProgram[module]))

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

  func testCoreLibraryGeneration() throws {
    let typedProgram = try checkNoDiagnostic { d in
      try TypedProgram.init(AST.coreModule, diagnostics: &d)
    }

    let cxxCode = typedProgram.cxx(typedProgram[AST.coreModule.coreLibrary!]).text

    // Read test cases; use .val files just for convenience.
    try checkAnnotatedValFiles(
      inSuiteAt: "TestCases/CXXCoreLibrary",
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
