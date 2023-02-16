import ArgumentParser
import TestSupport
import ValCommand
import XCTest

final class ExecutionTests: XCTestCase {

  func testExecution() throws {
    try checkAnnotatedValFiles(
      in: Bundle.module.url(forResource: "TestCases/Execution", withExtension: nil)!,
      checkingAnnotationCommands: ["run"],
      { (source, cxxAnnotations, diagnostics) in
        // Compile the test file; expect success.
        let result = try compile(source.url, with: ["--emit", "binary"])
        result.assertSuccess()

        return cxxAnnotations.compactMap { a in
          // Run the executable, check for success.
          return a.run(result.output)
        }
      })
  }

}
