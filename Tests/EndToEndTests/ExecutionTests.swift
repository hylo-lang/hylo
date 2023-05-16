import ArgumentParser
import Core
import TestUtils
import ValCommand
import XCTest

final class ExecutionTests: EndToEndTestCase {

  func testHelloWorld() throws {
    let f = FileManager.default.makeTemporaryFileURL()
    let s = #"public fun main() { print("Hello, World!") }"#
    try s.write(to: f, atomically: true, encoding: .utf8)

    let output = try compile(f, with: ["--emit", "binary", "-o", "hello"])
    let result = try run(output)
    XCTAssertEqual(result.status, 0, "Exit code is \(result.status)")
    XCTAssertEqual(result.standardOutput, "Hello, World!\n")
  }

}

class EndToEndTestCase: XCTestCase {

  /// Compiles and runs the given file at `valFilePath`, `XCTAssert`ing that diagnostics and exit
  /// codes match annotated expectations.
  func compileAndRun(_ valFilePath: String)
    throws
  {
    // Using an IUO here because threading it through all the layers used by compile is just too
    // awful.
    var executable: URL!

    try checkAnnotatedValFileDiagnostics(inFileAt: valFilePath) {
      (_ valSource: SourceFile, _ diagnostics: inout DiagnosticSet) in

      do {
        executable = try compile(valSource.url, with: ["--emit", "binary"])
      } catch let d as DiagnosticSet {
        // Recapture the diagnostics so the annotation testing framework can use them.  The need for
        // this ugliness makes me wonder how important it is to test cli.execute, which after all is
        // just a thin wrapper over cli.executeCommand (currently private).
        diagnostics = d
        throw d
      }
    }

    let (status, _) = try run(executable)
    XCTAssertEqual(status, 0, "\n\(valFilePath):1:1 execution failed with exit code \(status)")
  }

  /// Compiles `input` with the given arguments and returns the URL of the output file.
  ///
  /// Ensures that the compilation succeeds.
  fileprivate func compile(_ input: URL, with arguments: [String]) throws -> URL {
    let output = FileManager.default.makeTemporaryFileURL()
    let cli = try ValCommand.parse(arguments + ["-o", output.relativePath, input.relativePath])
    let (status, diagnostics) = try cli.execute()
    if !status.isSuccess {
      throw diagnostics
    }

    XCTAssert(
      !diagnostics.containsError,
      "Compilation of \(input) contains diagnostics: \(diagnostics.rendered())")

    #if os(Windows)
      let executableSuffix = ".exe"
    #else
      let executableSuffix = ""
    #endif

    XCTAssert(
      FileManager.default.fileExists(atPath: output.relativePath + executableSuffix),
      "Compilation output file not found: \(output.relativePath)")

    return output
  }

  /// Runs `executable` and returns its exit status along with the text written to its standard
  /// output.
  fileprivate func run(_ executable: URL) throws -> (status: Int32, standardOutput: String) {
    let pipe = Pipe()
    let task = Process()
    task.executableURL = executable
    task.standardOutput = pipe
    try task.run()
    task.waitUntilExit()

    let standardOutput = String(
      data: pipe.fileHandleForReading.readDataToEndOfFile(), encoding: .utf8)
    return (task.terminationStatus, standardOutput ?? "")
  }

}
