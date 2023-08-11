import ArgumentParser
import Core
import Driver
import TestUtils
import XCTest

final class ExecutionTests: XCTestCase {

  func testHelloWorld() throws {
    let f = FileManager.default.makeTemporaryFileURL()
    let s = #"public fun main() { print("Hello, World!") }"#
    try s.write(to: f, atomically: true, encoding: .utf8)

    let output = try compile(f, with: ["--emit", "binary", "-o", "hello"])
    let result = try run(output)
    XCTAssertEqual(result.status, 0, "Exit code is \(result.status)")
    XCTAssert(result.standardOutput.last?.isNewline ?? false)  // Allow for windows newline.
    XCTAssertEqual(result.standardOutput.dropLast(), "Hello, World!")
  }

}

extension XCTestCase {

  /// Compiles and runs the hylo file at `hyloFilePath`, `XCTAssert`ing that diagnostics and exit
  /// codes match annotated expectations.
  func compileAndRun(_ hyloFilePath: String, expectSuccess: Bool) throws {
    try checkAnnotatedHyloFileDiagnostics(inFileAt: hyloFilePath, expectSuccess: expectSuccess) {
      (hyloSource, diagnostics) in

      var executable: URL

      //Test debugged builds
      do {
        executable = try compile(hyloSource.url, with: ["--emit", "binary"])
      } catch let d as DiagnosticSet {
        // Recapture the diagnostics so the annotation testing framework can use them.  The need for
        // this ugliness makes me wonder how important it is to test cli.execute, which after all is
        // just a thin wrapper over cli.executeCommand (currently private).
        diagnostics = d
        throw d
      }

      var (status, _) = try run(executable)
      if status != 0 {
        throw NonzeroExitCode(value: status)
      }

      //Test optimized builds
      do {
        executable = try compile(hyloSource.url, with: ["--emit", "binary", "-O"])
      } catch let r as DiagnosticSet {
        // Recapture the diagnostics so the annotation testing framework can use them.  The need for
        // this ugliness makes me wonder how important it is to test cli.execute, which after all is
        // just a thin wrapper over cli.executeCommand (currently private).
        diagnostics = r
        throw r
      }

      (status, _) = try run(executable)
      if status != 0 {
        throw NonzeroExitCode(value: status)
      }

    }
  }

  /// Compiles `input` with the given arguments and returns the URL of the output file, throwing
  /// diagnostics if there are any errors.
  fileprivate func compile(_ input: URL, with arguments: [String]) throws -> URL {
    let output = FileManager.default.makeTemporaryFileURL()
    let cli = try Driver.parse(arguments + ["-o", output.relativePath, input.relativePath])
    let (status, diagnostics) = try cli.execute()
    if !status.isSuccess {
      throw diagnostics
    }

    XCTAssert(
      !diagnostics.containsError,
      "CLI reported success but \(input) contains errors: \(diagnostics.rendered())")

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

struct NonzeroExitCode: Error {
  var value: Int32
}
