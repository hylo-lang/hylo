import ArgumentParser
import Core
import ValCommand
import XCTest

final class ExecutionTests: XCTestCase {

  /// Compiles and executes all tests in `TestCases` directory, and ensures they return success.
  func testExecution() throws {
    let s = Bundle.module.url(forResource: "TestCases", withExtension: nil)!
    for testFile in try! sourceFiles(in: [s]) {
      let output = try compile(testFile.url, with: ["--emit", "binary"])
      do {
        let exitCode = try run(output)
        XCTAssertEqual(
          exitCode, 0,
          "Execution of binary for test \(testFile.baseName) failed with exit code \(exitCode)")
      } catch {
        XCTFail("While testing \(testFile.baseName), cannot execute: \(output)")
      }
    }
  }

  /// Compiles `input` with the given arguments and returns the URL of the output file.
  ///
  /// Ensures that the compilation succeeds.
  func compile(_ input: URL, with arguments: [String]) throws -> URL {
    let output = FileManager.default.temporaryFile()
    let cli = try ValCommand.parse(arguments + ["-o", output.relativePath, input.relativePath])
    var stderr = ""
    let status = try cli.execute(loggingTo: &stderr)

    XCTAssert(status.isSuccess, "Compilation of \(input) failed with exit code \(status.rawValue)")
    XCTAssert(stderr.isEmpty, "Compilation of \(input) contains errors: \(stderr)")

    #if os(Windows)
      XCTAssert(
      FileManager.default.fileExists(atPath: output.relativePath + ".exe"),
      "Compilation output file not found: \(output.relativePath)")
    #else
    XCTAssert(
      FileManager.default.fileExists(atPath: output.relativePath),
      "Compilation output file not found: \(output.relativePath)")
    #endif

    return output
  }

  /// Run `executable` and return the exit code.
  ///
  /// Throws if the given file cannot be executed.
  public func run(_ executable: URL) throws -> Int32 {
    let task = Process()
    task.executableURL = executable
    try task.run()
    task.waitUntilExit()
    return task.terminationStatus
  }

}

extension FileManager {

  /// Returns the URL of a temporary file.
  func temporaryFile() -> URL {
    temporaryDirectory.appendingPathComponent("\(UUID())")
  }

}

extension String: Log {

  public var hasANSIColorSupport: Bool { false }

}
