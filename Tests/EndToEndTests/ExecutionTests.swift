import ArgumentParser
import Core
import ValCommand
import XCTest

final class ExecutionTests: XCTestCase {

  /// Compiles and executes all tests in `TestCases` directory, and ensures they return success.
  #if os(Windows)
    func testExecution() throws {
      try testValOnWindows("./TestCases/Factorial.val")
      try testValOnWindows("./TestCases/Destroy.val")
      try testValOnWindows("./TestCases/Lambda.val")
    }
    /// Compiles and executes tests, and ensures they return success on Windows.
    func testValOnWindows(_ fileURL: String) throws {
      let s = Bundle.module.url(forResource: fileURL, withExtension: nil)!
      for testFile in try sourceFiles(in: [s]) {
        let output = try compile(testFile.url, with: ["--emit", "binary"])
        do {
          let (status, _) = try run(output)
          XCTAssertEqual(
            status, 0,
            "Execution of binary for test \(testFile.baseName) failed with exit code \(status)")
        } catch {
          XCTFail("While testing \(testFile.baseName), cannot execute: \(output)")
        }
      }
    }
  #else
    func testExecution() throws {
      let s = Bundle.module.url(forResource: "TestCases", withExtension: nil)!
      for testFile in try! sourceFiles(in: [s]) {
        let output = try compile(testFile.url, with: ["--emit", "binary"])
        do {
          let (status, _) = try run(output)
          XCTAssertEqual(
            status, 0,
            "Execution of binary for test \(testFile.baseName) failed with exit code \(status)")
        } catch {
          XCTFail("While testing \(testFile.baseName), cannot execute: \(output)")
        }
      }
    }
  #endif
  func testHelloWorld() throws {
    let f = FileManager.default.temporaryFile()
    let s = #"public fun main() { print("Hello, World!") }"#
    try s.write(to: f, atomically: true, encoding: .utf8)

    let output = try compile(f, with: ["--emit", "binary", "-o", "hello"])
    let result = try run(output)
    XCTAssertEqual(result.status, 0, "Exit code is \(result.status)")
    #if os(Windows)
      XCTAssertEqual(result.standardOutput, "Hello, World!\r\n")
    #else
      XCTAssertEqual(result.standardOutput, "Hello, World!\n")
    #endif
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

  /// Runs `executable` and returns its exit status along with the text written to its standard
  /// output.
  public func run(_ executable: URL) throws -> (status: Int32, standardOutput: String) {
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

extension FileManager {

  /// Returns the URL of a temporary file.
  func temporaryFile() -> URL {
    temporaryDirectory.appendingPathComponent("\(UUID())")
  }

}

extension String: Log {

  public var hasANSIColorSupport: Bool { false }

}
