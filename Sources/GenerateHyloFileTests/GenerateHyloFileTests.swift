import ArgumentParser
import Foundation
import Utils

/// A command-line tool that generates XCTest cases for a list of annotated ".hylo"
/// files as part of our build process.
@main
struct GenerateHyloFileTests: ParsableCommand {

  @Option(
    name: [.customShort("o")],
    help: ArgumentHelp("Write output to <file>.", valueName: "output-swift-file"),
    transform: URL.init(fileURLWithPath:))
  var outputURL: URL

  @Option(
    name: [.customShort("n")],
    help: ArgumentHelp("Name of generated test case.", valueName: "XCTestCase name"))
  var testCaseName: String

  @Argument(
    help: "Paths of annotated hylo source files to be tested.",
    transform: URL.init(fileURLWithPath:))
  var hyloSourceFiles: [URL]

  /// Returns the Swift source of the test function for the Hylo file at `source`.
  func swiftFunctionTesting(valAt source: URL) throws -> String {
    let firstLine = try String(contentsOf: source).prefix { !$0.isNewline }
    let parsed = try firstLine.parsedAsFirstLineOfAnnotatedHyloFileTest()
    let testID = source.deletingPathExtension().lastPathComponent.asSwiftIdentifier

    return parsed.reduce(into: "") { (o, p) in
      o += """

        func test_\(p.methodName)_\(testID)() throws {
          try \(p.methodName)(
            \(String(reflecting: source.fileSystemPath)),
            expectSuccess: \(p.expectSuccess))
        }

      """
    }
  }

  func run() throws {
    var output =
      """
      import XCTest
      import TestUtils

      final class \(testCaseName.asSwiftIdentifier): UnrecognizedSelectorWorkaroundTestCase {

      """

    for f in hyloSourceFiles {
      do {
        output += try swiftFunctionTesting(valAt: f)
      } catch let e as FirstLineError {
        try! FileHandle.standardError.write(
          contentsOf: Data("\(f.fileSystemPath):1: error: \(e.details)\n".utf8))
        GenerateHyloFileTests.exit(withError: ExitCode(-1))
      }
    }

    output += "\n}\n"

    try output.write(to: outputURL, atomically: true, encoding: .utf8)
  }

}

extension String {

  /// Returns a valid Swift identifier made by replacing non-identifier characters in `self` with
  /// underscores, and prefixing it with "X" if it happens to begin with a number.
  fileprivate var asSwiftIdentifier: String {
    let r = String(self.map { $0.isNumber || $0.isLetter ? $0 : "_" })
    return (r.first?.isNumber ?? true) ? "X" + r : r
  }

}

extension StringProtocol where Self.SubSequence == Substring {

  /// Interpreting `self` as the first line of an annotated Hylo test file, returns the embedded
  /// test method name and expectation of success.
  fileprivate func parsedAsFirstLineOfAnnotatedHyloFileTest() throws -> [TestDescription] {
    var text = self[...]
    if !text.removeLeading("//- ") {
      throw FirstLineError("first line of annotated test file must begin with “//-”.")
    }

    let methodName = text.removeFirstUntil(it: \.isWhitespace)
    if methodName.isEmpty {
      throw FirstLineError("missing test method name.")
    }
    text.removeFirstWhile(it: \.isWhitespace)

    if !text.removeLeading("expecting:") {
      throw FirstLineError("missing “expecting:” after test method name.")
    }
    text.removeFirstWhile(it: \.isWhitespace)

    let expectation = text.removeFirstUntil(it: \.isWhitespace)
    if expectation != "success" && expectation != "failure" {
      throw FirstLineError(
        "illegal expectation “\(expectation)” must be “success” or “failure”."
      )
    }
    let expectSuccess = expectation == "success"

    if !text.drop(while: \.isWhitespace).isEmpty {
      throw FirstLineError("illegal trailing text “\(text)”.")
    }

    var tests = [TestDescription(methodName: methodName, expectSuccess: expectSuccess)]
    if methodName == "compileAndRun" {
      tests.append(
        .init(methodName: "compileAndRunWithOptimizations", expectSuccess: expectSuccess))
    }
    return tests
  }

}

/// Information necessary to generate a test case.
fileprivate struct TestDescription {

  /// The name of the method implementing the logic of the test runner.
  let methodName: Substring

  /// `true` iff the invoked compilation is expected to succeed.
  let expectSuccess: Bool

}

/// A failure to parse the first line of an annotated Hylo file.
struct FirstLineError: Error {
  /// Creates an instance whose detailed description is `details`.
  init(_ details: String) { self.details = details }

  /// A detailed description of the cause of this failure.
  let details: String
}
