import ArgumentParser
import Foundation

/// A command-line tool that generates XCTest cases for a list of annotated .val
/// files as part of our build process.
@main
struct GenerateValFileTests: ParsableCommand {

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
    help: "Paths of annotated val source files to be tested.",
    transform: URL.init(fileURLWithPath:)
  )
  var valSourceFiles: [URL]

  /// Returns the Swift source of the test function for the Val at `source`, having `firstLine` as
  /// its first line.
  func swiftFunctionTesting(valAt source: URL, withFirstLine firstLine: Substring) throws -> String
  {
    let parsed = try firstLine.parsedAsFirstLineOfAnnotatedValFileTest()
    let testID = source.deletingPathExtension().lastPathComponent.asSwiftIdentifier

    return """

        func test_\(parsed.methodName)_\(testID)() throws {
          try \(parsed.methodName)(
            \(String(reflecting: source.path)), expectSuccess: \(parsed.expectSuccess))
        }

      """
  }

  func run() throws {
    var output =
      """
      import XCTest

      final class \(testCaseName.asSwiftIdentifier): XCTestCase {

      """

    for f in valSourceFiles {
      let firstLine = try String(contentsOf: f).split(separator: "\n", maxSplits: 1).first ?? ""
      do {
        output += try swiftFunctionTesting(valAt: f, withFirstLine: firstLine)
      } catch let e as FirstLineError {
        try! FileHandle.standardError.write(
          contentsOf: Data("\(f.path):1: error: \(e.details)\n".utf8))
        GenerateValFileTests.exit(withError: ExitCode(-1))
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
    let r = String(self.lazy.map { $0.isNumber || $0.isLetter ? $0 : "_" })
    return (r.first?.isNumber ?? true) ? "X" + r : r
  }

}

extension Substring {

  /// Removes `prefix` from the beginning and returns `true`, or returns `false` if `self`
  /// has no such prefix.
  fileprivate mutating func removeLeading(_ prefix: String) -> Bool {
    var me = self[...]
    var p = prefix[...]

    while let x = me.first, let y = p.first {
      if x != y { return false }
      me = me.dropFirst()
      p = p.dropFirst()
    }

    if !p.isEmpty { return false }
    self = me
    return true
  }

  /// Removes characters from the beginning until the first character satisfies `p` or `self` is
  /// empty, returning the removed substring.
  fileprivate mutating func removeUntil(firstSatisfies p: (Character) -> Bool) -> Substring {
    let firstToKeep = firstIndex(where: p) ?? endIndex
    defer { self = self[firstToKeep...] }
    return self[..<firstToKeep]
  }

}

extension StringProtocol where Self.SubSequence == Substring {

  /// Interpreting `self` as the first line of an annotated Val test file, returns the embedded test
  /// method name and expectation of success.
  func parsedAsFirstLineOfAnnotatedValFileTest() throws -> (
    methodName: Substring, expectSuccess: Bool
  ) {
    var text = self[...]
    if !text.removeLeading("//- ") {
      throw FirstLineError("first line of annotated test file must begin with “//-”.")
    }
    let methodName = text.removeUntil { $0.isWhitespace }
    if methodName.isEmpty {
      throw FirstLineError("missing test method name.")
    }
    text = text.drop(while: \.isWhitespace)

    if !text.removeLeading("expecting:") {
      throw FirstLineError("missing “expecting:” after test method name.")
    }
    text = text.drop(while: \.isWhitespace)

    let expectation = text.removeUntil { $0.isWhitespace }
    if expectation != "success" && expectation != "failure" {
      throw FirstLineError(
        "illegal expectation “\(expectation)” must be “success” or “failure”."
      )
    }
    if !text.drop(while: \.isWhitespace).isEmpty {
      throw FirstLineError("illegal trailing text “\(text)”.")
    }
    return (methodName: methodName, expectSuccess: expectation == "success")
  }

}

/// A failure to parse the first line of an annotated Val file.
struct FirstLineError: Error {
  /// Creates an instance whose detailed description is `details`.
  init(_ details: String) { self.details = details }

  /// A detailed description of the cause of this failure.
  let details: String
}
