import ArgumentParser
import Foundation

@main
struct GenerateValFileTests: AsyncParsableCommand {
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

  func firstLineOfEachInput() async throws -> [URL: String] {
    try await withThrowingTaskGroup(of: (URL, String).self) { (g) -> [URL: String] in
      for f in valSourceFiles {
        g.addTask {
          for try await firstLine in f.lines {
            return (f, firstLine)
          }
          return (f, "")
        }
      }
      var r = [URL: String]()
      for try await (k, v) in g { r[k] = v }
      return r
    }
  }

  /// Returns the Swift source of the test function for the Val at `source`, having `firstLine` as
  /// its first line.
  func swiftFunctionTesting(valAt source: URL, withFirstLine firstLine: String) -> String {

    func die(_ message: String) -> Never {
      try! FileHandle.standardError.write(
        contentsOf: Data("\(source.path):1: error: \(message)\n".utf8))
      GenerateValFileTests.exit(withError: ExitCode(-1))
    }

    var text = firstLine[...]
    if !text.removeLeading("//- ") {
      die("first line of annotated test file must begin with “//-”.")
    }
    let methodName = text.removeUntil { $0.isWhitespace }
    if methodName.isEmpty {
      die("missing test method name.")
    }
    text = text.drop(while: \.isWhitespace)

    if !text.removeLeading("expecting:") {
      die("missing “expecting:” after test method name.")
    }
    text = text.drop(while: \.isWhitespace)

    let expectation = text.removeUntil { $0.isWhitespace }
    if expectation != "success" && expectation != "failure" {
      die(
        "illegal expectation “\(expectation)” must be “success” or “failure”."
      )
    }
    if !text.drop(while: \.isWhitespace).isEmpty {
      die("illegal trailing text “\(text)”.")
    }

    return """

      func test_\(source.lastPathComponent.asSwiftIdentifier)() throws {
        try \(methodName)(\(String(reflecting: source.path)), expectSuccess: \(expectation == "success"))
      }

      """
  }

  func run() async throws {

    let firstLines = try await firstLineOfEachInput()
    _ = firstLines

    var output =
      """
      import XCTest

      final class \(testCaseName.asSwiftIdentifier): EndToEndTestCase {

      """

    for f in valSourceFiles {
      output += swiftFunctionTesting(valAt: f, withFirstLine: firstLines[f]!)
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
    var me = self
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
