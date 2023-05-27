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

  func run() async throws {

    let firstLines = try await firstLineOfEachInput()
    _ = firstLines

    var output =
      """
      import XCTest

      final class \(testCaseName.asSwiftIdentifier): EndToEndTestCase {

      """

    for f in valSourceFiles {
      output += """

        func test_\(f.lastPathComponent.asSwiftIdentifier)() throws {
          try compileAndRun(\(String(reflecting: f.path)))
        }

        """
    }

    output += """
      }
      """

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
