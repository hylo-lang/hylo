import ArgumentParser
import Foundation

@main
struct GenerateValFileTests: ParsableCommand {
  @Argument(
    help: "Paths of val source files to build",
    transform: URL.init(fileURLWithPath:)
  )
  var valSourceFiles: [URL] = []

  @Option(
    name: [.customShort("o")],
    help: ArgumentHelp("Write output to <file>.", valueName: "file"),
    transform: URL.init(fileURLWithPath:))
  var outputURL: URL

  @Option(
    name: [.customShort("n")],
    help: ArgumentHelp("Name of generated test case."))
  var testCaseName: String

  func run() throws {

    var output =
      """
      import Core
      import ValCommand
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
