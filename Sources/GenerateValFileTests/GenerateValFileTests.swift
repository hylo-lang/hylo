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

  func run() throws {

    var output =
      """
      import Core
      import ValCommand
      import XCTest

      final class ValFileTests: EndToEndTestCase {

      """

    for f in valSourceFiles {
      output += """

        func test_\(f.lastPathComponent.replacingOccurrences(of: ".", with: "_"))() throws {
          let output = try compile(URL(fileURLWithPath: \(String(reflecting: f.path))), with: ["--emit", "binary"])
          do {
            let (status, _) = try run(output)
            XCTAssertEqual(
              status, 0,
              "Execution of binary for test \(f.lastPathComponent) failed with exit code \\(status)")
          } catch {
            XCTFail("While testing \(f.lastPathComponent), cannot execute: \\(output)")
          }
        }

      """
    }

    output += """
      }
      """

    try output.write(to: outputURL, atomically: true, encoding: .utf8)
  }
}
