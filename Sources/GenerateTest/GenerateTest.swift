import ArgumentParser
import Foundation

@main
struct GenerateTest: ParsableCommand {
  @Argument(help: "The name of the generated Swift XCTestCase")
  var testCaseName: String

  @Option(
    name: [.customShort("o")],
    help: ArgumentHelp("Write output to <file>.", valueName: "file"),
    transform: URL.init(fileURLWithPath:))
  var outputURL: URL

  func run() throws {

    print("** GenerateTest \(testCaseName) -o \(outputURL)")

    try """
    import XCTest

    final class \(testCaseName): XCTestCase {
      func test0() {
        XCTAssert(true)
      }
    }
    """.write(to: outputURL, atomically: true, encoding: .utf8)
  }
}
