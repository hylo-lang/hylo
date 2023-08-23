import ArgumentParser
import Foundation

/// A command-line tool that generates replacements for file/line-reporting Swift standard library
/// functions, so that the full path to the file is always represented in the message.
struct GenerateFatalFilePathOverrides: ParsableCommand {

  @Option(
    name: [.customShort("o")],
    help: ArgumentHelp("Write output to <file>.", valueName: "output-swift-file"),
    transform: URL.init(fileURLWithPath:))
  var outputURL: URL

  func run() throws {
    try """
    /// Just like Swift.precondition, but includes the full file path.
    func precondition(
      _ condition: @autoclosure () -> Bool,
      _ message: @autoclosure () -> String = String(),
      file: StaticString = #filePath,
      line: UInt = #line
    ) {
      Swift.precondition(condition(), message(), file: (file), line: line)
    }

    /// Just like Swift.preconditionFailure, but includes the full file path.
    func preconditionFailure(
        _ message: @autoclosure () -> String = String(),
        file: StaticString = #filePath,
        line: UInt = #line
    ) -> Never {
      Swift.preconditionFailure(message(), file: (file), line: line)
    }

    /// Just like Swift.fatalError, but includes the full file path.
    func fatalError(
      _ message: @autoclosure () -> String = String(),
      file: StaticString = #filePath,
      line: UInt = #line
    ) -> Never {
      Swift.fatalError(message(), file: (file), line: line)
    }

    /// Just like Swift.fatalError, but includes the full file path.
    func fatal(
      _ message: @autoclosure () -> String = String(),
      file: StaticString = #filePath,
      line: UInt = #line
    ) -> Never {
      Swift.fatalError(message(), file: (file), line: line)
    }

    /// Just like Swift.assert, but includes the full file path.
    func assert(
        _ condition: @autoclosure () -> Bool,
        _ message: @autoclosure () -> String = String(),
        file: StaticString = #filePath,
        line: UInt = #line
    ) {
      Swift.assert(condition(), message(), file: (file), line: line)
    }

    func assertionFailure(
        _ message: @autoclosure () -> String = String(),
        file: StaticString = #filePath,
        line: UInt = #line
    ) {
      Swift.assertionFailure(message(), file: (file), line: line)
    }
    """.write(to: outputURL, atomically: true, encoding: .utf8)
  }

}
