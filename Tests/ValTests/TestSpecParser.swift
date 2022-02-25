import Compiler
import Foundation

struct TestSpecParser {

  /// The test annotations that have been parsed.
  var annotations: [TestSpec.Loc: [TestSpec]] = [:]

  /// Scans the given source file for test annotations.
  mutating func scan(contentsOf url: URL) throws {
    let contents = try String(contentsOf: url)
    let lines = contents.split(separator: "\n", omittingEmptySubsequences: false)

    for (i, line) in lines.enumerated() {
      // Check if the line contains a test annotation.
      guard let range = line.range(of: "#!") else { continue }
      var start = line.index(range.lowerBound, offsetBy: 2)

      // Parse the type of the annotation.
      let command = line[start...].prefix(while: { !$0.isWhitespace && ($0 != "@") })
      start = line.index(start, offsetBy: command.count)

      // Parse the line offset, if any.
      var offset: Int = 1
      if line[start...].starts(with: "@") {
        let end = line[start...].firstIndex(where: { $0.isWhitespace }) ?? line.endIndex
        offset += Int(line[line.index(after: start) ..< end]) ?? 0
        start = end
      }

      // Parse the argument, if any.
      let suffix = line[start...].drop(while: { $0.isWhitespace })
      let argument = suffix.isEmpty
        ? nil
        : String(suffix)

      // Determine the location of the annotation.
      let specLoc = TestSpec.Loc(url: url, line: i + offset)

      // Register the annotation.
      let pattern: DiagPattern
      switch command {
      case "error":
        pattern = DiagPattern(level: .error, message: argument)
      case "warning":
        pattern = DiagPattern(level: .warning, message: argument)
      default:
        fatalError("unrecognized command: \(command)")
      }
      annotations[specLoc, default: []].append(.diagnostic(pattern))
    }
  }

}

/// A test annotation.
enum TestSpec {

  /// The location of a test annotation.
  struct Loc: Hashable {

    /// The URL of the source file from which the annotation comes.
    let url: URL

    /// The line to which the annotation refers in its source file.
    let line: Int

  }

  case diagnostic(DiagPattern)

}
