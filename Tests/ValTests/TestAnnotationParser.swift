import Foundation
import Basic

struct TestAnnotationParser {

  /// The test annotations that have been parsed.
  var annotations: [TestAnnotation.Location: [TestAnnotation]] = [:]

  /// Scans the given source file for test annotations.
  mutating func scan(_ source: SourceFile) {
    let lines = source.split(separator: "\n", omittingEmptySubsequences: false)
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
      let loc = TestAnnotation.Location(url: source.url, line: i + offset)

      // Register the annotation.
      switch command {
      case "error":
        annotations[loc, default: []].append(.diagnostic(DiagnosticPattern(message: argument)))
      default:
        fatalError("unrecognized command: \(command)")
      }
    }
  }

}

/// A test annotation.
enum TestAnnotation {

  case diagnostic(DiagnosticPattern)

  /// The location of a test annotation.
  struct Location: Hashable {

    /// The URL of the source file from which the annotation comes.
    let url: URL

    /// The line to which the annotation refers in its source file.
    let line: Int

  }

}
