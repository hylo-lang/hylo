import Foundation
import Basic

struct Terminal {

  let sourceManager: SourceManager

  /// Writes the given character string to the standard error.
  func error<S>(_ string: S) where S: StringProtocol {
    FileHandle.standardError.write(string.data(using: .utf8)!)
  }

}

extension Terminal: DiagnosticConsumer {

  func consume(_ diagnostic: Diagnostic) {
    // Print the location at which the diagnostic occured.
    if let location = diagnostic.reportLocation,
       let source = sourceManager.source(containing: location)
    {
      let filename = source.url.path
      let indices = source.lineColumnIndices(at: location)
      error("\(filename):\(indices.line):\(indices.column): ")
    }

    // Print the diagnostic.
    error("\(diagnostic.level): \(diagnostic.message)\n")

    // Print the highlighted ranges.
    for range in diagnostic.ranges {
      guard let source = sourceManager.source(containing: range.lowerBound) else { continue }
      let line = source.line(containing: range.lowerBound)
      error(line)
      error("\n")

      let padding = source.distance(from: line.startIndex, to: range.lowerBound)
      let count = source.distance(from: range.lowerBound, to: min(range.upperBound, line.endIndex))
      error(String(repeating: " ", count: padding))
      if count > 1 {
        error(String(repeating: "~", count: count))
      } else {
        error("^")
      }
      error("\n")
    }
  }

}
