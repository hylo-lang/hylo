import Compiler
import Foundation

struct Terminal {

  /// Writes the given character string to the standard error.
  func error<S>(_ string: S) where S: StringProtocol {
    FileHandle.standardError.write(string.data(using: .utf8)!)
  }

}

extension Terminal: DiagConsumer {

  func consume(_ diagnostic: Diag) {
    // Print the location at which the diagnostic occured.
    if let loc = diagnostic.reportLocation {
      let filename = loc.url.path
      let (line, column) = loc.lineColumnIndices
      error("\(filename):\(line):\(column): ")
    }

    // Print the diagnostic.
    error("\(diagnostic.level): \(diagnostic.message)\n")

    // Print the highlighted ranges.
    for range in diagnostic.ranges {
      let line = range.lowerBound.line
      error(line)
      error("\n")

      let padding = line.distance(from: line.startIndex, to: range.lowerBound.index)
      let count = line.distance(
        from: range.lowerBound.index, to: min(range.upperBound.index, line.endIndex))

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
