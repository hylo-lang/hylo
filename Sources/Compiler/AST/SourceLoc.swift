import Foundation

/// A location in a source file.
public struct SourceLoc {

  public let source: SourceFile

  public let index: String.Index

}

extension SourceLoc {

  /// The line containing the location.
  public var line: Substring {
    var lower = index
    while lower > source.contents.startIndex {
      let predecessor = source.contents.index(before: lower)
      if source.contents[predecessor].isNewline {
        break
      } else {
        lower = predecessor
      }
    }

    var upper = index
    while upper < source.contents.endIndex && !source.contents[upper].isNewline {
      upper = source.contents.index(after: upper)
    }

    return source.contents[lower ..< upper]
  }

  /// The 1-based line and column indices of the location.
  public var lineColumnIndices: (line: Int, column: Int) {
    if index == source.contents.endIndex {
      let lines = source.contents.split(whereSeparator: { $0.isNewline })
      return (line: lines.count, column: (lines.last?.count ?? 0) + 1)
    }

    var lineIndex = 1
    for c in source.contents[...index] where c.isNewline {
      lineIndex += 1
    }

    let buffer = source.contents.prefix(through: index)
    var columnIndex = 0
    for c in buffer.reversed() {
      guard !c.isNewline else { break }
      columnIndex += 1
    }

    return (lineIndex, columnIndex)
  }

}

extension SourceLoc: Equatable {}

extension SourceLoc: Hashable {}

extension SourceLoc: Comparable {

  public static func < (lhs: SourceLoc, rhs: SourceLoc) -> Bool {
    precondition(lhs.source == rhs.source, "locations are not in the same file")
    return lhs.index < rhs.index
  }

}

extension SourceLoc: CustomStringConvertible {

  public var description: String {
    let (line, column) = lineColumnIndices
    return "\(source.url.path):\(line):\(column)"
  }

}
