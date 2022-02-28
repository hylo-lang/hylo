import Foundation

/// A location in a source file.
public struct SourceLoc {

  public let url: URL

  public let index: String.Index

}

extension SourceLoc {

  /// The line containing the location.
  public var line: Substring {
    let contents = try! String(contentsOf: url)
    var lower = index
    while lower > contents.startIndex {
      let predecessor = contents.index(before: lower)
      if contents[predecessor].isNewline {
        break
      } else {
        lower = predecessor
      }
    }

    var upper = index
    while upper < contents.endIndex && !contents[upper].isNewline {
      upper = contents.index(after: upper)
    }

    return contents[lower ..< upper]
  }

  /// The 1-based line and column indices of the location.
  public var lineColumnIndices: (line: Int, column: Int) {
    let contents = try! String(contentsOf: url)
    if index == contents.endIndex {
      let lines = contents.split(whereSeparator: { $0.isNewline })
      return (line: lines.count, column: (lines.last?.count ?? 0) + 1)
    }

    var lineIndex = 1
    for c in contents[...index] where c.isNewline {
      lineIndex += 1
    }

    let buffer = contents.prefix(through: index)
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
    precondition(lhs.url == rhs.url, "locations are not in the same file")
    return lhs.index < rhs.index
  }

}

extension SourceLoc: CustomStringConvertible {

  public var description: String {
    let (line, column) = lineColumnIndices
    return "\(url.path):\(line):\(column)"
  }

}
