import Foundation

/// A handle to a source file, owned by a source manager.
public struct SourceFile {

  /// The manager owning the contents of the source file.
  public unowned let manager: SourceManager

  /// The source's URL.
  public let url: URL

  /// The index of the first character in the source file.
  public let startIndex: String.Index

  /// One part the index of the last character in the source file.
  public let endIndex: String.Index

  /// The line containing the given location.
  ///
  /// - Parameter location: A location within this source file.
  public func line(containing location: String.Index) -> Substring {
    precondition((startIndex ... endIndex) ~= location)

    let contents = manager.contents(of: self)
    var lower = location
    while lower > contents.startIndex {
      let predecessor = contents.index(before: lower)
      if contents[predecessor].isNewline {
        break
      } else {
        lower = predecessor
      }
    }

    var upper = location
    while upper < contents.endIndex && !contents[upper].isNewline {
      upper = contents.index(after: upper)
    }

    return contents[lower ..< upper]
  }

  /// The 1-based line and column indices of the given location.
  ///
  /// - Parameter location: A location within this source file.
  public func lineColumnIndices(at location: String.Index) -> (line: Int, column: Int) {
    precondition((startIndex ... endIndex) ~= location)
    var contents = manager.contents(of: self)

    if location == endIndex {
      let lines = contents.split(whereSeparator: { $0.isNewline })
      return (line: lines.count, column: (lines.last?.count ?? 0) + 1)
    }

    var lineIndex = 1
    for c in contents[...location] where c.isNewline {
      lineIndex += 1
    }

    contents = contents.prefix(through: location)
    var columnIndex = 0
    for c in contents.reversed() {
      guard !c.isNewline else { break }
      columnIndex += 1
    }

    return (lineIndex, columnIndex)
  }

}

extension SourceFile: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(manager))
    hasher.combine(startIndex)
  }

  public static func == (lhs: SourceFile, rhs: SourceFile) -> Bool {
    return (lhs.manager === rhs.manager) && (lhs.startIndex == rhs.startIndex)
  }

}

extension SourceFile: BidirectionalCollection {

  public func index(after i: String.Index) -> String.Index {
    return manager.contents(of: self).index(after: i)
  }

  public func index(before i: String.Index) -> String.Index {
    return manager.contents(of: self).index(before: i)
  }

  public subscript(position: String.Index) -> Character {
    return manager.contents(of: self)[position]
  }

  public subscript(bounds: Range<String.Index>) -> Substring {
    return manager.contents(of: self)[bounds]
  }

  public subscript<R>(range: R) -> Substring where R: RangeExpression, R.Bound == String.Index {
    return manager.contents(of: self)[range]
  }

}
