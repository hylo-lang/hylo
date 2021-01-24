import Foundation

/// A handle to a source file, owned by a source manager.
///
/// A source manager is essentially a large buffer of concatenated source files. This type provide
/// a thin abstraction over this buffer to manipulate to interact with a single source file.
public struct SourceFile {

  /// The manager owning the contents of the source file.
  public unowned let manager: SourceManager

  /// The source's URL.
  public let url: URL

  public let startIndex: String.Index

  public let endIndex: String.Index

  /// The line containing the given location.
  ///
  /// - Parameter location: A location within this source file.
  public func line(containing location: String.Index) -> Substring {
    precondition((startIndex ..< endIndex) ~= location)

    let contents = manager.contents(of: self)
    var lower = location
    while lower >= contents.startIndex && !contents[lower].isNewline {
      lower = contents.index(before: lower)
    }

    var upper = location
    while upper < contents.endIndex && !contents[upper].isNewline {
      upper = contents.index(after: upper)
    }

    return contents[contents.index(after: lower) ..< upper]
  }

  /// The 1-based line and column indices of the given location.
  ///
  /// - Parameter location: A location within this source file.
  public func lineColumnIndices(at location: String.Index) -> (line: Int, column: Int) {
    precondition((startIndex ..< endIndex) ~= location)

    var contents = manager.contents(of: self)
    var lineIndex = 1
    for c in contents[...location] where c.isNewline {
      lineIndex += 1
    }

    contents = manager.contents(of: self).prefix(through: location)
    var columnIndex = 0
    for c in contents.reversed() {
      guard !c.isNewline else { break }
      columnIndex += 1
    }

    return (lineIndex, columnIndex)
  }

  /// The 1-based line index of the given location.
  ///
  /// - Parameter location: A location within this source file.
  @available(*, deprecated, renamed: "lineColumnIndices(at:)")
  public func lineIndex(at location: String.Index) -> Int {
    precondition((startIndex ..< endIndex) ~= location)

    let contents = manager.contents(of: self)
    var n = 1
    for c in contents[...location] where c.isNewline {
      n += 1
    }
    return n
  }

  /// The 1-based column index of the given location.
  ///
  /// - Parameter location: A location within this source file.
  @available(*, deprecated, renamed: "lineColumnIndices(at:)")
  public func columnIndex(at location: String.Index) -> Int {
    precondition((startIndex ..< endIndex) ~= location)

    let contents = manager.contents(of: self).prefix(through: location)
    var n = 0
    for c in contents.reversed() {
      guard !c.isNewline else { break }
      n += 1
    }
    return n
  }

}

extension SourceFile: Equatable {

  public static func == (lhs: SourceFile, rhs: SourceFile) -> Bool {
    return (lhs.manager === rhs.manager) && (lhs.startIndex == rhs.startIndex)
  }

}

extension SourceFile: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(manager))
    hasher.combine(startIndex)
  }

}

extension SourceFile: Collection {

  public func index(after i: String.Index) -> String.Index {
    return manager.contents(of: self).index(after: i)
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
