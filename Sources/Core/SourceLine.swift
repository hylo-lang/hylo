import Foundation

/// A line if a source file.
public struct SourceLine: Hashable {

  /// The source file containing the position.
  public let file: SourceFile

  /// The 1-based index of the line in `file`.
  public let index: Int

  /// Creates an instance representing the line at `index` in `url`.
  init(_ index: Int, in file: SourceFile) {
    self.file = file
    self.index = index
  }

}

extension SourceLine: CustomStringConvertible {

  public var description: String { "\(file.url.relativePath):\(index)" }

}
