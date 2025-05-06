import Foundation

/// A line of a source file.
public struct SourceLine: Hashable, Sendable {

  /// The source file containing the position.
  public let file: SourceFile

  /// The 1-based index of the line in `file`.
  public let number: Int

  /// Creates an instance representing the line at `index` in `url`.
  init(_ index: Int, in file: SourceFile) {
    self.file = file
    self.number = index
  }

  /// The source text contained in this line.
  public var text: Substring { file[bounds] }

  /// The bounds of this line, including any trailing newline.
  public var bounds: SourceRange { file.bounds(of: self) }

}

extension SourceLine: CustomStringConvertible {

  public var description: String { "\(file.url.relativePath):\(number)" }

}
