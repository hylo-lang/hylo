import ArgumentParser
import Core
import Foundation

extension SourceRange: ExpressibleByArgument {

  public init?(argument: String) {
    // Parse the components of the range.
    var s = Substring(argument)
    guard
      let line = (s.takeSuffix(until: ":")?.dropFirst()).flatMap({ Int($0) }),
      let source = try? SourceFile(contentsOf: URL(fileURLWithPath: String(s)))
    else { return nil }

    // Make sure the line index is in range.
    guard line <= source.lineStarts.count else { return nil }

    // Create a range covering the given line.
    let startIndex = source.lineStarts[line - 1]
    let endIndex = source.text
      .suffix(from: startIndex)
      .firstIndex(where: { $0.isNewline })
    self = source.range(startIndex ..< (endIndex ?? source.text.endIndex))
  }

}

extension Substring {

  fileprivate mutating func takeSuffix(until ch: Character) -> Substring? {
    guard let i = lastIndex(of: ch) else { return nil }
    let result = suffix(from: i)
    self = prefix(upTo: i)
    return result
  }

}
