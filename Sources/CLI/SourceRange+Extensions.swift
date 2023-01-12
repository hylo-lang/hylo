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

    let start = source.position(line: line, column: 1)

    // Create a range covering the given line.
    let endIndex = source.text
      .suffix(from: start.index)
      .firstIndex(where: { $0.isNewline })
    self = source.range(start.index ..< (endIndex ?? source.text.endIndex))
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
