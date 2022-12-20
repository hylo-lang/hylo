import ArgumentParser
import Core
import Foundation

extension SourceRange: ExpressibleByArgument {

  public init?(argument: String) {
    // Parse the components of the range.
    var s = Substring(argument)
    guard
      let line = (s.takeSuffix(until: ":")?.dropFirst()).flatMap({ Int($0) }),
      let source = try? SourceFile(contentsOf: URL(fileURLWithPath: String(s))),
      let start = source.location(at: line, 1)
    else { return nil }

    // Create a range covering the given line.
    let endIndex = source.contents
      .suffix(from: start.index)
      .firstIndex(where: { $0.isNewline })
    self.init(in: source, from: start.index, to: endIndex ?? source.contents.endIndex)
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
