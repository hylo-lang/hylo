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

    guard line > 0 && line <= source.line(containing: source.text.endIndex)
    else { return nil }

    let r = source.textOfLine(line).removingTrailingNewlines()
    self = source.range(r.startIndex ..< r.endIndex)
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
