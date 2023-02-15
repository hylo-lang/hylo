import ArgumentParser
import Core
import Foundation

extension SourceLine: ExpressibleByArgument {

  public init?(argument: String) {
    var s = Substring(argument)
    guard
      let line = (s.takeSuffix(until: ":")?.dropFirst()).flatMap({ Int($0) }),
      let source = try? SourceFile(contentsOf: URL(fileURLWithPath: String(s)))
    else { return nil }

    guard line > 0 && line <= source.line(containing: source.text.endIndex).index
    else { return nil }

    self = source.line(at: line)
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
