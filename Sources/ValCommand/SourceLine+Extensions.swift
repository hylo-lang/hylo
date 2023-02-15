import ArgumentParser
import Core
import Foundation

extension SourceLine: ExpressibleByArgument {

  public init?(argument: String) {
    let x = argument.split(atLastIndexOf: ":")

    guard
      let file = try? SourceFile(contentsOf: URL(fileURLWithPath: String(x.head))),
      let line = Int(x.tail.dropFirst()).map(file.line(at:))
    else { return nil }
    self = line
  }

}
