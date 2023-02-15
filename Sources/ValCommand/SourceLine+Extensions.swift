import ArgumentParser
import Core
import Foundation

extension SourceLine: ExpressibleByArgument {

  public init?(argument: String) {
    let x = argument.split(atLastIndexOf: ":")

    guard
      let file = try? SourceFile(contentsOf: URL(fileURLWithPath: String(x.head))),
      let lineNumber = Int(x.tail.dropFirst()),
      lineNumber > 0 && lineNumber <= file.lineCount
    else { return nil }
    self = file.line(lineNumber)
  }

}
