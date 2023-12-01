import ArgumentParser
import Core
import Foundation

extension SourceLine {

  public init?(argument: String) {
    let x = argument.split(atLastIndexOf: ":")

    guard
      let file = try? SourceFile(path: x.head),
      let lineNumber = Int(x.tail.dropFirst()),
      lineNumber > 0 && lineNumber <= file.lineCount
    else { return nil }
    self = file.line(lineNumber)
  }

}

#if swift(>=5.11)
  extension SourceLine: @retroactive ExpressibleByArgument {}
#else
  extension SourceLine: ExpressibleByArgument {}
#endif
