import Foundation
import IR

/// Reports the given `diagnostic` on the standard error and exit with status -1.
func error(_ diagnostic: String) -> Never {
  let d = Data("\(diagnostic)\n".utf8)
  FileHandle.standardError.write(d)
  exit(-1)
}

func main() {
  guard CommandLine.arguments.count > 1 else {
    error("missing input")
  }

  let n = CommandLine.arguments[1]
  guard let s = DemangledSymbol(n) else {
    error("could not demangle '\(n)'")
  }

  print(s)
}

main()
