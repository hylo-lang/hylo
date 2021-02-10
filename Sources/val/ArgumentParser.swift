import Foundation

/// An command-line argument parser.
struct ArgumentParser {

  init(_ args: [String]) throws {
    // Set default values.
    sysroot = URL(fileURLWithPath: FileManager.default.currentDirectoryPath)

    // Parse the arguments.
    var i = 1
    while i < args.count {
      switch args[i] {
      case "--sysroot":
        guard ((i + 1) < args.count) && !args[i + 1].starts(with: "-") else {
          throw missingArgument(for: args[i])
        }
        sysroot = URL(fileURLWithPath: args[i + 1])
        i = i + 2

      case "--dump-ast":
        dumpAST = true
        i = i + 1

      default:
        if args[i].starts(with: "--") {
          throw illegalParameter(args[i])
        } else {
          throw unexpectedArgument(args[i])
        }
      }
    }

  }

  /// The path where the compiler looks for Val modules.
  var sysroot: URL

  /// An option to dump the AST after the semantic analysis.
  var dumpAST = false

  private func missingArgument(for parameter: String) -> ArgumentParserError {
    return ArgumentParserError(message: "missing argument value for '\(parameter)'")
  }

  private func illegalParameter(_ parameter: String) -> ArgumentParserError {
    return ArgumentParserError(message: "illegal parameter '\(parameter)'")
  }

  private func unexpectedArgument(_ argument: String) -> ArgumentParserError {
    return ArgumentParserError(message: "unexpected argument '\(argument)'")
  }

}

struct ArgumentParserError: Error {

  let message: String
}
