import ArgumentParser
import CBORCoding
import Core
import Foundation
import FrontEnd
import Utils

/// A command-line tool that generates a Hylo standard library module as part of our build process.
struct BuildStandardLibrary: ParsableCommand {

  @Option(
    name: [.customShort("o")],
    help: ArgumentHelp("Write output to <file>.", valueName: "module-file"),
    transform: URL.init(fileURLWithPath:))
  var outputURL: URL

  @Argument(
    help: "The Hylo source files that comprise the library.",
    transform: URL.init(fileURLWithPath:))
  var hyloSourceFiles: [URL]

  func run() throws {
    try CBOREncoder().forAST
      .encode(AST(sources: hyloSourceFiles, for: CompilerConfiguration([])))
      .write(to: outputURL, options: .atomic)
  }

}

extension AST {

  /// Creates an instance that includes the Hylo library built from the given `sources`.
  init(sources: [URL], for compiler: CompilerConfiguration) throws {
    self.init(for: compiler)
    var diagnostics = DiagnosticSet()
    coreLibrary = try makeModule(
      "Hylo",
      sourceCode: sources.map(SourceFile.init(contentsOf:)),
      builtinModuleAccess: true,
      diagnostics: &diagnostics)
    precondition(isCoreModuleLoaded)
    self.coreTraits = .init(self)
  }

}

public func main() {
  do {
    try BuildStandardLibrary().run()
  }
  catch let e {
    fatalError("\(e)")
  }
}
