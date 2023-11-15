import ArgumentParser
import Core
import Foundation
import FrontEnd
import Utils
import CBORCoding

extension URL {
  
  func startsWith(_ possiblePrefix: URL) -> Bool {
    return pathComponents.starts(with: possiblePrefix.pathComponents) && scheme == possiblePrefix.scheme
  }
  
}

/// A command-line tool that generates a Hylo standard library module as part of our build process.
@main
struct BuildStandardLibrary: ParsableCommand {

  @Option(
    name: [.customShort("o")],
    help: ArgumentHelp("Write output to <file>.", valueName: "module-file"),
    transform: URL.init(fileURLWithPath:))
  var outputURL: URL

  @Option(
    name: [.customShort("r")],
    help: "Root directory of the Hylo sources containing the standard library source files.",
    transform: URL.init(fileURLWithPath:))
  var hyloCheckoutRoot: URL

  @Argument(
    help: "Paths of hylo standard library source files.",
    transform: URL.init(fileURLWithPath:))
  var librarySourceFiles: [URL]

  func run() throws {
    let c = CBOREncoder().forAST
    let fullLibrary = try AST(standardLibrarySources: librarySourceFiles.map(SourceFile.init(contentsOf:)))
    
    let freestandingLibraryRoot = hyloCheckoutRoot.appending(path: "StandardLibrary/Sources/Core")
    let freestandingLibrary = try AST(standardLibrarySources: librarySourceFiles.filter { $0.startsWith(freestandingLibraryRoot)}.map(SourceFile.init(contentsOf:)))
    
    try c.encode([fullLibrary, freestandingLibrary]).write(to: outputURL, options: .atomic)
  }

}

