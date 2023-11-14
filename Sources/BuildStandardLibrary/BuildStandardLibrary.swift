import ArgumentParser
import Foundation
import FrontEnd
import Utils
import CBORCoding

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
    help: "Root directory of Hylo Sources.",
    transform: URL.init(fileURLWithPath:))
  var hyloSourceRoot: URL

  @Argument(
    help: "Paths of hylo standard library source files.",
    transform: URL.init(fileURLWithPath:))
  var librarySourceFiles: [URL]

  func run() throws {
    let encoder = CBOREncoder()
    AST(libraryRoot: standardLibrarySourceRoot)
    encoder.encode(AST(libraryRoot: coreLibrarySourceRoot))
  }

}

