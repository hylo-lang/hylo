import ArgumentParser
import Foundation
import FrontEnd
import Utils

/// A command-line tool that generates a Hylo standard library module as part of our build process.
@main
struct BuildStandardLibrary: ParsableCommand {

  @Option(
    name: [.customShort("o")],
    help: ArgumentHelp("Write output to <file>.", valueName: "module-file"),
    transform: URL.init(fileURLWithPath:))
  var outputURL: URL

  @Argument(
    help: "Root directory of Hylo Sources.",
    transform: URL.init(fileURLWithPath:))
  var hyloSourceFiles: [URL]

  func run() throws {
    //
  }

}
