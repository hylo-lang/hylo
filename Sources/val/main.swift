import Foundation
import Driver

func main(commandLineArgs: [String]) throws {
  let args = try ArgumentParser(commandLineArgs)

  // Create a new driver.
  let driver = Driver()
  driver.context.diagnosticConsumer = Terminal(sourceManager: driver.context.sourceManager)

  // Load the standard library.
  try driver.loadStdLib(path: args.sysroot)

  // Load the given input files as a module.
  if !args.files.isEmpty {
    // Parse the module.
    let files = args.files.map(URL.init(fileURLWithPath:))
    let decl = try driver.parse(moduleName: "main", moduleFiles: files)

    // Type check the module.
    guard driver.typeCheck(moduleDecl: decl) else { return }

    // Dump the module, if requested.
    if args.dumpAST {
      driver.dump()
    }

    // Lower and evaluate the module.
    let module = try driver.lower(moduleDecl: decl)
    try driver.eval(module: module)
  }
}

try main(commandLineArgs: CommandLine.arguments)
