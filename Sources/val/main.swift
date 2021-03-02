import Foundation

import Driver
import Eval

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

    // Lower the module to VIL code.
    let main = try driver.lower(moduleDecl: decl)
    main.dump()

    // Interpret the module.
    var interpreter = Interpreter(context: driver.context)
    try interpreter.load(module: driver.lower(moduleDecl: driver.context.stdlib!))
    try interpreter.load(module: main)
    try interpreter.start()
  }
}

try main(commandLineArgs: CommandLine.arguments)
