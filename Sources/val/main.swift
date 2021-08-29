import Foundation

import ArgumentParser
import Driver
import Eval

/// The compiler's command parser.
struct ValCommand: ParsableCommand {

  /// The home path for Val's runtime and standard library.
  static var home = URL(fileURLWithPath: "/opt/local/lib/val")

  @Argument(help: "The input file(s).", transform: URL.init(fileURLWithPath:))
  var input: [URL]

  @Option(help: "The location Val's runtime environment.", transform: URL.init(fileURLWithPath:))
  var home = ValCommand.home

  @Flag(help: "Do not load the standard library.")
  var noStdlib = false

  @Flag(help: "Parse input file(s) and dump AST(s), before semantic analyis.")
  var dumpRawAST = false

  @Flag(help: "Parse and type-check input file(s) and dump AST(s).")
  var dumpAST = false

  @Flag(help: "Emit raw VIL code.")
  var emitVIL = false

  func run() throws {
    // Create a new driver.
    let driver = Driver(home: home)
    driver.context.diagConsumer = Terminal(sourceManager: driver.context.sourceManager)

    // Load the standard library.
    if !noStdlib {
      try driver.loadStdlib()
    }

    // Load the given input files as a module.
    if !input.isEmpty {
      // Parse the module.
      let decl = try driver.parse(moduleName: "main", moduleFiles: input)

      // Dump the module before semantic analysis, if requested.
      if dumpRawAST {
        decl.dump(context: driver.context)
        return
      }

      // Type check the module.
      guard driver.typeCheck(moduleDecl: decl) else { return }

      // Dump the module, if requested.
      if dumpAST {
        decl.dump(context: driver.context)
        return
      }

      // Lower the module to VIL code.
      let main = try driver.lower(moduleDecl: decl)

      // Dump the VIL code if requested.
      if emitVIL {
        main.dump()
        return
      }

      // Interpret the module.
      var interpreter = Interpreter()
      if let stdlib = driver.context.stdlib {
        try interpreter.load(module: driver.lower(moduleDecl: stdlib))
      }
      try interpreter.load(module: main)
      let status = interpreter.start()
      if status != 0 {
        print("Program exited with status \(status)")
      }
    }
  }

}

ValCommand.main()
