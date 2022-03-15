import ArgumentParser
import Driver
import Eval
import Foundation

/// The compiler's command parser.
struct ValCommand: ParsableCommand {

  @Argument(help: "The input file(s).", transform: URL.init(fileURLWithPath:))
  var input: [URL]

  @Option(help: "The location Val's runtime environment.", transform: URL.init(fileURLWithPath:))
  var home: URL?

  @Flag(help: "Do not load the standard library.")
  var noStdlib = false

  @Flag(help: "Parse input file(s) and dump AST(s), before semantic analyis.")
  var dumpParse = false

  @Flag(help: "Parse and type-check input file(s) and dump AST(s).")
  var dumpAST = false

  @Flag(help: "Emit raw VIL code.")
  var emitVIL = false

  func run() throws {
    // Create a new driver.
    let driver = Driver(home: home)
    DiagDispatcher.instance.register(consumer: Terminal())

    // Load the standard library.
    if !noStdlib {
      try driver.loadStdlib()
    }

    // Load the given input files as a module.
    if !input.isEmpty {
      // Parse the module.
      let sources = try input.map(SourceFile.init(url:))
      let decl = try driver.parse(moduleName: "main", sources: sources)

      // Dump the module before semantic analysis, if requested.
      if dumpParse {
        decl.dump()
        return
      }

      // Type check the module.
      guard driver.typeCheck(moduleDecl: decl) else { return }

      // Dump the module, if requested.
      if dumpAST {
        decl.dump()
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
      if let stdlib = driver.compiler.stdlib {
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
