import ArgumentParser
import Compiler
import Foundation
import Utils
import ValModule

struct CLI: ParsableCommand {

  /// The type of the output files to generate.
  enum OutputType: ExpressibleByArgument {

    /// AST before type-checking.
    case rawAST

    /// Val IR before mandatory transformations.
    case rawIR

    /// Val IR.
    case ir

    /// C++ code
    case cpp

    /// Executable binary.
    case binary

    init?(argument: String) {
      switch argument {
      case "raw-ast"  : self = .rawAST
      case "raw-ir"   : self = .rawIR
      case "ir"       : self = .ir
      case "cpp"      : self = .cpp
      case "binary"   : self = .binary
      default         : return nil
      }
    }

  }

  @Flag(
    name: [.customLong("modules")],
    help: "Compile inputs as separate modules.")
  var compileInputAsModules: Bool = false

  @Flag(
    name: [.customLong("import-builtin")],
    help: "Import the built-in module.")
  var importBuiltinModule: Bool = false

  @Flag(
    name: [.customLong("no-std")],
    help: "Do not include the standard library.")
  var noStandardLibrary: Bool = false

  @Flag(
    name: [.customLong("typecheck")],
    help: "Type-check the input file(s).")
  var typeCheckOnly: Bool = false

  @Option(
    name: [.customLong("emit")],
    help: "Emit the specified type output files.")
  var outputType: OutputType = .binary

  @Option(
    name: [.customShort("o")],
    help: "Write output to <o>.",
    transform: URL.init(fileURLWithPath:))
  var outputURL: URL?

  @Flag(
    name: [.short, .long],
    help: "Use verbose output.")
  var verbose: Bool = false

  @Argument(
    transform: URL.init(fileURLWithPath:))
  var inputs: [URL]

  private var warningLabel: String { "warning: ".styled([.bold, .yellow]) }

  private var errorLabel: String { "error: ".styled([.bold, .red]) }

  /// The URL of the current working directory.
  private var currentDirectory: URL {
    URL(fileURLWithPath: FileManager.default.currentDirectoryPath, isDirectory: true)
  }

  mutating func run() throws {
    /// The name of the product being built.
    let productName = "main"
    /// The AST of the program being compiled.
    var rawProgram = AST()

    if compileInputAsModules {
      fatalError("not implemented")
    }

    // *** Parsing ***

    log(verbose: "Parsing '\(productName)'".styled([.bold]))

    // Merge all inputs into the same same module.
    let moduleDecl = rawProgram.insert(ModuleDecl(name: productName))
    for input in inputs {
      if input.hasDirectoryPath {
        if !withFiles(in: input, { insert(contentsOf: $0, into: moduleDecl, in: &rawProgram) }) {
          CLI.exit(withError: ExitCode(-1))
        }
      } else {
        if !insert(contentsOf: input, into: moduleDecl, in: &rawProgram) {
          CLI.exit(withError: ExitCode(-1))
        }
      }
    }

    // Handle `--emit raw-ast`.
    if outputType == .rawAST {
      let url = outputURL ?? URL(fileURLWithPath: "ast.json")
      let encoder = JSONEncoder()
      try encoder.encode(rawProgram).write(to: url, options: .atomic)
      CLI.exit()
    }

    // *** Type checking ***

    log(verbose: "Type-checking '\(productName)'".styled([.bold]))

    // Import the core library.
    rawProgram.stdlib = rawProgram.insert(ModuleDecl(name: "Val"))
    if !withFiles(in: ValModule.core!, {
      insert(contentsOf: $0, into: rawProgram.stdlib!, in: &rawProgram)
    }) {
      CLI.exit(withError: ExitCode(-1))
    }

    // Initialize the type checker.
    var checker = TypeChecker(ast: rawProgram)
    var typeCheckingSucceeded = true

    // Type check the code library.
    checker.isBuiltinModuleVisible = true
    typeCheckingSucceeded = checker.check(module: rawProgram.stdlib!)

    // Type-check the input.
    checker.isBuiltinModuleVisible = importBuiltinModule
    typeCheckingSucceeded = checker.check(module: moduleDecl) && typeCheckingSucceeded

    // Report type-checking errors.
    log(diagnostics: checker.diagnostics)
    if !typeCheckingSucceeded {
      CLI.exit(withError: ExitCode(-1))
    }

    // Exit if `--typecheck` is set.
    if typeCheckOnly { CLI.exit() }

    let typedProgram = TypedProgram(
      ast: checker.ast,
      scopeHierarchy: checker.scopeHierarchy,
      declTypes: checker.declTypes,
      exprTypes: checker.exprTypes,
      referredDecls: checker.referredDecls)

    // *** IR Lowering ***

    log(verbose: "Lowering '\(productName)'".styled([.bold]))

    // Initialize the IR emitter.
    var emitter = Emitter(program: typedProgram)
    var irModule = emitter.emit(module: moduleDecl)

    // Handle `--emit raw-ir`.
    if outputType == .rawIR {
      let url = outputURL ?? URL(fileURLWithPath: productName + ".vir")
      try irModule.description.write(to: url, atomically: true, encoding: .utf8)
      CLI.exit()
    }

    // Run mandatory IR analysis and transformation passes.
    var pipeline: [TransformPass] = [
      ImplicitReturnInsertionPass(),
      DefiniteInitializationPass(program: typedProgram),
      LifetimePass(program: typedProgram),
      // OwnershipPass(program: typedProgram),
    ]

    log(verbose: "Analyzing '\(productName)'".styled([.bold]))
    for i in 0 ..< pipeline.count {
      log(verbose: type(of: pipeline[i]).name)
      var passSuccess = true
      for f in 0 ..< irModule.functions.count {
        passSuccess = pipeline[i].run(function: f, module: &irModule) && passSuccess
        log(diagnostics: pipeline[i].diagnostics)
      }
      guard passSuccess else { CLI.exit(withError: ExitCode(-1)) }
    }

    // Handle `--emit ir`
    if outputType == .ir {
      let url = outputURL ?? URL(fileURLWithPath: productName + ".vir")
      try irModule.description.write(to: url, atomically: true, encoding: .utf8)
      CLI.exit()
    }

    // *** C++ Transpiling ***

    log(verbose: "Traspiling to C++ '\(productName)'".styled([.bold]))

    // Initialize the transpiler.
    var transpiler = CXXTranspiler(program: typedProgram)
    let cppModuleContent = transpiler.emitHeader(of: moduleDecl)

    // Handle `--emit cpp`.
    if outputType == .cpp {
      let url = outputURL ?? URL(fileURLWithPath: productName + ".cpp")
      try cppModuleContent.write(to: url, atomically: true, encoding: .utf8)
      CLI.exit()
    }

    // *** Machine code generation ***

    assert(outputType == .binary)

    let temporaryDirectoryURL = try FileManager.default.url(
      for: .itemReplacementDirectory,
      in: .userDomainMask,
      appropriateFor: currentDirectory,
      create: true)

    // Compile the transpiled module.
    let cppSourceURL = temporaryDirectoryURL.appendingPathComponent(productName + ".cpp")
    try cppModuleContent.write(to: cppSourceURL, atomically: true, encoding: .utf8)

    let clang = find("clang++")
    let binaryURL = outputURL ?? URL(fileURLWithPath: productName)
    try runCommandLine(clang, ["-o", binaryURL.path, cppSourceURL.path])
  }

  /// Parses the contents of the file at `fileURL` and insert them into `ast[module]`.
  func insert(
    contentsOf fileURL: URL,
    into module: NodeID<ModuleDecl>,
    in ast: inout AST
  ) -> Bool {
    switch fileURL.pathExtension {
    case "val":
      log(verbose: fileURL.relativePath)

      // Read the contents of the file.
      let sourceFile: SourceFile
      do {
        sourceFile = try SourceFile(contentsOf: fileURL)
      } catch {
        log(errorLabel + error.localizedDescription)
        return false
      }

      // Parse the file.
      let (decls, parserDiagnostics) = Parser.parse(sourceFile, into: module, in: &ast)
      log(diagnostics: parserDiagnostics)

      // Bail out if the parser failed.
      return decls != nil

    default:
      log(warningLabel + "ignoring file with unsupported extension: \(fileURL.relativePath)")
      return true
    }
  }

  /// Creates a module from the contents at `url` and adds it to the AST.
  ///
  /// - Requires: `url` must denote a directly.
  func addModule(url: URL) {
    fatalError("not implemented")
  }

  /// Logs the contents of `diagnostics` tot he standard error.
  func log<S: Sequence>(diagnostics: S) where S.Element == Diagnostic {
    for d in diagnostics.sorted(by: Diagnostic.isLoggedBefore) {
      log(diagnostic: d)
    }
  }

  /// Logs `diagnostic` to the standard error.
  func log(diagnostic: Diagnostic) {
    // Log the level.
    switch diagnostic.level {
    case .warning:
      write("warning".styled([.bold, .yellow]))
    case .error:
      write("error".styled([.bold, .red]))
    }
    write(": ")

    // Log the location, if available.
    if let location = diagnostic.location {
      let path = location.source.url.relativePath
      let (line, column) = location.source.lineAndColumnIndices(at: location)
      write("\(path):\(line):\(column): ")
    }

    // Log the message.
    write(diagnostic.message)
    write("\n")

    // Log the window
    if let window = diagnostic.window {
      let line = window.range.source.lineContents(at: window.range.first())
      write(line)
      write("\n")

      let padding = line.distance(from: line.startIndex, to: window.range.lowerBound)
      write(String(repeating: " ", count: padding))

      let count = line.distance(
        from: window.range.lowerBound, to: min(window.range.upperBound, line.endIndex))
      if count > 1 {
        write(String(repeating: "~", count: count))
      } else {
        write("^")
      }
      write("\n")
    }
  }

  /// Logs `message` to the standard error file if `--verbose` is set.
  func log(verbose message: @autoclosure () -> String, terminator: String = "\n") {
    if !verbose { return }
    write(message())
    write(terminator)
  }

  /// Logs `message` to the standard error file.
  func log(_ message: String, terminator: String = "\n") {
    write("warning".styled([.bold, .yellow]))
    write(": ")
    write(message)
    write(terminator)
  }

  /// Writes `text` to the standard error file.
  func write<S: StringProtocol>(_ text: S) {
    FileHandle.standardError.write(Data(text.utf8))
  }

  /// Returns the path of the specified executable.
  mutating func find(_ executable: String) -> String {
    // Nothing to do if `executable` is a path
    if executable.contains("/") {
      return executable
    }

    // Check the cache.
    if let path = CLI.executableLocationCache[executable] {
      return path
    }

    // Search in the current working directory.
    var candidateURL = currentDirectory.appendingPathComponent(executable)
    if FileManager.default.fileExists(atPath: candidateURL.path) {
      CLI.executableLocationCache[executable] = candidateURL.path
      return candidateURL.path
    }

    // Search in the PATH.
    let environmentPath = ProcessInfo.processInfo.environment["PATH"] ?? ""
    for base in environmentPath.split(separator: ":") {
      candidateURL = URL(fileURLWithPath: String(base)).appendingPathComponent(executable)
      if FileManager.default.fileExists(atPath: candidateURL.path) {
        CLI.executableLocationCache[executable] = candidateURL.path
        return candidateURL.path
      }
    }

    log(errorLabel + "executable not found: \(executable)")
    CLI.exit(withError: ExitCode(-1))
  }

  /// Executes the program at `path` with the specified arguments in a subprocess.
  @discardableResult
  func runCommandLine(_ programPath: String, _ arguments: [String] = []) throws -> String? {
    log(verbose: ([programPath] + arguments).joined(separator: " "))

    let pipe = Pipe()
    let process = Process()
    process.executableURL = URL(fileURLWithPath: programPath)
    process.arguments = arguments
    process.standardOutput = pipe
    try process.run()
    process.waitUntilExit()

    let data = pipe.fileHandleForReading.readDataToEndOfFile()
    return String(data: data, encoding: .utf8).flatMap({ (result) -> String? in
      let trimmed = result.trimmingCharacters(in: .whitespacesAndNewlines)
      return trimmed.isEmpty ? nil : trimmed
    })
  }

  /// A table mapping executable names to their path.
  private static var executableLocationCache: [String: String] = [:]

}

// Run the program.
CLI.main()
