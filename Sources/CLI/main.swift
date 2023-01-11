import ArgumentParser
import CodeGenCXX
import Core
import Foundation
import FrontEnd
import IR
import Utils
import ValModule

private struct CLI: ParsableCommand {

  /// The type of the output files to generate.
  private enum OutputType: ExpressibleByArgument {

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
      case "raw-ast": self = .rawAST
      case "raw-ir": self = .rawIR
      case "ir": self = .ir
      case "cpp": self = .cpp
      case "binary": self = .binary
      default: return nil
      }
    }

  }

  fileprivate static let configuration = CommandConfiguration(commandName: "valc")

  @Flag(
    name: [.customLong("modules")],
    help: "Compile inputs as separate modules.")
  private var compileInputAsModules: Bool = false

  @Flag(
    name: [.customLong("import-builtin")],
    help: "Import the built-in module.")
  private var importBuiltinModule: Bool = false

  @Flag(
    name: [.customLong("no-std")],
    help: "Do not include the standard library.")
  private var noStandardLibrary: Bool = false

  @Flag(
    name: [.customLong("typecheck")],
    help: "Type-check the input file(s).")
  private var typeCheckOnly: Bool = false

  @Option(
    name: [.customLong("trace-inference")],
    help: ArgumentHelp(
      "Enable tracing of type inference requests at the given line.",
      valueName: "file:line"))
  private var inferenceTracingRange: SourceRange?

  @Option(
    name: [.customLong("emit")],
    help: ArgumentHelp(
      "Emit the specified type output files. From: raw-ast, raw-ir, ir, cpp, binary",
      valueName: "output-type"))
  private var outputType: OutputType = .binary

  @Option(
    name: [.customShort("o")],
    help: ArgumentHelp("Write output to <file>.", valueName: "file"),
    transform: URL.init(fileURLWithPath:))
  private var outputURL: URL?

  @Flag(
    name: [.short, .long],
    help: "Use verbose output.")
  private var verbose: Bool = false

  @Argument(
    transform: URL.init(fileURLWithPath:))
  private var inputs: [URL]

  private var noteLabel: String { "note: ".styled([.bold, .cyan]) }

  private var warningLabel: String { "warning: ".styled([.bold, .yellow]) }

  private var errorLabel: String { "error: ".styled([.bold, .red]) }

  /// The URL of the current working directory.
  private var currentDirectory: URL {
    URL(fileURLWithPath: FileManager.default.currentDirectoryPath, isDirectory: true)
  }

  fileprivate mutating func run() throws {
    if compileInputAsModules {
      fatalError("compilation as modules not yet implemented.")
    }

    var diagnostics = Diagnostics(reportingToStderr: true)

    let productName = "main"
    log(verbose: "Parsing '\(productName)'".styled([.bold]))

    /// The AST of the program being compiled.
    var ast = AST()

    let newModule = try ast.makeModule(
      "Main", sourceCode: sourceFiles(in: inputs), diagnostics: &diagnostics)

    // Handle `--emit raw-ast`.
    if outputType == .rawAST {
      let url = outputURL ?? URL(fileURLWithPath: "ast.json")
      let encoder = JSONEncoder()
      try encoder.encode(ast).write(to: url, options: .atomic)
      CLI.exit()
    }

    // *** Type checking ***

    log(verbose: "Type-checking '\(productName)'".styled([.bold]))

    // Import the core library.
    ast.importCoreModule()

    // Initialize the type checker.
    var checker = TypeChecker(
      program: ScopedProgram(ast),
      isBuiltinModuleVisible: true,
      enablingInferenceTracingIn: inferenceTracingRange)
    var typeCheckingSucceeded = true

    // Type check the core library.
    typeCheckingSucceeded = checker.check(module: checker.program.ast.corelib!)

    // Type-check the input.
    checker.isBuiltinModuleVisible = importBuiltinModule
    typeCheckingSucceeded = checker.check(module: newModule) && typeCheckingSucceeded

    // Report type-checking errors.
    log(diagnostics: checker.diagnostics)
    if !typeCheckingSucceeded {
      CLI.exit(withError: ExitCode(-1))
    }

    // Exit if `--typecheck` is set.
    if typeCheckOnly { CLI.exit() }

    let typedProgram = TypedProgram(
      annotating: checker.program,
      declTypes: checker.declTypes,
      exprTypes: checker.exprTypes,
      implicitCaptures: checker.implicitCaptures,
      referredDecls: checker.referredDecls,
      foldedSequenceExprs: checker.foldedSequenceExprs)

    // *** IR Lowering ***

    log(verbose: "Lowering '\(productName)'".styled([.bold]))

    // Initialize the IR emitter.
    var irModule = Module(newModule, in: typedProgram)

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

    log(verbose: "Transpiling to C++ '\(productName)'".styled([.bold]))

    // Initialize the transpiler.
    var transpiler = CXXTranspiler(program: typedProgram)

    // Translate the module to C++.
    let cxxModule = transpiler.emit(module: typedProgram[newModule])
    let cxxHeader = cxxModule.emitHeader()
    let cxxSource = cxxModule.emitSource()

    // Handle `--emit cpp`.
    if outputType == .cpp {
      let baseURL = outputURL?.deletingPathExtension() ?? URL(fileURLWithPath: productName)
      try cxxHeader.write(
        to: baseURL.appendingPathExtension("h"), atomically: true, encoding: .utf8)
      try cxxSource.write(
        to: baseURL.appendingPathExtension("cpp"), atomically: true, encoding: .utf8)
      CLI.exit()
    }

    // *** Machine code generation ***

    assert(outputType == .binary)

    let buildDirectoryURL = try FileManager.default.url(
      for: .itemReplacementDirectory,
      in: .userDomainMask,
      appropriateFor: currentDirectory,
      create: true)

    // Compile the transpiled module.
    let cxxHeaderURL = buildDirectoryURL.appendingPathComponent(productName + ".h")
    try cxxHeader.write(to: cxxHeaderURL, atomically: true, encoding: .utf8)

    let cxxSourceURL = buildDirectoryURL.appendingPathComponent(productName + ".cpp")
    try cxxSource.write(to: cxxSourceURL, atomically: true, encoding: .utf8)

    let clang = find("clang++")
    let binaryURL = outputURL ?? URL(fileURLWithPath: productName)
    try runCommandLine(
      clang,
      [
        "-o", binaryURL.path,
        "-I", buildDirectoryURL.path,
        cxxSourceURL.path,
      ])
  }

  /// Creates a module from the contents at `url` and adds it to the AST.
  ///
  /// - Requires: `url` must denote a directly.
  private func addModule(url: URL) {
    fatalError("not implemented")
  }

  /// Logs the contents of `diagnostics` tot he standard error.
  private func log<S: Sequence>(diagnostics: S) where S.Element == Diagnostic {
    for d in diagnostics.sorted(by: Diagnostic.isLoggedBefore) {
      log(diagnostic: d)
    }
  }

  /// Logs `diagnostic` to the standard error.
  private func log(diagnostic: Diagnostic, asChild isChild: Bool = false) {
    // Log the location, if available.
    if let location = diagnostic.location?.first() {
      let path = location.file.url.relativePath
      let (line, column) = location.lineAndColumnIndices
      write("\(path):\(line):\(column): ".styled([.bold]))
    }

    // Log the level.
    if isChild {
      write(noteLabel)
    } else {
      switch diagnostic.level {
      case .warning:
        write(warningLabel)
      case .error:
        write(errorLabel)
      }
    }

    // Log the message.
    write(diagnostic.message.styled([.bold]))
    write("\n")

    // Log the window
    if let site = diagnostic.location {
      let line = site.file.lineContents(at: site.first())
      write(line)
      write("\n")

      let padding = line.distance(from: line.startIndex, to: site.start)
      write(String(repeating: " ", count: padding))

      let count = line.distance(
        from: site.start, to: min(site.end, line.endIndex))
      if count > 1 {
        write(String(repeating: "~", count: count))
      } else {
        write("^")
      }
      write("\n")
    }

    // Log the children.
    for child in diagnostic.children {
      log(diagnostic: child, asChild: true)
    }
  }

  /// Logs `message` to the standard error file if `--verbose` is set.
  private func log(verbose message: @autoclosure () -> String, terminator: String = "\n") {
    if !verbose { return }
    write(message())
    write(terminator)
  }

  /// Logs `message` to the standard error file.
  private func log(_ message: String, terminator: String = "\n") {
    write(message)
    write(terminator)
  }

  /// Writes `text` to the standard error file.
  private func write<S: StringProtocol>(_ text: S) {
    FileHandle.standardError.write(Data(text.utf8))
  }

  /// Returns the path of the specified executable.
  mutating private func find(_ executable: String) -> String {
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
  private func runCommandLine(_ programPath: String, _ arguments: [String] = []) throws -> String? {
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

  /// A map from executable name to path of the named binary.
  private static var executableLocationCache: [String: String] = [:]

}

// Run the program.
CLI.main()
