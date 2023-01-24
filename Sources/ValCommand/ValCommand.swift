import ArgumentParser
import CodeGenCXX
import Core
import Foundation
import FrontEnd
import IR
import Utils
import ValModule

public struct ValCommand: ParsableCommand {

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

  /// An error indicating that the compiler's environment is not properly configured.
  fileprivate struct EnvironmentError: Error {

    /// The error's message.
    let message: String

  }

  public static let configuration = CommandConfiguration(commandName: "valc")

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

  public init() {}

  /// The URL of the current working directory.
  private var currentDirectory: URL {
    URL(fileURLWithPath: FileManager.default.currentDirectoryPath, isDirectory: true)
  }

  public func run() throws {
    var errorChannel = StandardErrorStream()
    ValCommand.exit(withError: try execute(loggingTo: &errorChannel))
  }

  /// Executes the command, logging messages to `errorChannel`, and returns its exit status.
  public func execute<ErrorChannel: Channel>(
    loggingTo errorChannel: inout ErrorChannel
  ) throws -> ExitCode {
    if compileInputAsModules {
      fatalError("compilation as modules not yet implemented.")
    }

    var diagnostics = Diagnostics()
    let productName = "main"

    /// The AST of the program being compiled.
    var ast = AST()

    // Parse the source code.
    let newModule: NodeID<ModuleDecl>
    do {
      newModule = try ast.makeModule(
        "Main", sourceCode: sourceFiles(in: inputs), diagnostics: &diagnostics)
    } catch _ as Diagnostics {
      return exit(logging: diagnostics, to: &errorChannel)
    }

    // Handle `--emit raw-ast`.
    if outputType == .rawAST {
      let url = outputURL ?? URL(fileURLWithPath: "ast.json")
      let encoder = JSONEncoder()
      try encoder.encode(ast).write(to: url, options: .atomic)
      return exit(logging: diagnostics, to: &errorChannel)
    }

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
    diagnostics.report(checker.diagnostics)
    if !typeCheckingSucceeded {
      return exit(logging: diagnostics, to: &errorChannel)
    }

    // Exit if `--typecheck` is set.
    if typeCheckOnly { return exit(logging: diagnostics, to: &errorChannel) }

    let typedProgram = TypedProgram(
      annotating: checker.program,
      declTypes: checker.declTypes,
      exprTypes: checker.exprTypes,
      implicitCaptures: checker.implicitCaptures,
      referredDecls: checker.referredDecls,
      foldedSequenceExprs: checker.foldedSequenceExprs)

    // *** IR Lowering ***

    // Initialize the IR emitter.
    var irModule = Module(newModule, in: typedProgram)

    // Handle `--emit raw-ir`.
    if outputType == .rawIR {
      let url = outputURL ?? URL(fileURLWithPath: productName + ".vir")
      try irModule.description.write(to: url, atomically: true, encoding: .utf8)
      return exit(logging: diagnostics, to: &errorChannel)
    }

    // Run mandatory IR analysis and transformation passes.
    var pipeline: [TransformPass] = [
      ImplicitReturnInsertionPass(),
      DefiniteInitializationPass(program: typedProgram),
      LifetimePass(program: typedProgram),
      // OwnershipPass(program: typedProgram),
    ]

    for i in 0 ..< pipeline.count {
      var passSuccess = true
      for f in 0 ..< irModule.functions.count {
        passSuccess = pipeline[i].run(function: f, module: &irModule) && passSuccess
        diagnostics.report(pipeline[i].diagnostics)
      }
      if !passSuccess { return exit(logging: diagnostics, to: &errorChannel) }
    }

    // Handle `--emit ir`
    if outputType == .ir {
      let url = outputURL ?? URL(fileURLWithPath: productName + ".vir")
      try irModule.description.write(to: url, atomically: true, encoding: .utf8)
      return exit(logging: diagnostics, to: &errorChannel)
    }

    // *** C++ Transpiling ***

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
      return exit(logging: diagnostics, to: &errorChannel)
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

    let clang = try find("clang++")
    let binaryURL = outputURL ?? URL(fileURLWithPath: productName)
    try runCommandLine(
      clang,
      [
        "-o", binaryURL.path,
        "-I", buildDirectoryURL.path,
        cxxSourceURL.path,
      ],
      loggingTo: &errorChannel)

    return exit(logging: diagnostics, to: &errorChannel)
  }

  /// Logs the given diagnostics to the standard error and returns a success code if none of them
  /// is an error; otherwise, returns a failure code.
  private func exit<C: Channel>(
    logging diagnostics: Diagnostics,
    to channel: inout C
  ) -> ExitCode {
    log(diagnostics: diagnostics, to: &channel)
    return diagnostics.errorReported ? ExitCode.failure : ExitCode.success
  }

  /// Creates a module from the contents at `url` and adds it to the AST.
  ///
  /// - Requires: `url` must denote a directly.
  private func addModule(url: URL) {
    fatalError("not implemented")
  }

  /// Logs the contents of `diagnostics` tot he standard error.
  private func log<C: Channel>(
    diagnostics: Diagnostics,
    to channel: inout C
  ) {
    for d in diagnostics.log.sorted(by: Diagnostic.isLoggedBefore) {
      log(diagnostic: d, to: &channel)
    }
  }

  /// Logs `diagnostic` to the standard error.
  private func log<C: Channel>(
    diagnostic: Diagnostic,
    asChild isChild: Bool = false,
    to channel: inout C
  ) {
    // Log the location
    let siteFirst = diagnostic.site.first()
    let path = siteFirst.file.url.relativePath
    let (lineFirst, column) = siteFirst.lineAndColumn()
    channel.write("\(path):\(lineFirst):\(column): ", in: [.bold])

    // Log the level.
    if isChild {
      log(label: .note, to: &channel)
    } else {
      log(label: diagnostic.level, to: &channel)
    }

    // Log the message.
    channel.write(diagnostic.message, in: [.bold])
    channel.write("\n")

    // Log the window
    let site = diagnostic.site
    let line = site.first().textOfLine()
    channel.write(String(line))

    let padding = line.distance(from: line.startIndex, to: site.start)
    channel.write(String(repeating: " ", count: padding))

    let count = line.distance(
      from: site.start, to: min(site.end, line.endIndex))
    if count > 1 {
      channel.write(String(repeating: "~", count: count))
    } else {
      channel.write("^")
    }
    channel.write("\n")

    // Log the notes.
    for child in diagnostic.notes {
      log(diagnostic: child, asChild: true, to: &channel)
    }
  }

  /// Logs `message` to the standard error file if `--verbose` is set.
  private func log<C: Channel>(
    verbose message: @autoclosure () -> String,
    terminator: String = "\n",
    to channel: inout C
  ) {
    if !verbose { return }
    channel.write(message())
    channel.write(terminator)
  }

  /// Logs `message` to the standard error file.
  private func log<C: Channel>(
    _ message: String,
    terminator: String = "\n",
    to channel: inout C
  ) {
    channel.write(message)
    channel.write(terminator)
  }

  private func log<C: Channel>(label: Diagnostic.Level, to channel: inout C) {
    switch label {
    case .note:
      channel.write("note: ", in: [.bold, .cyan])
    case .warning:
      channel.write("warning: ", in: [.bold, .yellow])
    case .error:
      channel.write("error: ", in: [.bold, .red])
    }
  }

  /// Returns the path of the specified executable.
  private func find(_ executable: String) throws -> String {
    // Nothing to do if `executable` is a path
    if executable.contains("/") {
      return executable
    }

    // Check the cache.
    if let path = ValCommand.executableLocationCache[executable] {
      return path
    }

    // Search in the current working directory.
    var candidateURL = currentDirectory.appendingPathComponent(executable)
    if FileManager.default.fileExists(atPath: candidateURL.path) {
      ValCommand.executableLocationCache[executable] = candidateURL.path
      return candidateURL.path
    }

    // Search in the PATH.
    let environmentPath = ProcessInfo.processInfo.environment["PATH"] ?? ""
    for base in environmentPath.split(separator: ":") {
      candidateURL = URL(fileURLWithPath: String(base)).appendingPathComponent(executable)
      if FileManager.default.fileExists(atPath: candidateURL.path) {
        ValCommand.executableLocationCache[executable] = candidateURL.path
        return candidateURL.path
      }
    }

    throw EnvironmentError(message: "executable not found: \(executable)")
  }

  /// Executes the program at `path` with the specified arguments in a subprocess.
  @discardableResult
  private func runCommandLine<C: Channel>(
    _ programPath: String,
    _ arguments: [String] = [],
    loggingTo channel: inout C
  ) throws -> String? {
    log(verbose: ([programPath] + arguments).joined(separator: " "), to: &channel)

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
