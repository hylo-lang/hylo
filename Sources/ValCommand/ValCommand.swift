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
    var errorLog = StandardErrorLog()
    ValCommand.exit(withError: try execute(loggingTo: &errorLog))
  }

  /// Executes the command, logging messages to `errorLog`, and returns its exit status.
  public func execute<ErrorLog: Log>(loggingTo errorLog: inout ErrorLog) throws -> ExitCode {
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
      return finalize(logging: diagnostics, to: &errorLog)
    }

    // Handle `--emit raw-ast`.
    if outputType == .rawAST {
      let url = outputURL ?? URL(fileURLWithPath: "ast.json")
      let encoder = JSONEncoder()
      try encoder.encode(ast).write(to: url, options: .atomic)
      return finalize(logging: diagnostics, to: &errorLog)
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
      return finalize(logging: diagnostics, to: &errorLog)
    }

    // Exit if `--typecheck` is set.
    if typeCheckOnly { return finalize(logging: diagnostics, to: &errorLog) }

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
      return finalize(logging: diagnostics, to: &errorLog)
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
      if !passSuccess { return finalize(logging: diagnostics, to: &errorLog) }
    }

    // Handle `--emit ir`
    if outputType == .ir {
      let url = outputURL ?? URL(fileURLWithPath: productName + ".vir")
      try irModule.description.write(to: url, atomically: true, encoding: .utf8)
      return finalize(logging: diagnostics, to: &errorLog)
    }

    // *** C++ Transpiling ***

    // Initialize the transpiler & code writer.
    var transpiler = CXXTranspiler(program: typedProgram)
    let codeWriter = CXXCodeWriter()

    // Translate the module to C++ AST.
    let cxxModule = transpiler.emit(module: typedProgram[newModule])

    // Generate the C++ code, header & source.
    let cxxHeaderCode = codeWriter.emitHeaderCode(cxxModule)
    let cxxSourceCode = codeWriter.emitSourceCode(cxxModule)

    // Handle `--emit cpp`.
    if outputType == .cpp {
      let baseURL = outputURL?.deletingPathExtension() ?? URL(fileURLWithPath: productName)
      try cxxHeaderCode.write(
        to: baseURL.appendingPathExtension("h"), atomically: true, encoding: .utf8)
      try cxxSourceCode.write(
        to: baseURL.appendingPathExtension("cpp"), atomically: true, encoding: .utf8)
      return finalize(logging: diagnostics, to: &errorLog)
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
    try cxxHeaderCode.write(to: cxxHeaderURL, atomically: true, encoding: .utf8)

    let cxxSourceURL = buildDirectoryURL.appendingPathComponent(productName + ".cpp")
    try cxxSourceCode.write(to: cxxSourceURL, atomically: true, encoding: .utf8)

    let clang = try find("clang++")
    let binaryURL = outputURL ?? URL(fileURLWithPath: productName)
    try runCommandLine(
      clang,
      [
        "-o", binaryURL.path,
        "-I", buildDirectoryURL.path,
        cxxSourceURL.path,
      ],
      loggingTo: &errorLog)

    return finalize(logging: diagnostics, to: &errorLog)
  }

  /// Logs the given diagnostics to the standard error and returns a success code if none of them
  /// is an error; otherwise, returns a failure code.
  private func finalize<L: Log>(logging diagnostics: Diagnostics, to log: inout L) -> ExitCode {
    log.log(diagnostics: diagnostics)
    return diagnostics.errorReported ? ExitCode.failure : ExitCode.success
  }

  /// Creates a module from the contents at `url` and adds it to the AST.
  ///
  /// - Requires: `url` must denote a directly.
  private func addModule(url: URL) {
    fatalError("not implemented")
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
  private func runCommandLine<L: Log>(
    _ programPath: String,
    _ arguments: [String] = [],
    loggingTo log: inout L
  ) throws -> String? {
    if verbose {
      log.log(([programPath] + arguments).joined(separator: " "))
    }

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
