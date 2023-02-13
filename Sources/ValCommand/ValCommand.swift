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

  /// Executes the command, logging Val messages to `errorLog`, and returns its exit status.
  ///
  /// Propagates any thrown errors that are not Val diagnostics,
  public func execute<ErrorLog: Log>(loggingTo errorLog: inout ErrorLog) throws -> ExitCode {
    do {
      try execute1(loggingTo: &errorLog)
    } catch let d as DiagnosticSet {
      assert(d.containsError, "Diagnostics containing no errors were thrown")
      return ExitCode.failure
    }
    return ExitCode.success
  }

  /// Executes the command, logging Val messages to `errorLog`.
  public func execute1<ErrorLog: Log>(loggingTo errorLog: inout ErrorLog) throws {
    var diagnostics = DiagnosticSet()
    defer { errorLog.log(diagnostics: diagnostics) }

    if compileInputAsModules {
      fatalError("compilation as modules not yet implemented.")
    }

    let productName = makeProductName(inputs)

    /// The AST of the program being compiled.
    var ast = AST.coreModule

    // Parse the source code.
    let newModule = try ast.makeModule(
      productName, sourceCode: sourceFiles(in: inputs),
      builtinModuleAccess: importBuiltinModule,
      diagnostics: &diagnostics)

    // Handle `--emit raw-ast`.
    if outputType == .rawAST {
      let url = outputURL ?? URL(fileURLWithPath: productName + ".ast.json")
      let encoder = JSONEncoder().forAST
      try encoder.encode(ast).write(to: url, options: .atomic)
      return
    }

    let typedProgram = try TypedProgram(
      ast, tracingInferenceIn: inferenceTracingRange, diagnostics: &diagnostics)

    // Exit if `--typecheck` is set.
    if typeCheckOnly { return }

    // *** IR Lowering ***

    // Initialize the IR emitter.
    var irModule = try Module(lowering: newModule, in: typedProgram, diagnostics: &diagnostics)

    // Handle `--emit raw-ir`.
    if outputType == .rawIR {
      let url = outputURL ?? URL(fileURLWithPath: productName + ".vir")
      try irModule.description.write(to: url, atomically: true, encoding: .utf8)
      return
    }

    do {
      let p = PassPipeline(withMandatoryPassesForModulesLoweredFrom: typedProgram)
      try p.apply(&irModule, reportingDiagnosticsInto: &diagnostics)
    } catch let d as DiagnosticSet {
      diagnostics.formUnion(d)
      return
    }

    // Handle `--emit ir`
    if outputType == .ir {
      let url = outputURL ?? URL(fileURLWithPath: productName + ".vir")
      try irModule.description.write(to: url, atomically: true, encoding: .utf8)
      return
    }

    // *** C++ Transpiling ***

    let clangFormat = try? find("clang-format")
    let codeFormatter: CXXCodeFormatter =
      clangFormat != nil
      ? ClangFormatFormatter(URL(fileURLWithPath: clangFormat!))
      : IdentityFormatter()

    // Initialize the transpiler & code writer.
    let transpiler = CXXTranspiler(typedProgram)
    var codeWriter = CXXCodeWriter(formatter: codeFormatter)

    // Generate C++ code: Val's StdLib + current module.
    let cxxStdLibModule = transpiler.transpile(stdlib: typedProgram.corelib!)
    let cxxStdLib = codeWriter.cxxCode(cxxStdLibModule)
    let cxxCode = codeWriter.cxxCode(transpiler.transpile(typedProgram[newModule]))

    // Handle `--emit cpp`.
    if outputType == .cpp {
      try write(
        cxxStdLib,
        to: outputURL?.deletingLastPathComponent().appendingPathComponent(cxxStdLibModule.name)
          ?? URL(fileURLWithPath: cxxStdLibModule.name),
        loggingTo: &errorLog)
      try write(
        cxxCode,
        to: outputURL?.deletingPathExtension() ?? URL(fileURLWithPath: productName),
        loggingTo: &errorLog)
      return
    }

    // *** Machine code generation ***

    assert(outputType == .binary)

    let buildDirectoryURL = try FileManager.default.url(
      for: .itemReplacementDirectory,
      in: .userDomainMask,
      appropriateFor: currentDirectory,
      create: true)

    // Write the C++ code to the build directory.
    try write(
      cxxStdLib,
      to: buildDirectoryURL.appendingPathComponent(cxxStdLibModule.name),
      loggingTo: &errorLog)
    try write(
      cxxCode, to: buildDirectoryURL.appendingPathComponent(productName), loggingTo: &errorLog)

    let clang = try find("clang++")
    let binaryURL = outputURL ?? URL(fileURLWithPath: productName)
    try runCommandLine(
      clang,
      [
        "-o", binaryURL.path,
        "-I", buildDirectoryURL.path,
        buildDirectoryURL.appendingPathComponent(productName + ".cpp").path,
      ],
      loggingTo: &errorLog)
  }

  /// If `inputs` contains a single URL `u` whose path is non-empty, returns the last component of
  /// `u` without any path extension and stripping all leading dots. Otherwise, returns "Main".
  private func makeProductName(_ inputs: [URL]) -> String {
    if let u = inputs.uniqueElement {
      let n = u.deletingPathExtension().lastPathComponent.drop(while: { $0 == "." })
      if !n.isEmpty { return String(n) }
    }
    return "Main"
  }

  /// Writes the code for a C++ translation unit to .h/.cpp files at `baseUrl`.
  private func write<L: Log>(
    _ source: TranslationUnitCode, to baseURL: URL, loggingTo log: inout L
  ) throws {
    try write(source.headerCode, toURL: baseURL.appendingPathExtension("h"), loggingTo: &log)
    try write(source.sourceCode, toURL: baseURL.appendingPathExtension("cpp"), loggingTo: &log)
  }

  /// Writes `source` to the `filename`, possibly with verbose logging.
  private func write<L: Log>(_ source: String, toURL url: URL, loggingTo log: inout L) throws {
    if verbose {
      log.log("Writing \(url)")
    }
    try source.write(to: url, atomically: true, encoding: .utf8)
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

    // Search in the PATH(for Windows).
    #if os(Windows)
      let environmentPath = ProcessInfo.processInfo.environment["Path"] ?? ""
      for base in environmentPath.split(separator: ";") {
        candidateURL = URL(fileURLWithPath: String(base)).appendingPathComponent(executable)
        if FileManager.default.fileExists(atPath: candidateURL.path + ".exe") {
          ValCommand.executableLocationCache[executable] = candidateURL.path
          return candidateURL.path
        }
      }
    // Search in the PATH(for Linux and MacOS).
    #else
      let environmentPath = ProcessInfo.processInfo.environment["PATH"] ?? ""
      for base in environmentPath.split(separator: ":") {
        candidateURL = URL(fileURLWithPath: String(base)).appendingPathComponent(executable)
        if FileManager.default.fileExists(atPath: candidateURL.path) {
          ValCommand.executableLocationCache[executable] = candidateURL.path
          return candidateURL.path
        }
      }
    #endif
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
