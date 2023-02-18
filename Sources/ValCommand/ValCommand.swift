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
  private var inferenceTracingSite: SourceLine?

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

    var ast = AST.coreModule

    // The module whose Val files were given on the command-line
    let sourceModule = try ast.makeModule(
      productName, sourceCode: sourceFiles(in: inputs),
      builtinModuleAccess: importBuiltinModule, diagnostics: &diagnostics)

    if outputType == .rawAST {
      try write(ast, to: astFile(productName))
      return
    }

    let program = try TypedProgram(
      ast, tracingInferenceIn: inferenceTracingSite, diagnostics: &diagnostics)
    if typeCheckOnly { return }

    // IR

    var sourceIR = try IR.Module(lowering: sourceModule, in: program, diagnostics: &diagnostics)
    if outputType != .rawIR {
      try sourceIR.applyMandatoryPasses(reportingDiagnosticsInto: &diagnostics)
    }
    if outputType == .ir || outputType == .rawIR {
      try sourceIR.description.write(to: irFile(productName), atomically: true, encoding: .utf8)
      return
    }

    // C++

    let cxxModules = (
      core: program.cxx(program.corelib!), source: program.cxx(program[sourceModule])
    )

    if outputType == .cpp {
      try write(cxxModules.core, to: coreLibCXXOutputBase, loggingTo: &errorLog)
      try write(cxxModules.source, to: sourceModuleCXXOutputBase(productName), loggingTo: &errorLog)
      return
    }

    // Executables

    assert(outputType == .binary)

    try writeExecutableCode(cxxModules, productName: productName, loggingTo: &errorLog)
  }

  /// Returns the path for executable file.
  /// Use `productName` to generate executable file location if `outputURL` is nil.
  private func executablePath(_ outputURL: URL?, _ name: productName) -> String {
    var binaryPath = outputURL?.path ?? URL(fileURLWithPath: productName).path

    //Generate binary programs with `.exe` suffix in windows
    #if os(Windows)
      if !binaryPath.hasSuffix(".exe") {
        binaryPath += ".exe"
      }
    #endif

    return binaryPath
  }

  /// Given the transpiled core and source modules and the product name (whatever that means),
  /// generates a binary product into a temporary build directory, logging errors to `errorLog`.
  func writeExecutableCode<L: Log>(
    _ cxxModules: (core: TypedProgram.CXXModule, source: TypedProgram.CXXModule),
    productName: String,
    loggingTo errorLog: inout L
  ) throws {
    let buildDirectory = FileManager.default.temporaryDirectory

    try write(
      cxxModules.core, to: buildDirectory.appendingPathComponent(CXXTranspiler.coreLibModuleName),
      loggingTo: &errorLog)

    try write(
      cxxModules.source, to: buildDirectory.appendingPathComponent(productName),
      loggingTo: &errorLog)

    let clang = try find("clang++")

    let binaryPath = executablePath(outputURL, productName)

    try runCommandLine(
      clang,
      [
        "-o", binaryPath,
        "-I", buildDirectory.path,
        buildDirectory.appendingPathComponent(productName + ".cpp").path,
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

  /// Writes the code for `m` to .h/.cpp files having the given `basePath`, logging diagnostics to
  /// `log`.
  private func write<L: Log>(
    _ m: TypedProgram.CXXModule, to basePath: URL, loggingTo log: inout L
  ) throws {
    try write(m.text.headerCode, toURL: basePath.appendingPathExtension("h"), loggingTo: &log)
    try write(m.text.sourceCode, toURL: basePath.appendingPathExtension("cpp"), loggingTo: &log)
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
      for bareType in environmentPath.split(separator: ";") {
        candidateURL = URL(fileURLWithPath: String(bareType)).appendingPathComponent(executable)
        if FileManager.default.fileExists(atPath: candidateURL.path + ".exe") {
          ValCommand.executableLocationCache[executable] = candidateURL.path
          return candidateURL.path
        }
      }
    // Search in the PATH(for Linux and MacOS).
    #else
      let environmentPath = ProcessInfo.processInfo.environment["PATH"] ?? ""
      for bareType in environmentPath.split(separator: ":") {
        candidateURL = URL(fileURLWithPath: String(bareType)).appendingPathComponent(executable)
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

  /// Writes a textual descriptioni of `input` to the given `output` file.
  func write(_ input: AST, to output: URL) throws {
    let encoder = JSONEncoder().forAST
    try encoder.encode(input).write(to: output, options: .atomic)
  }

  /// Given the product name (whatever that means), returns the file to write when "raw-ast" is
  /// selected as the output type.
  private func astFile(_ productName: String) -> URL {
    outputURL ?? URL(fileURLWithPath: productName + ".ast.json")
  }

  /// Given the product name (whatever that means), returns the file to write when "ir" or "raw-ir"
  /// is selected as the output type.
  private func irFile(_ productName: String) -> URL {
    outputURL ?? URL(fileURLWithPath: productName + ".vir")
  }

  /// The base path (sans extension) of the `.cpp` and `.h` files representing the core library when
  /// "cpp" is selected as the output type.
  private var coreLibCXXOutputBase: URL {
    outputURL?.deletingLastPathComponent()
      .appendingPathComponent(CXXTranspiler.coreLibModuleName)
      ?? URL(fileURLWithPath: CXXTranspiler.coreLibModuleName)
  }

  /// Given the product name (whatever that means), returns the base path (sans extension) of the
  /// `.cpp` and `.h` files representing the module whose files are given on the command-line, when
  /// "cpp" is selected as the output type.
  private func sourceModuleCXXOutputBase(_ productName: String) -> URL {
    outputURL?.deletingPathExtension() ?? URL(fileURLWithPath: productName)
  }

}

extension TypedProgram {

  /// The bundle of products resulting from transpiling a module to C++.
  typealias CXXModule = (syntax: CodeGenCXX.CXXModule, text: TranslationUnitCode)

  /// Returns the C++ Transpilation of `m`.
  func cxx(_ m: ModuleDecl.Typed) -> CXXModule {
    let x = CXXTranspiler(self).cxx(m)
    var w = CXXCodeWriter()
    return (syntax: x, text: w.cxxCode(x))
  }

}
