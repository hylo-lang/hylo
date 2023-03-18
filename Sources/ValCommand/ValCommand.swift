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
      case "raw-ast":
        self = .rawAST
      case "raw-ir":
        self = .rawIR
      case "ir":
        self = .ir
      case "cpp":
        self = .cpp
      case "binary":
        self = .binary
      default:
        return nil
      }
    }

  }

  /// The identifier of a C++ compiler.
  private enum CXXCompiler: String, ExpressibleByArgument, RawRepresentable {

    case clang

    case gcc

    case msvc

    case zig

    init?(argument: String) {
      guard let s = CXXCompiler(rawValue: argument) else { return nil }
      #if !os(Windows)
        if s == .msvc { return nil }
      #endif
      self = s
    }

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
    name: [.customLong("cc")],
    help: ArgumentHelp(
      "Select the C++ compiler used by the Val backend. From: clang, gcc, msvc (Windows only)",
      valueName: "CXXCompiler"))
  private var cxxCompiler: CXXCompiler = .clang

  @Option(
    name: [.customLong("cc-flags")],
    help: ArgumentHelp(
      "Specify flags for the CXX compiler to use",
      valueName: "CXXCompilerFlags"))
  private var ccFlags: [String] = []

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
      try executeCommand(loggingTo: &errorLog)
    } catch let d as DiagnosticSet {
      assert(d.containsError, "Diagnostics containing no errors were thrown")
      return ExitCode.failure
    }
    return ExitCode.success
  }

  /// Executes the command, logging Val messages to `errorLog`.
  private func executeCommand<ErrorLog: Log>(loggingTo errorLog: inout ErrorLog) throws {
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

    var irProgram: [ModuleDecl.ID: IR.Module] = [:]
    for m in ast.modules {
      var ir = try IR.Module(lowering: m, in: program, diagnostics: &diagnostics)
      if outputType != .rawIR {
        try ir.applyMandatoryPasses(reportingDiagnosticsInto: &diagnostics)
      }
      irProgram[m] = ir
    }

    if outputType == .ir || outputType == .rawIR {
      try irProgram[sourceModule]!.description
        .write(to: irFile(productName), atomically: true, encoding: .utf8)
      return
    }

    // C++

    let codeFormatter: CodeTransform? = (try? find("clang-format")).map({
      clangFormatter(URL(fileURLWithPath: $0))
    })

    let cxxModules = (
      core: program.cxx(program.coreLibrary!, withFormatter: codeFormatter),
      source: program.cxx(program[sourceModule], withFormatter: codeFormatter)
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

  /// Returns `outputURL` transformed as a suitable executable file path, using `productName` as
  /// a default name if `outputURL` is `nil`.
  ///
  /// The returned path has a `.exe` extension on Windows.
  private func executableOutputPath(_ outputURL: URL?, default productName: String) -> String {
    var binaryPath = outputURL?.path ?? URL(fileURLWithPath: productName).path
    #if os(Windows)
      if !binaryPath.hasSuffix(".exe") { binaryPath += ".exe" }
    #endif
    return binaryPath
  }

  /// Given the transpiled core and source modules and the desired name of compiler's product,
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

    let binaryPath = executableOutputPath(outputURL, default: productName)
    var arguments: [String] = []
    if cxxCompiler == .msvc {
      arguments = ccFlags.map({ "/\($0)" })
      arguments += [
        buildDirectory.appendingPathComponent(cxxModules.core.syntax.name + ".cpp").path,
        buildDirectory.appendingPathComponent(productName + ".cpp").path,
        "/link",
        "/out:" + binaryPath,
      ]
    } else if cxxCompiler == .zig{
      arguments = ccFlags.map({ "-\($0)" })
      arguments += [
        "c++",
        "-o", binaryPath,
        "-I", buildDirectory.path,
        buildDirectory.appendingPathComponent(cxxModules.core.syntax.name + ".cpp").path,
        buildDirectory.appendingPathComponent(productName + ".cpp").path,
      ]
    } else {
      arguments = ccFlags.map({ "-\($0)" })
      arguments += [
        "-o", binaryPath,
        "-I", buildDirectory.path,
        buildDirectory.appendingPathComponent(cxxModules.core.syntax.name + ".cpp").path,
        buildDirectory.appendingPathComponent(productName + ".cpp").path,
      ]
    }
    try runCommandLine(find(cxxCompiler), arguments, loggingTo: &errorLog)
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

  /// Returns the path of the specified C++ compiler's executable.
  private func find(_ compiler: CXXCompiler) throws -> String {
    switch compiler {
    case .clang:
      return try find("clang++")
    case .gcc:
      return try find("g++")
    case .zig:
      return try find("zig")
    case .msvc:
      return try find("cl")
    }
  }

  /// Returns the path of the specified executable.
  private func find(_ executable: String) throws -> String {
    if let path = ValCommand.executableLocationCache[executable] { return path }

    // Search in the current working directory.
    var candidate = currentDirectory.appendingPathComponent(executable)
    if FileManager.default.fileExists(atPath: candidate.path) {
      ValCommand.executableLocationCache[executable] = candidate.path
      return candidate.path
    }

    // Search in the PATH.
    #if os(Windows)
      let environment = ProcessInfo.processInfo.environment["Path"] ?? ""
      for base in environment.split(separator: ";") {
        candidate = URL(fileURLWithPath: String(base)).appendingPathComponent(executable)
        if FileManager.default.fileExists(atPath: candidate.path + ".exe") {
          ValCommand.executableLocationCache[executable] = candidate.path
          return candidate.path
        }
      }
    #else
      let environment = ProcessInfo.processInfo.environment["PATH"] ?? ""
      for base in environment.split(separator: ":") {
        candidate = URL(fileURLWithPath: String(base)).appendingPathComponent(executable)
        if FileManager.default.fileExists(atPath: candidate.path) {
          ValCommand.executableLocationCache[executable] = candidate.path
          return candidate.path
        }
      }
    #endif

    throw EnvironmentError("executable not found: \(executable)")
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

  /// Given the desired name of the compiler's product, returns the file to write when "raw-ast" is
  /// selected as the output type.
  private func astFile(_ productName: String) -> URL {
    outputURL ?? URL(fileURLWithPath: productName + ".ast.json")
  }

  /// Given the desired name of the compiler's product, returns the file to write when "ir" or
  /// "raw-ir" is selected as the output type.
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

  /// Given the desired name of the compiler's product, returns the base path (sans extension) of
  /// the `.cpp` and `.h` files representing the module whose files are given on the command-line,
  /// when "cpp" is selected as the output type.
  private func sourceModuleCXXOutputBase(_ productName: String) -> URL {
    outputURL?.deletingPathExtension() ?? URL(fileURLWithPath: productName)
  }

}
