import ArgumentParser
import CodeGenCXX
import CodeGenLLVM
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

    /// LLVM IR
    case llvm

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
      case "llvm":
        self = .llvm
      case "binary":
        self = .binary
      default:
        return nil
      }
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
    var ast = noStandardLibrary ? AST.coreModule : AST.standardLibrary

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

    var irModules: [ModuleDecl.ID: IR.Module] = [:]
    for m in ast.modules {
      var ir = try IR.Module(lowering: m, in: program, diagnostics: &diagnostics)
      if outputType != .rawIR {
        try ir.applyMandatoryPasses(reportingDiagnosticsInto: &diagnostics)
      }
      irModules[m] = ir
    }

    if outputType == .ir || outputType == .rawIR {
      let m = irModules[sourceModule]!
      try m.description.write(to: irFile(productName), atomically: true, encoding: .utf8)
      return
    }
    let ir = LoweredProgram(syntax: program, modules: irModules)

    // C++

    if outputType == .cpp {
      let codeFormatter: CodeTransform? = (try? find("clang-format")).map({
        clangFormatter(URL(fileURLWithPath: $0))
      })

      let cxxModules = (
        core: program.cxx(program.coreLibrary!, withFormatter: codeFormatter),
        source: program.cxx(program[sourceModule], withFormatter: codeFormatter)
      )

      try write(cxxModules.core, to: coreLibCXXOutputBase, loggingTo: &errorLog)
      try write(cxxModules.source, to: sourceModuleCXXOutputBase(productName), loggingTo: &errorLog)
      return
    }

    // LLVM

    let llvmProgram = try LLVMProgram(ir, mainModule: sourceModule)

    if outputType == .llvm {
      let m = llvmProgram.llvmModules[sourceModule]!
      try m.description.write(to: llvmFile(productName), atomically: true, encoding: .utf8)
      return
    }

    // Executables

    assert(outputType == .binary)

    let objectFiles = try llvmProgram.write(
      .objectFile, to: FileManager.default.temporaryDirectory)
    let binaryPath = executableOutputPath(default: productName)

    #if os(macOS)
      try makeMacOSExecutable(at: binaryPath, linking: objectFiles, loggingTo: &errorLog)
    #elseif os(Linux)
      try makeLinuxExecutable(at: binaryPath, linking: objectFiles, loggingTo: &errorLog)
    #else
      _ = objectFiles
      _ = binaryPath
      fatalError("not implemented")
    #endif
  }

  /// Combines the object files located at `objects` into an executable file at `binaryPath`,
  /// logging diagnostics to `log`.
  private func makeMacOSExecutable<L: Log>(
    at binaryPath: String,
    linking objects: [URL],
    loggingTo log: inout L
  ) throws {
    let xcrun = try find("xcrun")
    let sdk =
      try runCommandLine(xcrun, ["--sdk", "macosx", "--show-sdk-path"], loggingTo: &log) ?? ""

    var arguments = [
      "-r", "ld", "-o", binaryPath,
      "-L\(sdk)/usr/lib",
      "-lSystem", "-lc++",
    ]
    arguments.append(contentsOf: objects.map(\.path))
    try runCommandLine(xcrun, arguments, loggingTo: &log)
  }

  /// Combines the object files located at `objects` into an executable file at `binaryPath`,
  /// logging diagnostics to `log`.
  private func makeLinuxExecutable<L: Log>(
    at binaryPath: String,
    linking objects: [URL],
    loggingTo log: inout L
  ) throws {
    var arguments = [
      "-o", binaryPath,
    ]
    arguments.append(contentsOf: objects.map(\.path))

    // Note: We use "clang" rather than "ld" so that to deal with the entry point of the program.
    // See https://stackoverflow.com/questions/51677440
    try runCommandLine(find("clang++"), arguments, loggingTo: &log)
  }

  /// Returns `self.outputURL` transformed as a suitable executable file path, using `productName`
  /// as a default name if `outputURL` is `nil`.
  ///
  /// The returned path has a `.exe` extension on Windows.
  private func executableOutputPath(default productName: String) -> String {
    var binaryPath = outputURL?.path ?? URL(fileURLWithPath: productName).path
    #if os(Windows)
      if !binaryPath.hasSuffix(".exe") { binaryPath += ".exe" }
    #endif
    return binaryPath
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

  /// Writes the code for `m` to `.h` and `.cpp` files having the given `basePath`, logging
  /// diagnostics to `log`.
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

  /// Given the desired name of the compiler's product, returns the file to write when "llvm" is
  /// selected as the output type.
  private func llvmFile(_ productName: String) -> URL {
    outputURL ?? URL(fileURLWithPath: productName + ".ll")
  }

  /// Given the desired name of the compiler's product, returns the file to write when "binary" is
  /// selected as the output type.
  private func binaryFile(_ productName: String) -> URL {
    outputURL ?? URL(fileURLWithPath: productName)
  }

  /// The base path (sans extension) of the `.cpp` and `.h` files representing the core library
  /// when "cpp" is selected as the output type.
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
