import ArgumentParser
import Core
import Foundation

public struct Driver: ParsableCommand {

  /// The type of the output files to generate.
  enum OutputType: String, ExpressibleByArgument {

    /// AST before type-checking.
    case rawAST = "raw-ast"

    /// Hylo IR before mandatory transformations.
    case rawIR = "raw-ir"

    /// Hylo IR.
    case ir = "ir"

    /// LLVM IR
    case llvm = "llvm"

    /// Intel ASM
    case intelAsm = "intel-asm"

    /// Executable binary.
    case binary = "binary"
  }

  public static let configuration = CommandConfiguration(commandName: "hc")

  @Flag(
    name: [.customLong("modules")],
    help: "Compile inputs as separate modules.")
  var compileInputAsModules: Bool = false

  @Flag(
    name: [.customLong("import-builtin")],
    help: "Import the built-in module.")
  var importBuiltinModule: Bool = false

  @Flag(
    name: [.customLong("freestanding")],
    help:
      "Import only the freestanding core of the standard library, omitting any definitions that depend on having an operating system."
  )
  var freestanding: Bool = false

  @Flag(
    name: [.customLong("experimental-parallel-typechecking")],
    help: "Parallelize the type checker")
  var experimentalParallelTypeChecking: Bool = false

  @Flag(
    name: [.customLong("typecheck")],
    help: "Type-check the input file(s).")
  var typeCheckOnly: Bool = false

  @Option(
    name: [.customLong("trace-inference")],
    help: ArgumentHelp(
      "Enable tracing of type inference requests at the given line.",
      valueName: "file:line"))
  var inferenceTracingSite: SourceLine?

  @Option(
    name: [.customLong("emit")],
    help: ArgumentHelp(
      "Emit the specified type output files. From: raw-ast, raw-ir, ir, llvm, intel-asm, binary",
      valueName: "output-type"))
  var outputType: OutputType = .binary

  @Option(
    name: [.customLong("transform")],
    help: ArgumentHelp(
      "Apply the specify transformations after IR lowering.",
      valueName: "transforms"))
  var transforms: ModulePassList?

  @Option(
    name: [.customShort("L")],
    help: ArgumentHelp(
      "Add a directory to the library search path.",
      valueName: "directory"))
  var librarySearchPaths: [String] = []

  @Option(
    name: [.customShort("l")],
    help: ArgumentHelp(
      "Link the generated module(s) to the specified native library.",
      valueName: "name"))
  var libraries: [String] = []

  @Option(
    name: [.customShort("o")],
    help: ArgumentHelp("Write output to <file>.", valueName: "file"),
    transform: URL.init(fileURLWithPath:))
  var outputURL: URL?

  @Flag(
    name: [.short, .long],
    help: "Use verbose output.")
  var verbose: Bool = false

  @Flag(
    name: [.customShort("V"), .long],
    help: "Output the compiler version.")
  var version: Bool = false

  @Flag(
    name: [.customShort("O")],
    help: "Compile with optimizations.")
  var optimize: Bool = false

  @Argument(
    transform: URL.init(fileURLWithPath:))
  var inputs: [URL] = []

  public init() {}

  /// The URL of the current working directory.
  private var currentDirectory: URL {
    URL(fileURLWithPath: FileManager.default.currentDirectoryPath, isDirectory: true)
  }

  public func run() throws {
    do {
      let (exitCode, diagnostics) = try execute()
      diagnostics.render(
        into: &standardError, style: ProcessInfo.ansiTerminalIsConnected ? .styled : .unstyled)
      Driver.exit(withError: exitCode)
    } catch let e {
      print("Unexpected error\n")
      Driver.exit(withError: e)
    }
  }

  /// Executes the command, returning its exit status and any generated diagnostics.
  ///
  /// Propagates any thrown errors that are not Hylo diagnostics,
  public func execute() throws -> (ExitCode, DiagnosticSet) {
    var diagnostics = DiagnosticSet()
    do {
      let args = Execution(args: self)
      try args.executeCommand(diagnostics: &diagnostics)
    } catch let d as DiagnosticSet {
      assert(d.containsError, "Diagnostics containing no errors were thrown")
      diagnostics.formUnion(d)
      return (ExitCode.failure, diagnostics)
    }
    return (ExitCode.success, diagnostics)
  }
}
