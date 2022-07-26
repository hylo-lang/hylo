import ArgumentParser
import Compiler
import Foundation

struct CLI: ParsableCommand {

  /// The type of the output files to generate.
  enum OutputType: ExpressibleByArgument {

    /// AST before type-checking.
    case rawAST

    /// Val IR.
    case ir

    /// LLVM IR.
    case llvmIR

    /// Executable binary.
    case binary

    init?(argument: String) {
      switch argument {
      case "raw-ast"  : self = .rawAST
      case "ir"       : self = .ir
      case "llvm-ir"  : self = .llvmIR
      case "binary"   : self = .binary
      default         : return nil
      }
    }

  }

  @Flag(
    name: [.customLong("nostdlib")],
    help: "Do not include the standard library.")
  var skipStandardLibrary: Bool = false

  @Flag(
    name: [.customLong("modules")],
    help: "Compile inputs as separate modules.")
  var compileInputAsModules: Bool = false

  @Option(
    name: [.customLong("emit")],
    help: "Emit the specified type output files.")
  var outputType: OutputType = .binary

  @Option(
    name: [.customShort("o")],
    help: "Write output to <o>.",
    transform: URL.init(fileURLWithPath:))
  var outputPath: URL?

  @Argument(
    transform: URL.init(fileURLWithPath:))
  var inputs: [URL]

  func run() throws {
    /// The AST of the program being compiled.
    var program = AST()

    if compileInputAsModules {
      fatalError("not implemented")
    } else {
      // Merge all inputs into the same same module.
      let module = program.insert(ModuleDecl(name: "Main"))
      for input in inputs {
        if input.hasDirectoryPath {
          if !withFiles(in: input, { insert(contentsOf: $0, into: module, in: &program) }) {
            CLI.exit(withError: ExitCode(-1))
          }
        } else {
          if !insert(contentsOf: input, into: module, in: &program) {
            CLI.exit(withError: ExitCode(-1))
          }
        }
      }
    }

    if outputType == .rawAST {
      let outputPath = outputPath ?? URL(fileURLWithPath: "ast.json")
      let encoder = JSONEncoder()
      try encoder.encode(program).write(to: outputPath)
      CLI.exit()
    }
  }

  /// Parses the contents of the file at `fileURL` and insert them into `ast[module]`.
  func insert(
    contentsOf fileURL: URL,
    into module: NodeID<ModuleDecl>,
    in ast: inout AST
  ) -> Bool {
    switch fileURL.pathExtension {
    case "val":
      // Parse the file.
      let sourceFile: SourceFile
      do {
        sourceFile = try SourceFile(contentsOf: fileURL)
      } catch {
        log(error: error.localizedDescription)
        return false
      }

      let (decls, diagnostics) = Parser.parse(sourceFile, into: module, in: &ast)

      // Report the parser's diagnostics.
      for diagnostic in diagnostics.sorted(by: Diagnostic.isLoggedBefore) {
        log(diagnostic: diagnostic)
      }

      // Bail out if the parser failed.
      return decls != nil

    default:
      log(warning: "ignoring file with unsupported extension: \(fileURL.relativePath)")
      return true
    }
  }

  /// Creates a module from the contents at `url` and adds it to the AST.
  ///
  /// - Requires: `url` must denote a directly.
  func addModule(url: URL) {
    fatalError("not implemented")
  }

  /// Calls `action` with the URL of the files in `directory` and its subdirectories.
  func withFiles(in directory: URL, _ action: (URL) throws -> Bool) rethrows -> Bool {
    let enumerator = FileManager.default.enumerator(
      at: directory,
      includingPropertiesForKeys: [.isRegularFileKey],
      options: [.skipsHiddenFiles, .skipsPackageDescendants])!
    for case let url as URL in enumerator {
      guard try action(url) else { return false }
    }
    return true
  }

  /// Logs `diagnostic` to the standard error.
  func log(diagnostic: Diagnostic) {
    // Log the level.
    switch diagnostic.level {
    case .warning:
      write("warning".styled([.bold, .yellow]))
    case .error:
      write("error".styled([.red, .yellow]))
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

  /// Logs `message` as a warning to the standard error file.
  func log(warning message: String, terminator: String = "\n") {
    write("warning".styled([.bold, .yellow]))
    write(": ")
    write(message)
    write(terminator)
  }

  /// Logs `message` as an error to the standard error file.
  func log(error message: String, terminator: String = "\n") {
    write("error".styled([.bold, .red]))
    write(": ")
    write(message)
    write(terminator)
  }

  /// Writes `text` to the standard error file.
  func write<S: StringProtocol>(_ text: S) {
    FileHandle.standardError.write(Data(text.utf8))
  }

}

// Run the program.
CLI.main()
