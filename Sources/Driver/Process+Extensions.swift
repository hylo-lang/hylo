import Foundation

extension Pipe {

  /// Returns the contents decoded as UTF-8, while consuming `self`.
  public func readUTF8() -> String {
    String(decoding: fileHandleForReading.readDataToEndOfFile(), as: UTF8.self)
  }

}

extension Process {

  /// The textual output of a process run.
  public typealias OutputStreams = (standardOutput: Pipe, standardError: Pipe)

  /// The results of a process run that exited with a nonzero code.
  public class NonZeroExit: Error {

    /// The nonzero exit code of the process run.
    public let terminationStatus: Int32

    /// The textual output of the process run.
    public let output: OutputStreams

    /// The command-line that triggered the process run.
    public let commandLine: [String]

    /// A cache that captures the output streams the first time they are read, so that their
    /// contents can be used repeatedly.
    private var outputMemo: (standardOutput: String, standardError: String)? = nil

    /// Creates an instance with the given properties.
    init(terminationStatus: Int32, output: OutputStreams, commandLine: [String]) {
      self.terminationStatus = terminationStatus
      self.output = output
      self.commandLine = commandLine
    }

  }

  /// Runs `executable` with the given command line `arguments` and returns its exit status along
  /// with the text written to its standard output and standard error streams.
  public static func run( _ executable: URL, arguments: [String] = []) throws -> OutputStreams {
    let output: OutputStreams = (standardOutput: Pipe(), standardError: Pipe())
    let p = Process()
    p.executableURL = executable
    p.arguments = arguments
    p.standardOutput = output.standardOutput
    p.standardError = output.standardError
    try p.run()
    p.waitUntilExit()

    if p.terminationStatus != 0 {
      throw NonZeroExit(
        terminationStatus: p.terminationStatus,
        output: output,
        commandLine: [executable.fileSystemPath] + arguments)
    }

    return output
  }

}

extension Process.NonZeroExit: CustomStringConvertible {

  public var description: String {
    if outputMemo == nil {
      outputMemo = (output.standardOutput.readUTF8(), output.standardError.readUTF8())
    }

    return """
      NonZeroExit(
        terminationStatus: \(terminationStatus),
        standardOutput: \(String(reflecting: outputMemo!.standardOutput)),
        standardError: \(String(reflecting: outputMemo!.standardError)),
        commandLine: \(commandLine))
      """
  }

}

extension URL {

  /// The representation used by the native filesystem.
  public var fileSystemPath: String {
    self.withUnsafeFileSystemRepresentation { String(cString: $0!) }
  }

}
