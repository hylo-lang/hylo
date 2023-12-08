import Foundation
import Utils

extension Pipe {

  /// Returns the contents decoded as UTF-8, while consuming `self`.
  public func readUTF8() -> String {
    String(decoding: fileHandleForReading.readDataToEndOfFile(), as: UTF8.self)
  }

}

extension Process {

  /// The textual output of a process run.
  public typealias OutputText = (standardOutput: Lazy<String>, standardError: Lazy<String>)

  /// The results of a process run that exited with a nonzero code.
  public struct NonzeroExit: Error {

    /// The nonzero exit code of the process run.
    public let terminationStatus: Int32

    /// The contents of the standard output stream.
    public let standardOutput: Lazy<String>

    /// The contents of the standard error stream.
    public let standardError: Lazy<String>

    /// The name of the executable ran by the process.
    public let executable: String

    /// The arguments passed to executable ran by the process.
    public let arguments: [String]

  }

  /// Runs `executable` with the given command line `arguments` and returns the text written to its
  /// standard output and standard error streams, throwing `NonzeroExit` if the command fails.
  public static func run(_ executable: URL, arguments: [String] = []) throws -> OutputText {
    let p = Process()
    let pipes = (standardOutput: Pipe(), standardError: Pipe())
    p.executableURL = executable
    p.arguments = arguments
    p.standardOutput = pipes.standardOutput
    p.standardError = pipes.standardError
    try p.run()
    p.waitUntilExit()

    let r: OutputText = (
      Lazy { pipes.standardOutput.readUTF8() },
      Lazy { pipes.standardError.readUTF8() }
    )

    if p.terminationStatus != 0 {
      throw NonzeroExit(
        terminationStatus: p.terminationStatus,
        standardOutput: r.standardOutput,
        standardError: r.standardError,
        executable: executable.fileSystemPath,
        arguments: arguments)
    }

    return r
  }

}

extension Process.NonzeroExit: CustomStringConvertible {

  public var description: String {
    """
    '\(executable)' failed with status \(terminationStatus)
    arguments: \(list: arguments)
    standard output:
    \(standardOutput[])
    standard error:
    \(standardError[])
    """
  }

}

extension URL {

  /// The representation used by the native filesystem.
  public var fileSystemPath: String {
    self.withUnsafeFileSystemRepresentation { String(cString: $0!) }
  }

}
