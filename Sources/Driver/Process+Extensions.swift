import Foundation
import Utils
import Dispatch

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

    /// The executable run by the process.
    public let executable: URL

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

    #if !os(Linux)

    try p.run()
    p.waitUntilExit()

    #else

    // Workaround https://github.com/swiftlang/swift-corelibs-foundation/issues/5197
    let done = DispatchSemaphore(value: 0)
    p.terminationHandler = { _ in done.signal() }
    try p.run()
    done.wait()

    #endif

    let r: OutputText = (
      Lazy { pipes.standardOutput.readUTF8() },
      Lazy { pipes.standardError.readUTF8() }
    )

    if p.terminationStatus != 0 {
      throw NonzeroExit(
        terminationStatus: p.terminationStatus,
        standardOutput: r.standardOutput,
        standardError: r.standardError,
        executable: executable,
        arguments: arguments)
    }

    return r
  }

}

extension Process.NonzeroExit: CustomStringConvertible, CustomDebugStringConvertible {

  public var description: String {
    """
    Process.NonzeroExit (status: \(terminationStatus))
    Command line: \(([executable.fileSystemPath] + arguments).map(String.init(reflecting:)).joined(separator: " "))

      standard output:
      -------------
    \(standardOutput[])
      -------------

      standard error:
      -------------
    \(standardError[])
      -------------
    """
  }

  public var debugDescription: String {
    """
    Process.NonzeroExit(
      terminationStatus: \(terminationStatus),
      standardOutput: \(String(reflecting: standardOutput[])),
      standardError: \(String(reflecting: standardError[])),
      executable: \(String(reflecting: executable)),
      arguments: \(String(reflecting: arguments)))
    """
  }

}

extension URL {

  /// The representation used by the native filesystem.
  public var fileSystemPath: String {
    self.withUnsafeFileSystemRepresentation { String(cString: $0!) }
  }

}
