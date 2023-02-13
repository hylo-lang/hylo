import Foundation

/// A C++ code formatter that invokes external `clang-format` to perform the job.
public struct ClangFormatFormatter: CXXCodeFormatter {

  /// The path to the `clang-format` executable.
  let executable: URL
  /// The format style to be used.
  let style: String

  /// Initializes the current object with the path to clang-format
  public init(_ executable: URL, style: String = "llvm") {
    self.executable = executable
    self.style = style
  }

  /// Return formatted code.
  public func format(_ code: String) -> String {
    let pipeIn = Pipe()
    let pipeOut = Pipe()
    // Run clang-format to format the C++ code.
    let process = Process()
    process.executableURL = executable
    process.arguments = ["--style=\(style)"]
    process.standardInput = pipeIn
    process.standardOutput = pipeOut
    do {
      try process.run()
      try pipeIn.fileHandleForWriting.write(contentsOf: Data(code.utf8))
      try pipeIn.fileHandleForWriting.close()
      process.waitUntilExit()
      return String(
        data: pipeOut.fileHandleForReading.readDataToEndOfFile(), encoding: String.Encoding.utf8)
        ?? code
    } catch {
      return code
    }
  }

}
