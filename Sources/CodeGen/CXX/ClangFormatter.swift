import Foundation

/// Returns a C++ code formatter that invokes external `clang-format` to perform the job.
public func clangFormatter(_ executable: URL, style: String = "llvm") -> (
  (String) -> String
) {
  return { code in
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
