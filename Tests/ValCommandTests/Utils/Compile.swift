import ValCommand
import XCTest

/// Compiles `input` with the given arguments and returns the compiler's exit status, the URL of
/// the output file, and the contents of the standard error.
func compile(
  _ input: URL,
  with arguments: [String]
) throws -> CompilationResult {
  // Create a temporary directory to write the output file.
  let outputDirectory = try FileManager.default.url(
    for: .itemReplacementDirectory,
    in: .userDomainMask,
    appropriateFor: input,
    create: true)
  let output = outputDirectory.appendingPathComponent("a.out")

  // Parse the command line's arguments.
  let cli = try ValCommand.parse(arguments + ["-o", output.relativePath, input.relativePath])

  // Execute the command.
  var stderr = ""
  let status = try cli.execute(loggingTo: &stderr)
  return CompilationResult(status: status, output: output, stderr: stderr)
}
