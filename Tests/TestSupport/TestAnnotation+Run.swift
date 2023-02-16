import XCTest

extension TestAnnotation {

  //! Run `executable` with `arguments`, and fails if the exit code isn't zero.
  public func run(_ executable: URL, with arguments: [String] = []) -> XCTIssue? {
    // Run the executable.
    let task = Process()
    task.executableURL = executable
    if (try? task.run()) == nil {
      return self.failure("Cannot execute: \(executable)")
    }
    task.waitUntilExit()

    // Check the termination status of the executed process.
    return task.terminationStatus == 0
      ? nil
      : self.failure("Execution failed with exit code \(task.terminationStatus)")
  }

}
