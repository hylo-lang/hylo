/// Stops the program, indicating that source program uses a feature that has not yet been
/// implemented.
public func UNIMPLEMENTED(
  _ message: @autoclosure () -> String = "not implemented",
  file: StaticString = #filePath,
  line: UInt = #line
) -> Never {
  fatalError(message(), file: file, line: line)
}
