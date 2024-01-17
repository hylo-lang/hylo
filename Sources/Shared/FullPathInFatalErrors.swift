// This file should be symlinked into each target's source directory by its relative-path so that
// these names shadow the ones that come from the Swift standard library. See
// https://github.com/apple/swift/issues/66777.

/// Just like Swift.precondition, but includes the full file path in the diagnostic.
func precondition(
  _ condition: @autoclosure () -> Bool,
  _ message: @autoclosure () -> String = String(),
  file: StaticString = #filePath,
  line: UInt = #line
) {
  Swift.precondition(condition(), message(), file: (file), line: line)
}

/// Just like Swift.preconditionFailure, but includes the full file path in the diagnostic.
func preconditionFailure(
  _ message: @autoclosure () -> String = String(),
  file: StaticString = #filePath,
  line: UInt = #line
) -> Never {
  Swift.preconditionFailure(message(), file: (file), line: line)
}

/// Just like Swift.fatalError, but includes the full file path in the diagnostic.
func fatalError(
  _ message: @autoclosure () -> String = String(),
  file: StaticString = #filePath,
  line: UInt = #line
) -> Never {
  Swift.fatalError(message(), file: (file), line: line)
}

/// Just like Swift.assert, but includes the full file path in the diagnostic.
func assert(
  _ condition: @autoclosure () -> Bool,
  _ message: @autoclosure () -> String = String(),
  file: StaticString = #filePath,
  line: UInt = #line
) {
  Swift.assert(condition(), message(), file: (file), line: line)
}

/// Just like Swift.assertionFailure, but includes the full file path in the diagnostic.
func assertionFailure(
  _ message: @autoclosure () -> String = String(),
  file: StaticString = #filePath,
  line: UInt = #line
) {
  Swift.assertionFailure(message(), file: (file), line: line)
}
