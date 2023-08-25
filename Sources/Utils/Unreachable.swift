/// Marks this execution path as unreachable, causing a fatal error otherwise.
public func unreachable(
  _ message: @autoclosure () -> String = "",
  file: StaticString = #filePath,
  line: UInt = #line
) -> Never {
  fatalError(message(), file: file, line: line)
}

/// Returns `lhs!` if `lhs` is non-`nil`; evaluates `rhs()`, which never returns, otherwise.
public func ?? <T>(lhs: T?, rhs: @autoclosure () -> Never) -> T {
  if let lhs = lhs {
    return lhs
  } else {
    rhs()
  }
}
