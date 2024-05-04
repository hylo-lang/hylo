/// Stops the program, indicating that source program uses a feature that has not yet been
/// implemented.
public func UNIMPLEMENTED(
  _ message: @autoclosure () -> String = "not implemented",
  file: StaticString = #filePath,
  line: UInt = #line
) -> Never {
  fatalError(message(), file: file, line: line)
}

/// Causes a fatal error reporting an unexpected `n`, where `n` is a node in `nodes`.
public func unexpected<ID: NodeIDProtocol>(
  _ n: ID, in nodes: AST,
  file: StaticString = #filePath, line: UInt = #line
) -> Never {
  fatalError(
    "\n\(nodes[n].site.gnuStandardText): error: unexpected \(n.kind)", file: file, line: line)
}
