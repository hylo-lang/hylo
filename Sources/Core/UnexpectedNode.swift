/// Causes a fatal error reporting an unexpected `n`, where `n` is a node in `nodes`.
public func unexpected<ID: NodeIDProtocol>(
  _ n: ID, in nodes: AST,
  file: StaticString = #filePath, line: UInt = #line
) -> Never {
  fatalError("\n\(nodes[n].site.gnuStandardText): error: unexpected \(n.kind)", file: file, line: line)
}
