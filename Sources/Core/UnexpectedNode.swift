/// Causes a fatal error reporting an unexpected `n`.
public func unexpected<ID: NodeIDProtocol>(
  _ n: TypedNode<ID>,
  file: StaticString = #file, line: UInt = #line
) -> Never {
  fatalError("unexpected \(n.kind) at location: \(n.site.first())", file: file, line: line)
}

/// Causes a fatal error reporting an unexpected `n`, where `n` is a node in `nodes`.
public func unexpected<ID: NodeIDProtocol>(
  _ n: ID, in nodes: AST,
  file: StaticString = #file, line: UInt = #line
) -> Never {
  fatalError("unexpected \(n.kind) at location: \(nodes[n].site.first())", file: file, line: line)
}
