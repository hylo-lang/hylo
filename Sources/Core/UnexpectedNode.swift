import Utils

/// Reports an unexpected node kind, causing a fatal error, marking this execution path as unreachable.
public func unexpected<ID: NodeIDProtocol>(
  _ entity: String,
  found: TypedNode<ID>,
  file: StaticString = #file, line: UInt = #line
) -> Never {
  let (l, c) = found.site.first().lineAndColumn()
  let location = "\(found.site.file.url):\(l):\(c)"
  fatalError(
    "unexpected \(entity), found \(found.kind.description) at location: \(location)",
    file: file, line: line)
}

/// Reports an unexpected node kind, causing a fatal error, marking this execution path as unreachable.
public func unexpected<ID: NodeIDProtocol>(
  _ entity: String,
  found: ID, of nodes: AST,
  file: StaticString = #file, line: UInt = #line
) -> Never {
  let (l, c) = nodes[found].site.first().lineAndColumn()
  let location = "\(nodes[found].site.file.url):\(l):\(c)"
  fatalError(
    "unexpected \(entity), found \(found.kind.description) at location: \(location)",
    file: file, line: line)
}
