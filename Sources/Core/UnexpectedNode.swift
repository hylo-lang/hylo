import Utils

/// Reports an unexpected node kind, causing a fatal error, marking this execution path as unreachable.
public func unexpectedNode<ID: NodeIDProtocol>(
  _ found: TypedNode<ID>, whileExpecting expectedString: String,
  file: StaticString = #file, line: UInt = #line
) -> Never {
  let (l, c) = found.site.first().lineAndColumn()
  let location = "\(found.site.file.url):\(l):\(c)"
  fatalError(
    "found \(found.kind.description) but expected \(expectedString) at location: \(location)",
    file: file, line: line)
}
