/// A map from AST nodes.
public struct PropertyMap<Property> {

  private var storage: [ObjectIdentifier: Property]

  public init() { storage = [:] }

  public init<S>(uniqueNodesWithProperties nodesAndProperties: S)
  where S: Sequence, S.Element == (node: Node, property: Property)
  {
    storage = Dictionary(
      uniqueKeysWithValues: nodesAndProperties.map({ (ObjectIdentifier($0), $1) }))
  }

  public subscript(node: Node) -> Property? {
    _read   { yield storage[ObjectIdentifier(node)] }
    _modify { yield &storage[ObjectIdentifier(node)] }
    set     { storage[ObjectIdentifier(node)] = newValue }
  }

  public subscript(node: Node, default property: @autoclosure () -> Property) -> Property {
    _read   { yield storage[ObjectIdentifier(node), default: property()] }
    _modify { yield &storage[ObjectIdentifier(node), default: property()] }
  }

}

extension PropertyMap: ExpressibleByDictionaryLiteral {

  public init(dictionaryLiteral elements: (Node, Property)...) {
    self.init(uniqueNodesWithProperties: elements)
  }

}

extension PropertyMap: Collection {

  public typealias Index = Dictionary<ObjectIdentifier, Property>.Index

  public typealias Element = Dictionary<ObjectIdentifier, Property>.Element

  public var isEmpty: Bool { storage.isEmpty }

  public var count: Int { storage.count }

  public var underestimatedCount: Int { storage.underestimatedCount }

  public var startIndex: Index { storage.startIndex }

  public var endIndex: Index { storage.endIndex }

  public func index(after i: Index) -> Index { storage.index(after: i) }

  public subscript(position: Index) -> Element { storage[position] }

}
