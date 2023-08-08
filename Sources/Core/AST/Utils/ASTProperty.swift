/// A mapping from AST node to `Property` value.
public typealias ASTProperty<Property> = [AnyNodeID: Property]

extension Dictionary where Key == AnyNodeID {

  /// Accesses the `Property` this map associates with `n`.
  public subscript<T: NodeIDProtocol>(n: T) -> Value? {
    _read { yield self[AnyNodeID(n)] }
    _modify { yield &self[AnyNodeID(n)] }
  }

  /// Accesses the property this map associates with `n`, or `defaultProperty` if this map makes
  /// no such association.
  public subscript<T: NodeIDProtocol>(
    n: T, default defaultProperty: @autoclosure () -> Value
  ) -> Value {
    _read { yield self[AnyNodeID(n), default: defaultProperty()] }
    _modify { yield &self[AnyNodeID(n), default: defaultProperty()] }
  }

}
