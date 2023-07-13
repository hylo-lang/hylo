import Utils

/// An unambiguous textual description of a type, scope, or declaration.
public enum Symbol: Hashable {

  /// A declaration or lexical scope.
  case node(AnyNodeID)

  /// A canonical type.
  case type(AnyType)

}
