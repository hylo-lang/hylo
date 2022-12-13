import Utils

/// A type variable.
public struct TypeVariable: TypeProtocol {

  /// The identifier of the variable.
  public let id: Int

  /// The AST node associated with this type variable, if any.
  @Incidental public private(set) var node: AnyNodeID?

  public let flags: TypeFlags = [.isCanonical, .hasVariable]

  /// Creates a new type variable, optionally associated with the specified node.
  public init(node: AnyNodeID? = nil) {
    defer { TypeVariable.nextID += 1 }
    self.id = TypeVariable.nextID
    self.node = node
  }

  /// The next type variable identifier.
  private static var nextID = 0

}

extension TypeVariable: CustomStringConvertible {

  public var description: String { "%\(id)" }

}
