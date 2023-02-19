import Utils

/// A type variable.
public struct TypeVariable: TypeProtocol {

  /// The identifier of the variable.
  public let id: Int

  public let flags: TypeFlags = [.isCanonical, .hasVariable]

  /// Creates a new type variable.
  public init() {
    defer { TypeVariable.nextID += 1 }
    self.id = TypeVariable.nextID
  }

  /// The next type variable identifier.
  private static var nextID = 0

}

extension TypeVariable: CustomStringConvertible {

  public var description: String { "_" }

}
