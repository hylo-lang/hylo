/// A skolem (a.k.a. rigid) type variable.
public struct SkolemType: TypeProtocol, Hashable {

  /// The type of the skolem before skolemization.
  public let base: Type

  public let flags: TypeFlags = [.isCanonical]

  public init(base: Type) {
    self.base = base
  }

  /// The next type variable identifier.
  private static var nextID = 0

}

extension SkolemType: CustomStringConvertible {

  public var description: String { "$\(base)" }

}

