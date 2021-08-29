/// A factory for unique identifiers.
public protocol IdentifierFactory {

  associatedtype ID

  /// Creates a unique identifier.
  mutating func makeID() -> ID

}

/// An identifier factory that generates auto incremented integers.
public struct AutoIncrementFactory: IdentifierFactory {

  public typealias ID = Int

  /// The next generated ID.
  private var nextID: Int

  /// Creates an identifier factory.
  ///
  /// - Parameter start: The value of the first generated index.
  public init(start: Int = 0) {
    self.nextID = start
  }

  public mutating func makeID() -> Int {
    let newID = nextID
    nextID += 1
    return newID
  }

}
