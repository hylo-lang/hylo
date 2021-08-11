/// A type that witnesses a conformance to `Hashable`.
public protocol HashWitness {

  associatedtype Value

  /// Hashes the essential components of the specified value by feeding them into the given hasher.
  ///
  /// - Parameters:
  ///   - value: A value.
  ///   - hahser: The hasher to use when combining the components of `value`.
  static func hash(_ value: Value, into hasher: inout Hasher)

  /// Returns whether two values are equal.
  ///
  /// - Parameters:
  ///   - lhs: A value.
  ///   - rhs: Another value.
  static func equals(_ lhs: Value, _ rhs: Value) -> Bool

}

/// A value wrapper exposing a hashable conformance.
///
/// You can use this wrapper to customize the behavior of the underlying type's conformance to
/// `Hashable`. For instance, you may want to change the equality operator used by Swift's `Set`
/// to implement a unique table.
public struct HashableBox<Value, Witness>: Hashable
where Witness: HashWitness, Witness.Value == Value
{

  /// Creates a box.
  ///
  /// - Parameter value: The wrapped value.
  public init(_ value: Value) {
    self.value = value
  }

  /// The wrapped value.
  public let value: Value

  public func hash(into hasher: inout Hasher) {
    Witness.hash(value, into: &hasher)
  }

  public static func == (lhs: HashableBox, rhs: HashableBox) -> Bool {
    return Witness.equals(lhs.value, rhs.value)
  }

}

/// A hash witness that uses reference identity to implement equality.
public struct ReferenceHashWitness<Value>: HashWitness where Value: AnyObject {

  public static func hash(_ value: Value, into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(value))
  }

  public static func equals(_ lhs: Value, _ rhs: Value) -> Bool {
    return lhs === rhs
  }

}

public typealias ReferenceBox<Value> = HashableBox<Value, ReferenceHashWitness<Value>>
where Value: AnyObject
