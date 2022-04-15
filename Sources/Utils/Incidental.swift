/// A wrapper type signaling that the wrapped value is incidental for the purpose of hashing and
/// equality comparison.
public struct Incidental<T>: Hashable {

  /// The incidental, wrapped value.
  public var value: T

  /// Creates a new wrapper around `value`.
  public init(_ value: T) {
    self.value = value
  }

  public func hash(into hasher: inout Hasher) {}

  public static func == (l: Self, r: Self) -> Bool { true }

}
