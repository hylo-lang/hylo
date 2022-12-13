/// A wrapper type signaling that the wrapped value is incidental for the purpose of hashing and
/// equality comparison.
@propertyWrapper
public struct Incidental<T>: Hashable {

  /// The incidental, wrapped value.
  public var wrappedValue: T

  public func hash(into hasher: inout Hasher) {}

  public static func == (l: Self, r: Self) -> Bool { true }

  public init(wrappedValue: T) { self.wrappedValue = wrappedValue }
}

extension Incidental where T == String {
  public static func ~= (l: String, r: Self) -> Bool {
    l ~= r.wrappedValue
  }
}
