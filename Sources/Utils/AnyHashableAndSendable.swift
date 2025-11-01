/// A type-erased wrapper for hashable values that are safe to send across concurrency domains.
public struct AnyHashableAndSendable: @unchecked Sendable, Hashable, Equatable {
    // Implementation note: the stored value is proven to be Sendable since it can only be
    // constructed from a concrete type that conforms to Sendable.

    /// The type-erased hashable value.
    public let anyHashable: AnyHashable

    /// Boxes the given object of a concrete type conforming to `Hashable` and `Sendable`.
    public init(wrapping w: some Hashable & Sendable) {
        self.anyHashable = .init(w)
    }

    /// Equality based on the underlying values' equality.
    public static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.anyHashable == rhs.anyHashable
    }

    /// Hashes the underlying value into the given hasher.
    public func hash(into hasher: inout Hasher) {
        anyHashable.hash(into: &hasher)
    }
}

extension AnyHashableAndSendable: CustomStringConvertible {
    public var description: String {
        return "AnyHashableAndSendable(\(anyHashable.base))"
    }
}
