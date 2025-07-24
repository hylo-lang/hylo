public struct AnyHashableAndSendable: @unchecked Sendable, Hashable, Equatable {
    public let anyHashable: AnyHashable

    public init(wrapping w: some Hashable & Sendable) {
        self.anyHashable = .init(w)
    }

    public static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.anyHashable == rhs.anyHashable
    }

    public func hash(into hasher: inout Hasher) {
        anyHashable.hash(into: &hasher)
    }
}

extension AnyHashableAndSendable: CustomStringConvertible {
    public var description: String {
        return "AnyHashableAndSendable(\(anyHashable.base))"
    }
}
