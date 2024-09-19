import Utils

/// A set of access effects.
public struct AccessEffectSet: OptionSet, Hashable {

  public typealias RawValue = UInt8

  public typealias Element = AccessEffect

  public var rawValue: UInt8

  public init(rawValue: UInt8) {
    self.rawValue = rawValue
  }

  /// Returns `true` if `self` contains `member`.
  public func contains(_ member: AccessEffect) -> Bool {
    (rawValue & member.rawValue) == member.rawValue
  }

  /// `true` if `self` is a singleton.
  public var isSingleton: Bool {
    (rawValue != 0) && (rawValue & (rawValue - 1) == 0)
  }

  /// The unique element in `self` if `self` is a singleton, or `nil` otherwise.
  public var uniqueElement: AccessEffect? {
    .init(rawValue: rawValue)
  }

  /// The weakest capability in `self`, or `nil` if `self` is empty.
  public var weakest: AccessEffect? {
    elements.first
  }

  /// Returns the strongest capability in `self` including `k`.
  public func strongest(including k: AccessEffect) -> AccessEffect {
    elements.reduce(k, max)
  }

  /// Returns a set containing the effects in `self` that are stronger or equal to `k`.
  public func filter(strongerOrEqualTo k: AccessEffect) -> AccessEffectSet {
    .init(rawValue: rawValue & ~(k.rawValue - 1))
  }

  @discardableResult
  public mutating func insert(
    _ newMember: AccessEffect
  ) -> (inserted: Bool, memberAfterInsert: AccessEffect) {
    if contains(newMember) {
      return (inserted: false, memberAfterInsert: newMember)
    } else {
      rawValue |= newMember.rawValue
      return (inserted: true, memberAfterInsert: newMember)
    }
  }

  @discardableResult
  public mutating func remove(_ member: AccessEffect) -> AccessEffect? {
    if contains(member) {
      rawValue &= ~member.rawValue
      return member
    } else {
      return nil
    }
  }

  public mutating func update(with newMember: AccessEffect) -> AccessEffect? {
    let (i, k) = insert(newMember)
    return i ? nil : k
  }

  /// A set with `set` and `inout`.
  public static let setOrInout: Self = [.set, .inout]

  /// A set with `let` and `sink`.
  public static let letOrSink: Self = [.let, .sink]

  /// A set with all access effects but `yielded`.
  public static let all: Self = [.let, .set, .inout, .sink]

  /// Returns an instance containing all possible capabilities that can be requested on the
  /// receiver of a bundle, used for in-place mutation iff `m` is `true`.
  public static func forUseOfBundle(performingInPlaceMutation m: Bool) -> Self {
    m ? .setOrInout : .letOrSink
  }

}

extension AccessEffectSet {

  /// A collection with the elements of an access effect set.
  public struct Elements: Collection {

    public typealias Index = UInt8

    public typealias Element = AccessEffect

    /// The contents of the set being iterated over.
    private let base: AccessEffectSet

    /// Creates an iterator over `s`.
    public init(_ s: AccessEffectSet) {
      self.base = s
    }

    public var isEmpty: Bool {
      base.rawValue == 0
    }

    public var startIndex: UInt8 {
      isEmpty ? endIndex : (base.rawValue & (~base.rawValue + 1))
    }

    public var endIndex: UInt8 {
      1 << 7
    }

    public func index(after position: UInt8) -> UInt8 {
      var next = position
      repeat {
        next = next << 1
      } while (next != endIndex) && ((next & base.rawValue) != next)
      return next
    }

    public subscript(position: UInt8) -> AccessEffect {
      AccessEffect(rawValue: position)!
    }

  }

  /// The elements contained in `self`.
  public var elements: Elements {
    .init(self)
  }

}

extension AccessEffectSet: CustomStringConvertible {

  public var description: String {
    "[\(list: elements)]"
  }

}
