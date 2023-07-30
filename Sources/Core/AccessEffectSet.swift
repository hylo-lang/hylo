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

  /// The unique element in `self` if `self` is a singleton. Otherwise, `nil`.
  public var uniqueElement: AccessEffect? {
    .init(rawValue: rawValue)
  }

  /// The weakest capability in `self`, or `nil` if `self` is empty.
  public var weakest: AccessEffect? {
    var s = elements
    return s.next()
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
    insert(newMember).memberAfterInsert
  }

}

extension AccessEffectSet {

  /// A sequence with the elements of an access effect set.
  public struct Elements: Sequence, IteratorProtocol {

    /// The contents of the set being iterated over.
    private let base: AccessEffectSet

    /// The next effect returned by the iterator.
    private var position: UInt8

    /// Creates an iterator over `s`.
    public init(_ s: AccessEffectSet) {
      self.base = s
      self.position = s.rawValue & (~s.rawValue + 1)
      if self.position == 0 {
        self.position = 1 << 7
      }
    }

    /// Returns the next element in the sequence.
    public mutating func next() -> AccessEffect? {
      if position == (1 << 7) { return nil }
      defer {
        repeat {
          position = position << 1
        } while (position != (1 << 7)) && ((position & base.rawValue) != position)
      }
      return .init(rawValue: position)
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
