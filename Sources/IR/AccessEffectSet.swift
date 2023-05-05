import Core
import Utils

/// A set of access effects.
public struct AccessEffectSet: OptionSet {

  public typealias RawValue = UInt8

  public typealias Element = AccessEffect

  public var rawValue: UInt8

  public init(rawValue: UInt8) {
    self.rawValue = rawValue
  }

  public func contains(_ member: AccessEffect) -> Bool {
    (rawValue & member.rawValue) == member.rawValue
  }

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
