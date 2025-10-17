/// An unordered collection of unique elements using a custom hash witness.
public struct CustomWitnessedSet<Witness: HashableWitness> {

  /// The type of the elements in the set.
  public typealias Element = Witness.Element

  /// An element wrapped into a hashable box.
  fileprivate typealias _Element = HashableBox<Witness>

  /// The contents of `self`.
  fileprivate var contents: Set<_Element>

  /// Creates an instance with the elements in `members`.
  public init<S: Sequence>(_ members: S) where S.Element == Element {
    contents = []
    contents.reserveCapacity(members.underestimatedCount)
    for m in members { contents.insert(_Element(m)) }
  }

  /// Reserves enough space to store the specified number of elements.
  public mutating func reserve(minimumCapacity: Int) {
    contents.reserveCapacity(minimumCapacity)
  }

  /// Removes and returns an element from `self`.
  public mutating func popFirst() -> Element? {
    contents.popFirst()?.base
  }

}

extension CustomWitnessedSet: Sendable where Witness.Element: Sendable, Witness: Sendable {}

extension CustomWitnessedSet: Collection {

  public struct Index: Comparable {

    fileprivate var base: Set<_Element>.Index

    fileprivate init(_ base: Set<_Element>.Index) {
      self.base = base
    }

    public static func < (l: Self, r: Self) -> Bool { l.base < r.base }

  }

  public var isEmpty: Bool { contents.isEmpty }

  public var count: Int { contents.count }

  public var startIndex: Index { Index(contents.startIndex) }

  public var endIndex: Index { Index(contents.endIndex) }

  public func index(after i: Index) -> Index { Index(contents.index(after: i.base)) }

  public subscript(position: Index) -> Element {
    contents[position.base].base
  }

  public func contains(_ member: Element) -> Bool {
    contents.contains(_Element(member))
  }

}

extension CustomWitnessedSet: SetAlgebra {

  public init() {
    contents = []
  }

  @discardableResult
  public mutating func insert(
    _ newMember: Element
  ) -> (inserted: Bool, memberAfterInsert: Element) {
    let (inserted, memberAfterInsert) = contents.insert(_Element(newMember))
    return (inserted, memberAfterInsert.base)
  }

  public mutating func update(with newMember: Element) -> Element? {
    contents.update(with: _Element(newMember))?.base
  }

  public mutating func remove(_ member: Element) -> Element? {
    contents.remove(_Element(member))?.base
  }

  public func union(_ other: Self) -> Self {
    var result = self
    result.contents.formUnion(other.contents)
    return result
  }

  /// Inserts the elements in `members` into `self`.
  public mutating func formUnion<S: Sequence>(_ members: S) where S.Element == Element {
    for m in members { contents.insert(_Element(m)) }
  }

  public mutating func formUnion(_ other: Self) {
    contents.formUnion(other.contents)
  }

  public func intersection(_ other: Self) -> Self {
    var result = self
    result.contents.formIntersection(other.contents)
    return result
  }

  public mutating func formIntersection(_ other: Self) {
    contents.formIntersection(other.contents)
  }

  public func symmetricDifference(_ other: Self) -> Self {
    var result = self
    result.contents.formIntersection(other.contents)
    return result
  }

  public mutating func formSymmetricDifference(_ other: Self) {
    contents.formSymmetricDifference(other.contents)
  }

  public func subtracting(_ other: Self) -> Self {
    var result = self
    result.contents.subtract(other.contents)
    return result
  }

  public mutating func subtract(_ other: Self) {
    contents.subtract(other.contents)
  }

}

extension CustomWitnessedSet: Hashable {}

extension CustomWitnessedSet: ExpressibleByArrayLiteral {

  public init(arrayLiteral members: Element...) {
    self.init(members)
  }

}

extension CustomWitnessedSet: CustomStringConvertible {

  public var description: String { String(describing: contents) }

}
