/// The result set of a name lookup.
public struct LookupResult_ {

  fileprivate struct ValueDeclBox: Hashable {

    let decl: ValueDecl

    func hash(into hasher: inout Hasher) { hasher.combine(ObjectIdentifier(decl)) }

    static func == (lhs: ValueDeclBox, rhs: ValueDeclBox) -> Bool { lhs.decl === rhs.decl }

  }

  fileprivate var _values: Set<ValueDeclBox>

  /// The first type declaration that was found.
  public private(set) var type: TypeDecl?

  /// Creates an empty result set.
  public init() {
    self._values = []
    self.type = nil
  }

  /// Indicates whether the set only contains overloadable declarations.
  public var allOverloadable: Bool {
    (type == nil) && (_values.isEmpty || _values.first!.decl.isOverloadable)
  }

  // Creates a set with the given value declarations.
  public init<S>(values: S) where S: Sequence, S.Element == ValueDecl {
    self._values = []
    self.type = nil
    values.forEach({ insert($0) })
  }

  /// Creates a singleton with the given type declaration.
  public init(type: TypeDecl) {
    self._values = []
    self.type = type
  }

  /// The value declarations that were found.
  public var values: [ValueDecl] { _values.map({ $0.decl }) }

  /// Inserts `decl` in the set if it it's not already present and if `decl` does not shadow a
  /// declaration already in the set.
  ///
  /// If the set is empty, then `decl` is always inserted. Otherwise, `decl` is inserted iff:
  /// - it is not already in the set,
  /// - all declarations in the set are overloadable, and
  /// - `decl` is overloadable.
  @discardableResult
  public mutating func insert(_ decl: TypeOrValueDecl) -> Bool {
    guard allOverloadable else { return false }
    assert(type == nil)

    if let decl = decl as? TypeDecl {
      type = decl
      return true
    } else if _values.isEmpty || decl.isOverloadable {
      return _values.insert(ValueDeclBox(decl: decl as! ValueDecl)).inserted
    } else {
      return false
    }
  }

  /// Returns a result set containing the elements that satisfy the given predicate.
  public func filter(_ isIncluded: (TypeOrValueDecl) -> Bool) -> LookupResult_ {
    var result = LookupResult_()
    if let type = self.type, isIncluded(type) {
      result.type = type
    } else {
      for value in values where isIncluded(value) {
        result.insert(value)
      }
    }
    return result
  }

}

extension LookupResult_: Collection {

  public typealias Element = TypeOrValueDecl

  public struct Index: Comparable {

    fileprivate var i: Set<ValueDeclBox>.Index?

    public static func < (lhs: Index, rhs: Index) -> Bool {
      guard let l = lhs.i else { return rhs.i != nil }
      guard let r = rhs.i else { return false }
      return l < r
    }

  }

  public var startIndex: Index {
    Index(i: type != nil ? nil : _values.startIndex)
  }

  public var endIndex: Index { Index(i: _values.endIndex) }

  public func index(after position: Index) -> Index {
    if let i = position.i {
      return Index(i: _values.index(after: i))
    } else {
      return Index(i: _values.startIndex)
    }
  }

  public subscript(position: Index) -> TypeOrValueDecl {
    if let i = position.i {
      return _values[i].decl
    } else if let d = type {
      return d
    } else {
      preconditionFailure("Index out of bounds")
    }
  }

}

extension LookupResult_: CustomStringConvertible {

  public var description: String { String(describing: Array(self)) }

}
