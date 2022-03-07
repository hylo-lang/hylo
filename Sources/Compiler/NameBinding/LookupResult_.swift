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

  /// Indicates whether the set only contains overloadable declarations.
  public private(set) var allOverloadable: Bool

  /// Creates an empty result set.
  public init() {
    self._values = []
    self.type = nil
    self.allOverloadable = true
  }

  /// Creates a singleton with the given type declaration.
  public init(type: TypeDecl) {
    self._values = []
    self.type = type
    self.allOverloadable = false
  }

  /// The value declarations that were found.
  public var values: [ValueDecl] { _values.map({ $0.decl }) }

  /// Inserts `decl` in the set if it it's not already present and if the set set does not already
  /// contain a non-overloadable declaration.
  @discardableResult
  public mutating func insert(_ decl: TypeOrValueDecl) -> Bool {
    guard allOverloadable else { return false }

    if let decl = decl as? TypeDecl {
      allOverloadable = false
      type = decl
      return true
    } else {
      allOverloadable = decl.isOverloadable
      return _values.insert(ValueDeclBox(decl: decl as! ValueDecl)).inserted
    }
  }

  /// Merges the elements of `other` into this set.
  public mutating func merge(_ other: LookupResult_) {
    if isEmpty {
      self = other
    } else if allOverloadable {
      allOverloadable = other.allOverloadable
      type = other.type
      _values.formUnion(other._values)
    }
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
