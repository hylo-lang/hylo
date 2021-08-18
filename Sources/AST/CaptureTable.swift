import OrderedCollections

/// A data structure providing information about the symbols being captured by an expression.
public struct CaptureTable {

  /// A key in a capture table.
  public struct Key: Hashable {

    public let decl: ValueDecl

    public func hash(into hasher: inout Hasher) {
      hasher.combine(ObjectIdentifier(decl))
    }

    public static func == (lhs: Key, rhs: Key) -> Bool {
      return lhs.decl === rhs.decl
    }

  }

  /// The internal storage of the table.
  private var storage: OrderedDictionary<Key, [DeclRefExpr]>

  public init() {
    storage = [:]
  }

  /// A list with all the captured declarations, ordered by insertion into the table.
  public var captures: OrderedSet<Key> { storage.keys }

  /// Appends a reference to a captured declaration, inserting that declaration in the table if
  /// necessary.
  public mutating func append(ref expr: DeclRefExpr) {
    storage[Key(decl: expr.decl), default: []].append(expr)
  }

  /// Accesses the list of captured references on the given value declaration.
  public subscript(decl: ValueDecl) -> [DeclRefExpr]? {
    get {
      return storage[Key(decl: decl)]
    }

    set {
      if let refs = newValue, !refs.isEmpty {
        storage[Key(decl: decl)] = refs
      } else {
        storage[Key(decl: decl)] = nil
      }
    }

    _modify {
      let key = Key(decl: decl)
      yield &storage[key]
      if storage[key]?.isEmpty ?? false {
        storage[key] = nil
      }
    }
  }

}

extension CaptureTable: Sequence {

  public func makeIterator() -> OrderedDictionary<Key, [DeclRefExpr]>.Iterator {
    return storage.makeIterator()
  }

}
