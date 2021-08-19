import OrderedCollections

/// A data structure providing information about the symbols being captured by an expression.
public struct CaptureTable {

  /// A key in a capture table.
  public struct Key: Hashable {

    /// The declaration being captured.
    public let capturedDecl: ValueDecl

    public func hash(into hasher: inout Hasher) {
      hasher.combine(ObjectIdentifier(capturedDecl))
    }

    public static func == (lhs: Key, rhs: Key) -> Bool {
      return lhs.capturedDecl === rhs.capturedDecl
    }

  }

  /// An entry in a capture table.
  public struct Value {

    /// The declaration to which captured identifiers refer in the context of the expression.
    ///
    /// If the capture is explicit, the property refers to the `CaptureDecl` that's defined in the
    /// the capture list. Otherwise, it refers directly to an external declaration.
    public let referredDecl: ValueDecl

    /// The contextual type of the capture, i.e., the type of a reference to the captured
    /// declaration in the context of the expression.
    public let type: ValType

    /// A list of declaration references associated with the capture.
    public var refs: [DeclRefExpr] = []

    /// The semantics of the capture.
    public var semantics: CaptureDecl.Semantics {
      if let capture = referredDecl as? CaptureDecl {
        return capture.semantics
      } else {
        return .val
      }
    }

  }

  /// The internal storage of the table.
  private var storage: OrderedDictionary<Key, Value>

  public init() {
    storage = [:]
  }

  /// A Boolean value indicating whether the table is empty.
  public var isEmpty: Bool { storage.isEmpty }

  /// The number of entries in the table.
  public var count: Int { storage.count }

  /// A list with all the captured declarations, ordered by insertion into the table.
  public var captures: OrderedSet<Key> { storage.keys }

  /// Appends a reference to a captured declaration, inserting that declaration in the table if
  /// necessary.
  public mutating func append(ref expr: DeclRefExpr) {
    let key = Key(capturedDecl: expr.decl)
    storage[key, default: Value(referredDecl: expr.decl, type: expr.type)].refs.append(expr)
  }

  /// Accesses the list of captured references on the given value declaration.
  public subscript(decl: ValueDecl) -> Value? {
    get {
      return storage[Key(capturedDecl: decl)]
    }
    set {
      storage[Key(capturedDecl: decl)] = newValue
    }
    _modify {
      yield &storage[Key(capturedDecl: decl)]
    }
  }

}

extension CaptureTable: Sequence {

  public func makeIterator() -> OrderedDictionary<Key, Value>.Iterator {
    return storage.makeIterator()
  }

}
