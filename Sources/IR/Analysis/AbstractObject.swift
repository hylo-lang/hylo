/// An object in an abstract interpreter.
struct AbstractObject<Domain: AbstractDomain>: Equatable {

  /// The abstract layout of the object.
  let layout: AbstractTypeLayout

  /// The value of the object.
  var value: Value

  /// Creates an instance with the given properties.
  init(layout: AbstractTypeLayout, value: Value) {
    self.layout = layout
    self.value = value.canonical
  }

  /// Returns the result of calling `action` with the sub-object at given `offset`.
  ///
  /// - Requires: `i` is a valid index in `layout`.
  mutating func withSubobject<T>(_ offset: Int, _ action: (inout AbstractObject) -> T) -> T {
    precondition(layout.properties.count > 0)

    var parts: [Value]
    if case .partial(let p) = value {
      parts = p
    } else {
      parts = Array(repeating: value, count: layout.properties.count)
    }

    var o = AbstractObject(layout: layout[offset], value: parts[offset])
    defer {
      parts[offset] = o.value
      value = .partial(parts).canonical
    }
    return action(&o)
  }

  /// Returns the result of calling `action` with the sub-object at given `path`.
  ///
  /// - Requires: `offsets` is a valid path in `self`.
  mutating func withSubobject<T, Path: Collection<Int>>(
    at path: Path,
    _ action: (inout AbstractObject) -> T
  ) -> T {
    if let (i, t) = path.headAndTail {
      return withSubobject(i, { $0.withSubobject(at: t, action) })
    } else {
      defer { value = value.canonical }
      return action(&self)
    }
  }

  /// Returns `l` merged with `r`.
  static func && (l: AbstractObject, r: AbstractObject) -> AbstractObject {
    precondition(l.layout == r.layout)
    return AbstractObject(layout: l.layout, value: l.value && r.value)
  }

  /// The value of an abstract object.
  enum Value: Equatable {

    /// An object whose parts all have the same value.
    case full(Domain)

    /// An object whose parts may have different values.
    ///
    /// - Requires: The payload is not empty.
    case partial([Value])

    /// The canonical form of `self`.
    var canonical: Value {
      switch self {
      case .full:
        return self

      case .partial(var subobjects):
        var isUniform = true
        subobjects[0] = subobjects[0].canonical
        for i in 1 ..< subobjects.count {
          subobjects[i] = subobjects[i].canonical
          isUniform = isUniform && subobjects[i] == subobjects[0]
        }
        return isUniform ? subobjects[0] : .partial(subobjects)
      }
    }

    /// Returns `lhs` merged with `rhs`.
    static func && (lhs: Value, rhs: Value) -> Value {
      switch (lhs.canonical, rhs.canonical) {
      case (.full(let lhs), .full(let rhs)):
        return .full(lhs && rhs)

      case (.partial(let lhs), .partial(let rhs)):
        precondition(lhs.count == rhs.count)
        return .partial(zip(lhs, rhs).map(&&))

      case (.partial(let lhs), _):
        return .partial(lhs.map({ $0 && rhs }))

      case (_, .partial(let rhs)):
        return .partial(rhs.map({ lhs && $0 }))
      }
    }

  }

}

extension AbstractObject: CustomStringConvertible {

  var description: String { "\(layout.type)(\(value))" }

}

extension AbstractObject.Value: CustomStringConvertible {

  var description: String {
    switch self {
    case .full(let s):
      return String(describing: s)
    case .partial(let s):
      return "{\(list: s, joinedBy: ", ")}"
    }
  }

}
