import Utils

/// An unambiguous textual description of a type, scope, or declaration.
public enum DemangledSymbol {

  /// Creates an instance decoding the symbol mangled in `s`, returning `nil` if decoding failed.
  public init?(_ s: String) {
    guard let i = String(assemblySanitized: s) else { return nil }
    var m = Demangler()
    var x = i[...]

    if let value = m.demangle(from: &x) {
      self = value
    } else {
      return nil
    }
  }

  /// The declaration of an entity or bundle.
  case entity(DemangledEntity)

  /// A type.
  case type(DemangledType)

  /// The entity wrapped in `self` if its payload is `.entity`. Otherwise, `nil`.
  public var entity: DemangledEntity? {
    if case .entity(let e) = self {
      return e
    } else {
      return nil
    }
  }

}

extension DemangledSymbol: CustomStringConvertible {

  public var description: String {
    switch self {
    case .entity(let e):
      return e.description
    case .type(let t):
      return t.description
    }
  }

}
