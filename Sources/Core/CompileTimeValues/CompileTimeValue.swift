/// A value computed at compile-time.
public enum CompileTimeValue: Hashable {

  /// A type.
  case type(AnyType)

  /// An instance of a type known by the compiler (e.g. `Int`).
  case compilerKnown(AnyHashable)

  /// The payload of `.type`.
  public var asType: AnyType? {
    if case .type(let t) = self {
      return t
    } else {
      return nil
    }
  }

  /// `true` if `self` is a `TypeVariable`.
  public var isTypeVariable: Bool {
    asType?.base is TypeVariable
  }

}

extension CompileTimeValue: CustomStringConvertible {

  public var description: String {
    switch self {
    case .type(let v):
      return "\(v)"
    case .compilerKnown(let v):
      return "\(v)"
    }
  }

}
