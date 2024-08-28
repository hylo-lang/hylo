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

  /// `true` iff self is in canonical form.
  public var isCanonical: Bool {
    if let t = asType { t[.isCanonical] } else { true }
  }

  /// `true` if `self` is a `TypeVariable`.
  public var isTypeVariable: Bool {
    asType?.base is TypeVariable
  }

  /// The payload of `.compilerKnown` as an instance of `T`.
  public func asCompilerKnown<T>(_: T.Type) -> T? {
    if case .compilerKnown(let v) = self {
      return v as? T
    } else {
      return nil
    }
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
