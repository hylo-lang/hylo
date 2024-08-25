/// A value computed at compile-time.
public enum CompileTimeValue: Hashable {

  /// A type.
  case type(AnyType)

  /// A term.
  case term(AnyTerm)

  /// The payload of `.type`.
  public var asType: AnyType? {
    if case .type(let v) = self { v } else { nil }
  }

  /// The payload of `.term`.
  public var asTerm: AnyTerm? {
    if case .term(let v) = self { v } else { nil }
  }

  /// `true` iff self is in canonical form.
  public var isCanonical: Bool {
    if let t = asType { t.isCanonical } else { true }
  }

  /// `true` if `self` is a `TypeVariable`.
  public var isTypeVariable: Bool {
    self.asType?.base is TypeVariable
  }

  /// The payload of `.compilerKnown` as an instance of `T`.
  public func asCompilerKnown<T>(_: T.Type) -> T? {
    ConcreteTerm(self.asTerm)?.value as? T
  }

}

extension CompileTimeValue: CustomStringConvertible {

  public var description: String {
    switch self {
    case .type(let v):
      return "\(v)"
    case .term(let v):
      return "\(v)"
    }
  }

}
