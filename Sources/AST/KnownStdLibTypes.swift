/// The set of standard library types that the compiler knows.
public enum KnownStdLibTypes: String {

  case ExpressibleByBuiltinIntLiteral
  case Int

  public var name: String { rawValue }

}
