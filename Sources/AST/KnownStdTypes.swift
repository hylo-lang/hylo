/// The set of standard library types that the compiler knows.
public enum KnownStdTypes: String {

  case Bool
  case Copyable
  case ExpressibleByBuiltinIntLiteral
  case Int
  case Maybe
  case Nil

  public var name: String { rawValue }

}
