/// An unambiguous textual description of a type, scope, or declaration known to the compiler.
enum ReservedSymbol: UInt8 {

  /// The `Val` module.
  case val

  /// `Val.Bool`.
  case bool

  /// `Val.Int`.
  case int

  /// `Val.Float64`.
  case float64

  /// `Val.String`.
  case string

  /// The `Any` type.
  case any

  /// The `Never` type.
  case never

  /// The `Void` type.
  case void

}

extension ReservedSymbol: TextOutputStreamable {

  public func write<T: TextOutputStream>(to output: inout T) {
    output.write(Base64Digit(rawValue: rawValue)!.description)
  }

}
