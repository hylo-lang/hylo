/// A parameter passing convention.
public enum ParamConvention {

  /// Pass by immutable projection.
  case `let`

  /// Pass by assignable projection.
  case `set`

  /// Pass by mutable projection.
  case `inout`

  /// Pass by consumption.
  case sink

  /// Yielded.
  case yielded

}
