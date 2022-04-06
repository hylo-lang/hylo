/// A parameter passing convention.
public enum ParamConvention {

  /// Pass by immutable projection.
  case byval

  /// Pass by mutable projection.
  case `inout`

  /// Pass by consumption.
  case sink

}
