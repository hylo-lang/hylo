/// The linkage of an IR symbol.
public enum Linkage: Sendable {

  /// The denoted entity can be referred from any module.
  case external

  /// The denoted entity can only be referred to inside its own module.
  case module

}
