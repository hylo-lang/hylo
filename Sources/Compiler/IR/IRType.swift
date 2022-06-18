/// The lowered (static) type of an entity.
public enum IRType: Hashable {

  /// The type of an owned object.
  case owned(Type)

  /// The type of a projected object.
  case projected(Type)

  /// The type of an instruction that does not produce any value.
  case void

}
