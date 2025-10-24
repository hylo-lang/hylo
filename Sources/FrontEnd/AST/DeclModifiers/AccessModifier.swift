/// An access modifier.
public enum AccessModifier: Codable, Sendable {

  /// Denotes (the default) private declaration.
  case `private`

  /// Denotes a declaration public up to the module boundary.
  case `internal`

  /// Denotes a public declaration.
  case `public`

}
