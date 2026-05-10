/// A place under some access.
public struct AccessedPlace {
  /// The location being accessed.
  public let location: Memory.Place

  /// The capability granting access to `location`.
  public let capability: Access
}
