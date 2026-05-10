/// A place under some access.
public struct AccessedPlace {
  /// The location being accessed.
  let location: Memory.Place

  /// The capability granting access to `location`.
  let capability: Access
}
