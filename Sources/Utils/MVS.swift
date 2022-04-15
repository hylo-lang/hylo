/// Returns the result of calling `action` with a mutable projection of `value`.
public func modifying<T, U>(_ value: inout T, _ action: (inout T) throws -> U) rethrows -> U {
  try action(&value)
}
