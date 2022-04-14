import Utils

/// A protocol describing the API of a type's associated value.
public protocol TypeProtocol {

  /// A set of flags describing recursive properties.
  var flags: TypeFlags { get }

}

extension TypeProtocol {

  /// Returns whether the specified flags are raised on this type.
  public subscript(fs: TypeFlags) -> Bool { flags.contains(fs) }

}
