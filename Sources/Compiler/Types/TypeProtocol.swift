/// A protocol describing the API of a type's associated value.
public protocol TypeProtocol {

  /// A set of flags describing recursive properties.
  var flags: TypeFlags { get }

  /// Returns the canonical form of this type.
  func canonical() -> Type

  /// Returns a textual description of this type.
  func describe(in ast: AST) -> String

}

extension TypeProtocol {

  public func describe(in ast: AST) -> String { "\(type(of: self))" }

  /// Returns whether the specified flags are raised on this type.
  public subscript(flags fs: TypeFlags) -> Bool { flags.contains(fs) }

}
