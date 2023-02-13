/// A formatter that doesn't change the given C++ code.
public struct IdentityFormatter: CXXCodeFormatter {

  /// Creates the object.
  public init() {}

  /// Return formatted code.
  public func format(_ code: String) -> String {
    code
  }

}
