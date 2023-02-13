/// A formatter for the C++ code.
public protocol CXXCodeFormatter {

  /// Return formatted code.
  func format(_ code: String) -> String

}
