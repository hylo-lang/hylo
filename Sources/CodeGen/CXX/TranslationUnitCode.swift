/// The C++ code for a .h/.cpp file pair.
///
/// By convention, for each C++ translation unit we generate, we create a header file containing
/// the interface of the translation unit declarations.
public struct TranslationUnitCode {

  /// The code that belongs to a C++ header file.
  public let headerCode: String
  /// The code that belongs to a C++ source file.
  public let sourceCode: String

}
