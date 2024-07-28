import Foundation
import FrontEnd
import Utils

private let standardLibrarySourceFolder = URL(
  fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("Sources")

#if SWIFT_PACKAGE // Check if SPM is being used
  /// The parent directory of the standard library sources directory.
  private let useBuiltinStandardLibrary = ProcessInfo.processInfo.environment["HYLO_USE_BUILTIN_STDLIB"] == "true"

  private let libraryRoot = if useBuiltinStandardLibrary {
    // This path points into the source tree rather than to some copy so that when diagnostics are
    // issued for the standard library, they point to the original source files and edits to those
    // source files actually fix the problems (see https://github.com/hylo-lang/hylo/issues/932).
    // If we ever change that, some other mechanism is needed to ensure that diagnostics refer to
    // the original source files.
    standardLibrarySourceFolder
  } else {
    Bundle.module.url(forResource: "Sources", withExtension: nil)!
  }
#else
  // Default to source location on SPM. TODO make this able to use exported resources
  private let libraryRoot = standardLibrarySourceFolder
#endif


/// The root of a directory hierarchy containing all the standard library sources.
private let hostedLibrarySourceRoot = libraryRoot

/// The root of a directory hierarchy containing the sources for the standard library's freestanding
/// core.
private let freestandingLibrarySourceRoot = hostedLibrarySourceRoot.appendingPathComponent("Core")

extension Utils.Host {

  /// An AST representing the whole standard library, conditionally compiled for targeting the host
  /// platform.
  public static let hostedLibraryAST = Result {
    try AST(
      libraryRoot: hostedLibrarySourceRoot,
      ConditionalCompilationFactors(freestanding: false))
  }

  /// An AST representing the freestanding core of standard library, conditionally compiled for
  /// targeting the host platform.
  public static let freestandingLibraryAST = Result {
    try AST(
      libraryRoot: freestandingLibrarySourceRoot,
      ConditionalCompilationFactors(freestanding: true))
  }

}
