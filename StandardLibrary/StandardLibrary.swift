import Core
import Foundation
import Utils

// This path points into the source tree rather than to some copy so that when diagnostics are
// issued for the standard library, they point to the original source files and edits to those
// source files actually fix the problems (see https://github.com/hylo-lang/hylo/issues/932).  If we
// ever change that, some other mechanism is needed to ensure that diagnostics refer to the original
// source files.
//
/// The parent directory of the standard library sources directory.
private let libraryRoot = URL(fileURLWithPath: #filePath).deletingLastPathComponent()

/// The root of a directory hierarchy containing all the standard library sources.
private let hostedLibrarySourceRoot = libraryRoot.appendingPathComponent("Sources")

/// The root of a directory hierarchy containing the sources for the standard library's freestanding
/// core.
private let freestandingLibrarySourceRoot = hostedLibrarySourceRoot.appendingPathComponent("Core")

extension Utils.Host {

  /// An AST representing the whole standard library, conditionally compiled for targeting the host
  /// platform.
  public static let hostedLibraryAST = Result {
    try AST(libraryRoot: hostedLibrarySourceRoot, for: CompilerConfiguration([]))
      .roundTripSerialized()
  }

  /// An AST representing the freestanding core of standard library, conditionally compiled for
  /// targeting the host platform.
  public static let freestandingLibraryAST = Result {
    try AST(
      libraryRoot: freestandingLibrarySourceRoot, for: CompilerConfiguration(["freestanding"])
    ).roundTripSerialized()
  }

}
