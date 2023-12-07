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

/// The root URL of Hylo's standard library.
private let standardLibrarySourceRoot = libraryRoot.appendingPathComponent("Sources")

/// The root URL of Hylo's core library.
private let freestandingLibrarySourceRoot = standardLibrarySourceRoot.appendingPathComponent("Core")

extension Utils.Host {

  /// An AST representing the whole standard library, conditionally compiled for targeting the host platform.
  public static let standardLibraryAST = AST(
    libraryRoot: standardLibrarySourceRoot,
    for: CompilerConfiguration([]))

  /// An AST representing the freestanding core of standard library, conditionally compiled for
  /// targeting the host platform.
  public static let freestandingLibraryAST = AST(
    libraryRoot: freestandingLibrarySourceRoot,
    for: CompilerConfiguration(["freestanding"]))

}
