import CBORCoding
import Core
import Foundation
import Utils

/// An error thrown when a resource couldn't be found on the host.
private struct ResourceNotFound: Error {

  /// An identifier for the resource that wasn't found.
  let resource: String

}

/// Returns the URL of the resource with given name and extension in the bundle associated with the
/// current Swift module.
private func resource(_ name: String, withExtension ext: String) throws -> URL {
  guard let u = Bundle.module.url(forResource: name, withExtension: ext) else {
    throw ResourceNotFound(resource: "\(name).\(ext)")
  }
  return u
}

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

  /// Returns an AST representing the given standard library `variant` (either `"freestanding"` or
  /// `"hosted"`), conditionally compiled for targeting the host platform.
  private static func libraryAST(_ variant: String) -> Result<AST, Error> {
    Result {
      try CBORDecoder().forAST.decode(
        AST.self,
        from: Data(
          contentsOf: resource(variant, withExtension: "cbor"),
          options: .alwaysMapped))
    }
  }

  /// An AST representing the whole standard library, conditionally compiled for targeting the host
  /// platform.
  public static let hostedLibraryAST: Result<AST, Error> = libraryAST("hosted")

  /// An AST representing the freestanding core of standard library, conditionally compiled for
  /// targeting the host platform.
  public static let freestandingLibraryAST: Result<AST, Error> = libraryAST("freestanding")

}
