import CodeGenCXX
import Core
import XCTest

/// A handler that processes annotations in C++ transpilation tests.
struct CXXAnnotationHandler: TestAnnotationHandler {
  /// The C++ header.
  private let cxxHeader: String

  /// The C++ source.
  private let cxxSource: String

  /// The recorded issues.
  private var issues_: [XCTIssue] = []

  /// The default handler for generic annotations.
  private var defaultHandler: DefaultTestAnnotationHandler

  /// Creates an instance with the C++ module.
  init(
    module: CXXModule?,
    ranToCompletion: Bool,
    diagnostics: [Diagnostic])
  {
    self.cxxHeader = module?.emitHeader() ?? ""
    self.cxxSource = module?.emitSource() ?? ""
    self.defaultHandler = .init(ranToCompletion: ranToCompletion, diagnostics: diagnostics)
  }

  mutating func handle(_ annotation: TestAnnotation) {
    switch annotation.command {
    case "cpp":
      let code = annotation.argument!.removingTrailingNewlines()
      check(cxxSource, contains: code, at: annotation.location)

    case "h":
      let code = annotation.argument!.removingTrailingNewlines()
      check(cxxHeader, contains: code, at: annotation.location)

    default:
      defaultHandler.handle(annotation)
    }
  }

  private mutating func check(
    _ haystack: String,
    contains needle: Substring,
    at location: XCTSourceCodeLocation
  ) {
    if haystack.contains(needle) { return }
    issues_.append(
      XCTIssue(
        type: .assertionFailure,
        compactDescription: """
          transpiled code not found:
          \(needle)
          --- not found in ---
          \(haystack)
          """,
        sourceCodeContext: .init(location: location)))
  }

  func issues() -> [XCTIssue] {
    return issues_
  }

}
