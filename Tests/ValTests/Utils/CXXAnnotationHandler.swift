import CodeGenCXX
import Core
import XCTest

/// A handler that processes annotations in C++ transpilation tests.
struct CXXAnnotationHandler: TestAnnotationHandler {

  struct Configuration {

    /// The transpiled C++ module, if any.
    let module: CXXModule?

    /// Indicates whether the test case ran through completion without any error.
    let ranToCompletion: Bool

    /// The diagnostics reported throughout compilation.
    let diagnostics: [Diagnostic]

  }

  /// The C++ header.
  private let cxxHeader: String

  /// The C++ source.
  private let cxxSource: String

  /// The recorded issues.
  private var issues: [XCTIssue] = []

  /// The default handler for generic annotations.
  private var defaultHandler: DefaultTestAnnotationHandler

  /// Creates an instance with the C++ module.
  init(_ configuration: Configuration) {
    self.cxxHeader = configuration.module?.emitHeader() ?? ""
    self.cxxSource = configuration.module?.emitSource() ?? ""
    self.defaultHandler = DefaultTestAnnotationHandler(
      .init(ranToCompletion: configuration.ranToCompletion, diagnostics: configuration.diagnostics))
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
    issues.append(
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

  func finalize() -> [XCTIssue] {
    return issues
  }

}
