import Core
import Utils
import XCTest

/// An object that processes the test annotations of a Val test.
struct DefaultTestAnnotationHandler: TestAnnotationHandler {

  /// Indicates whether the test case ran through completion without any error.
  private let testCaseRanToCompletion: Bool

  /// A table mapping a line location to the diagnostics recorded at that location.
  private var recordedDiagnostics: [XCTSourceCodeLocation?: [Diagnostic]]

  /// The recorded issues.
  private var issues_: [XCTIssue] = []

  /// Creates a new instance with the given information about the test case.
  init(ranToCompletion: Bool, diagnostics: [Diagnostic]) {
    self.testCaseRanToCompletion = ranToCompletion
    self.recordedDiagnostics = diagnostics.reduce(
      into: [:],
      { (ds, d) in
        ds[d.location.map(XCTSourceCodeLocation.init(_:)), default: []].append(d)
      })
  }

  /// Handles the given annotation.
  mutating func handle(_ annotation: TestAnnotation) {
    switch annotation.command {
    case "diagnostic":
      handle(diagnostic: annotation)
    case "expect-failure":
      handle(expectFailure: annotation)
    case "expect-success":
      handle(expectSuccess: annotation)
    default:
      handle(unexpected: annotation)
    }
  }

  /// Handles `diagnostic`.
  private mutating func handle(diagnostic annotation: TestAnnotation) {
    modifying(
      &recordedDiagnostics[annotation.location, default: []],
      { (expected) in
        if let i = expected.firstIndex(where: { $0.message == annotation.argument }) {
          expected.remove(at: i)
        } else {
          let message = annotation.argument ?? "_"
          issues_.append(
            XCTIssue(
              type: .assertionFailure,
              compactDescription: "missing diagnostic: \(message)",
              sourceCodeContext: .init(location: annotation.location)))
        }
      })
  }

  /// Handles `expect-failure`.
  private mutating func handle(expectFailure annotation: TestAnnotation) {
    if !testCaseRanToCompletion { return }
    issues_.append(
      XCTIssue(
        type: .assertionFailure,
        compactDescription: "type checking succeeded, but expected failure",
        sourceCodeContext: .init(location: annotation.location)))
  }

  /// Handles `expect-success`.
  private mutating func handle(expectSuccess annotation: TestAnnotation) {
    if testCaseRanToCompletion { return }
    issues_.append(
      XCTIssue(
        type: .assertionFailure,
        compactDescription: "type checking failed, but expected success",
        sourceCodeContext: .init(location: annotation.location)))
  }

  /// Handles an unexpected test annotation.
  private mutating func handle(unexpected annotation: TestAnnotation) {
    issues_.append(
      XCTIssue(
        type: .assertionFailure,
        compactDescription: "unexpected test command: '\(annotation.command)'",
        sourceCodeContext: .init(location: annotation.location)))
  }

  func issues() -> [XCTIssue] {
    var allIssues = issues_
    for d in recordedDiagnostics.values {
      allIssues.append(contentsOf: d.map(XCTIssue.init(_:)))
    }
    return allIssues
  }

}
