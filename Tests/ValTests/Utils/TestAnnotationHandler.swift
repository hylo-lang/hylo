import XCTest

/// An object capable of processing test annotations.
protocol TestAnnotationHandler {

  /// Handles the given annotations.
  mutating func handle(_ annotation: TestAnnotation)

  /// Returns the issues recorded.
  func issues() -> [XCTIssue]

}

extension TestAnnotationHandler {

  /// Handles the given annotations.
  mutating func handles<S: Sequence>(_ annotations: S) -> [XCTIssue]
  where S.Element == TestAnnotation {
    for a in annotations {
      handle(a)
    }
    return issues()
  }

}
