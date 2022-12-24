import XCTest

/// An object capable of processing test annotations.
protocol TestAnnotationHandler {

  /// The values necessary to initialize an instance of `Self`.
  associatedtype Configuration

  /// Creates an instance with the given configuration.
  init(_ configuration: Configuration)

  /// Handles the given annotations.
  mutating func handle(_ annotation: TestAnnotation)

  /// Finalizes this instance and returns issues it recorded.
  func finalize() -> [XCTIssue]

}

extension TestAnnotationHandler {

  /// Handles the given annotations.
  mutating func handle<S: Sequence>(_ annotations: S) where S.Element == TestAnnotation {
    for a in annotations {
      handle(a)
    }
  }

}
