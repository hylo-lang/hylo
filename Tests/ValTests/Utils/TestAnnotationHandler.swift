import XCTest

/// An object capable of processing test annotations.
protocol TestAnnotationHandler {

  /// The values necessary to initialize an instance of `Self`.
  associatedtype Configuration

  /// Creates an instance with the given configuration.
  init(_ configuration: Configuration)

  /// Handles the given annotations.
  mutating func handle(_ annotation: TestAnnotation)

  /// Returns the issues recorded.
  func issues() -> [XCTIssue]

}

extension TestAnnotationHandler {

  static func make(_ configuration: Configuration) -> ([TestAnnotation]) -> [XCTIssue] {
    return { annotations in
      var me = Self(configuration)
      for a in annotations {
        me.handle(a)
      }
      return me.issues()
    }
  }
}
