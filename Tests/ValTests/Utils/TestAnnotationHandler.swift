/// A type representing a callback that handles test annotations.
protocol TestAnnotationHandler {

  /// Handles the given annotation.
  mutating func handle(_ annotation: TestAnnotation)

}
