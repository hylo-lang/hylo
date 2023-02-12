import Core
import Utils
import XCTest

extension Diagnostic {

  /// A test annotation that announces `self` should be expected.
  var expectation: TestAnnotation {
    TestAnnotation(
      in: site.file.url,
      atLine: site.first().lineAndColumn().line,
      parsing: "diagnostic " + message
    )
  }

}

extension XCTestCase {

  /// The effects of running the `processAndCheck` parameter to `checkAnnotatedValFiles`.
  fileprivate typealias ProcessingEffects = (
    /// Whether the processing completed without Val errors.
    ranToCompletion: Bool,
    /// Test failures generated by processing.
    testFailures: [XCTIssue],
    /// Val diagnostics generated by processing.
    diagnostics: DiagnosticSet
  )

  /// Applies `process` to each ".val" file in `sourceDirectory` and reports XCTest failures where
  /// the effects of processing don't match the file's diagnostic annotation commands ("diagnostic",
  /// "expect-failure", and "expect-success").
  ///
  /// - Parameter `testCaseDirectory`: where the test files are searched.
  /// - Parameter `process`: applies some compilation phases to `file`, updating `diagnostics`
  ///   with any generated diagnostics. Throws an `Error` if any phases failed.
  func checkAnnotatedValFileDiagnostics(
    in testCaseDirectory: URL,
    _ process: (_ file: SourceFile, _ diagnostics: inout DiagnosticSet) throws -> Void
  ) throws {
    try checkAnnotatedValFiles(
      in: testCaseDirectory, checkingAnnotationCommands: [],
      { (file, annotationsToHandle, diagnostics) in
        assert(annotationsToHandle.isEmpty)
        try process(file, &diagnostics)
        return []
      }
    )
  }

  /// Applies `processAndCheck` to each ".val" file in `testCaseDirectory` along with the subset of
  /// that file's annotations whose commands match `checkedCommands`, and reports resulting XCTest
  /// failures, along with any additional failures where the effects of processing don't match the
  /// file's diagnostic annotation commands ("diagnostic", "expect-failure", and "expect-success").
  ///
  /// - Parameters:
  ///   - `testCaseDirectory`: where the test files are searched.
  ///   - `checkedCommands`: the annnotation commands to be validated by `processAndCheck`.
  ///   - `processAndCheck`: applies some compilation phases to `file`, updating `diagnostics`
  ///     with any generated diagnostics, then checks `annotationsToCheck` against the results,
  ///     returning corresponding test failures. Throws an `Error` if any phases failed.
  public func checkAnnotatedValFiles(
    in testCaseDirectory: URL,
    checkingAnnotationCommands checkedCommands: Set<String> = [],

    _ processAndCheck: (
      _ file: SourceFile,
      _ annotationsToCheck: ArraySlice<TestAnnotation>,
      _ diagnostics: inout DiagnosticSet
    ) throws -> [XCTIssue]
  ) throws {
    for file in try sourceFiles(in: [testCaseDirectory]) {
      var annotations = TestAnnotation.parseAll(from: file)

      // Separate the annotations to be checked by default diagnostic annotation checking from
      // those to be checked by `processAndCheck`.
      let p = annotations.partition(by: { checkedCommands.contains($0.command) })
      let (diagnosticAnnotations, processingAnnotations) = (annotations[..<p], annotations[p...])

      var diagnostics = DiagnosticSet()
      let failures = XCTContext.runActivity(
        named: file.baseName,
        block: { activity in
          let completedProcessingTestFailures = try? processAndCheck(
            file, processingAnnotations, &diagnostics)

          return failuresToReport(
            effectsOfProcessing: (
              ranToCompletion: completedProcessingTestFailures != nil,
              testFailures: completedProcessingTestFailures ?? [],
              diagnostics: diagnostics
            ),
            unhandledAnnotations: diagnosticAnnotations)
        })

      for f in failures {
        record(f)
      }
    }
  }

  /// Given the effects of processing and the annotations not specifically handled by
  /// `processAndCheck` above, returns the final set of test failures to be reported to XCTest.
  fileprivate func failuresToReport(
    effectsOfProcessing processing: ProcessingEffects,
    unhandledAnnotations: ArraySlice<TestAnnotation>
  ) -> [XCTIssue] {
    var testFailures = processing.testFailures

    // If two diagnostics get two diagnostics that would be matched by the same expectation, we'll
    // throw one of them. We can adjust the code to deal with that if it comes up.
    var diagnosticsByExpectation = Dictionary(
      zip(processing.diagnostics.elements.lazy.map(\.expectation), processing.diagnostics.elements),
      uniquingKeysWith: { (a, _) in a })

    func fail(_ expectation: TestAnnotation, _ message: String) {
      testFailures.append(expectation.failure(message))
    }

    for a in unhandledAnnotations {
      switch a.command {
      case "diagnostic":
        if let i = diagnosticsByExpectation.index(forKey: a) {
          diagnosticsByExpectation.remove(at: i)
        } else {
          fail(a, "missing expected diagnostic\(a.argument.map({": '\($0)'"}) ?? "")")
        }
      case "expect-failure":
        if processing.ranToCompletion {
          fail(a, "processing succeeded, but failure was expected")
        }
      case "expect-success":
        if !processing.ranToCompletion {
          fail(a, "processing failed, but success was expected")
        }
      default:
        fail(a, "unexpected test command: '\(a.command)'")
      }
    }

    testFailures += diagnosticsByExpectation.values.lazy.map {
      XCTIssue(.error("unexpected diagnostic: '\($0.message)'", at: $0.site, notes: $0.notes))
    }
    return testFailures
  }

}
