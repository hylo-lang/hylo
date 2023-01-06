import Core
import Utils
import XCTest

extension Diagnostic {
  /// A test annotation that announces `self` should be expected.
  var expectation: TestAnnotation {
    return TestAnnotation(
      in: location?.source.url ?? URL(string: "nowhere://at/all")!,
      atLine: location?.first().lineAndColumnIndices.line ?? 1,
      parsing: "diagnostic " + message
    )
  }
}

extension XCTestCase {

  /// The effects of running the `processAndCheck` parameter to `checkAnnotatedValFiles`.
  fileprivate typealias ProcessingEffects = (
    ranToCompletion: Bool, testFailures: [XCTIssue], diagnostics: DiagnosticLog
  )

  /// Applies `process` to each ".val" file in `sourceDirectory` and reports XCTest failures where
  /// the effects of processing don't match the file's diagnostic annotation commands ("diagnostic",
  /// "expect-failure", and "expect-success").
  ///
  /// - Parameter `sourceDirectory`: a path relative to the ValTests/ directory of this project.
  /// - Parameter `process`: applies some compilation phases to `source`, updating `diagnostics`
  ///   with any generated diagnostics. Throws an `Error` if any phases failed.
  func checkAnnotatedValFileDiagnostics(
    in sourceDirectory: String,
    _ process: (_ source: SourceFile, _ diagnostics: inout DiagnosticLog) throws -> Void
  ) throws {
    try checkAnnotatedValFiles(
      in: sourceDirectory, checkingAnnotationCommands: [],
      { (source, annotationsToHandle, diagnostics) in
        assert(annotationsToHandle.isEmpty)
        try process(source, &diagnostics)
        return []
      }
    )
  }

  /// Applies `processAndCheck` to each ".val" file in `sourceDirectory` along with the subset of
  /// that file's annotations whose commands match `checkedCommands`, and reports resulting XCTest
  /// failures, along with any additional failures where the effects of processing don't match the
  /// file's diagnostic annotation commands ("diagnostic", "expect-failure", and "expect-success").
  ///
  /// - Parameters:
  ///   - `sourceDirectory`: a path relative to the ValTests/ directory of this project.
  ///   - `checkedCommands`: the annnotation commands to be validated by `processAndCheck`.
  ///   - `processAndCheck`: applies some compilation phases to `source`, updating `diagnostics`
  ///     with any generated diagnostics, then checks `annotationsToCheck` against the results,
  ///     returning corresponding test failures. Throws an `Error` if any phases failed.
  func checkAnnotatedValFiles(
    in sourceDirectory: String,
    checkingAnnotationCommands checkedCommands: Set<String> = [],
    _ processAndCheck: (
      _ source: SourceFile,
      _ annotationsToCheck: ArraySlice<TestAnnotation>,
      _ diagnostics: inout DiagnosticLog
    ) throws -> [XCTIssue]
  ) throws {
    let testCaseDirectory = try XCTUnwrap(
      Bundle.module.url(forResource: sourceDirectory, withExtension: nil),
      "No test cases")

    try withFiles(
      in: testCaseDirectory,
      { (url) in
        // Skip non-val files.
        if url.pathExtension != "val" { return true }

        let source = try SourceFile(contentsOf: url)
        var annotations = TestAnnotation.parseAll(from: source)

        // Separate the annotations to be checked by default diagnostic annotation checking from
        // those to be checked by `processAndCheck`.
        let p = annotations.partition(by: { checkedCommands.contains($0.command) })
        let (diagnosticAnnotations, processingAnnotations) = (annotations[..<p], annotations[p...])

        var diagnostics = DiagnosticLog()
        let failures = XCTContext.runActivity(
          named: source.baseName,
          block: { activity in
            let completedProcessingTestFailures = try? processAndCheck(
              source, processingAnnotations, &diagnostics)

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

        // Move to the next test case.
        return true
      })
  }

  /// Given the effects of processing and the annotations not specifically handled by
  /// `processAndCheck` above, returns the final set of test failures to be reported to XCTest.
  fileprivate func failuresToReport(
    effectsOfProcessing processing: ProcessingEffects,
    unhandledAnnotations: ArraySlice<TestAnnotation>
  ) -> [XCTIssue] {
    var testFailures = processing.testFailures

    // If this traps due to non-uniqueness of keys, we have two diagnostics that would be matched by
    // the same expectation.  We can adjust the code to deal with that if it comes up.
    var diagnosticsByExpectation = Dictionary(
      uniqueKeysWithValues:
        zip(processing.diagnostics.lazy.map(\.expectation), processing.diagnostics))

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
      XCTIssue(
        Diagnostic(
          level: .error, message: "unexpected diagnostic: '\($0.message)'",
          location: $0.location,
          window: $0.window,
          children: $0.children))
    }
    return testFailures
  }
}
