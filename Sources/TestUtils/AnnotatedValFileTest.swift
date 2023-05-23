import Core
import Utils
import XCTest

extension Diagnostic {

  /// A test annotation that announces `self` should be expected.
  fileprivate var expectation: TestAnnotation {
    TestAnnotation(
      in: site.file.url,
      atLine: site.first().line.number,
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

  /// Applies `processAndCheck` to `valToTest`, along with the subset of its annotations
  /// whose commands match `checkedCommands`, and reports resulting XCTest failures, along with any
  /// additional failures where the effects of processing don't match the its annotation
  /// commands ("diagnostic", "expect-failure", and "expect-success").
  ///
  /// - Parameters:
  ///   - checkedCommands: the annnotation commands to be validated by `processAndCheck`.
  ///   - processAndCheck: applies some compilation phases to `file`, updating `diagnostics`
  ///     with any generated diagnostics, then checks `annotationsToCheck` against the results,
  ///     returning corresponding test failures. Throws an `Error` if any phases failed.
  fileprivate func checkAnnotations(
    in valToTest: SourceFile,
    checkingAnnotationCommands checkedCommands: Set<String> = [],
    _ processAndCheck: (
      _ file: SourceFile,
      _ annotationsToCheck: ArraySlice<TestAnnotation>,
      _ diagnostics: inout DiagnosticSet
    ) throws -> [XCTIssue]
  ) throws {
    var annotations = TestAnnotation.parseAll(from: valToTest)

    // Separate the annotations to be checked by default diagnostic annotation checking from
    // those to be checked by `processAndCheck`.
    let p = annotations.partition(by: { checkedCommands.contains($0.command) })
    let (diagnosticAnnotations, processingAnnotations) = (annotations[..<p], annotations[p...])

    var diagnostics = DiagnosticSet()
    let failures = XCTContext.runActivity(
      named: valToTest.baseName,
      block: { activity in
        let completedProcessingTestFailures = try? processAndCheck(
          valToTest, processingAnnotations, &diagnostics)

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

  /// Applies `process` to the ".val" file at the given path and reports XCTest failures where the
  /// effects of processing don't match the file's annotation commands ("diagnostic",
  /// "expect-failure", and "expect-success").
  ///
  /// - Parameter process: applies some compilation phases to `file`, updating `diagnostics`
  ///   with any generated diagnostics. Throws an `Error` if any phases failed.
  public func checkAnnotatedValFileDiagnostics(
    inFileAt valFilePath: String,
    _ process: (_ file: SourceFile, _ diagnostics: inout DiagnosticSet) throws -> Void
  ) throws {
    let f = try SourceFile(contentsOf: URL(fileURLWithPath: valFilePath))
    try checkAnnotations(
      in: f, checkingAnnotationCommands: [],
      { (file, annotationsToHandle, diagnostics) in
        assert(annotationsToHandle.isEmpty)
        try process(file, &diagnostics)
        return []
      }
    )
  }

  /// Given the effects of processing and the annotations not specifically handled by
  /// `processAndCheck` above, returns the final set of test failures to be reported to XCTest.
  fileprivate func failuresToReport(
    effectsOfProcessing processing: ProcessingEffects,
    unhandledAnnotations: ArraySlice<TestAnnotation>
  ) -> [XCTIssue] {
    var testFailures = processing.testFailures

    var diagnosticsByExpectation = Dictionary(
      grouping: processing.diagnostics.elements, by: \.expectation)

    func fail(_ expectation: TestAnnotation, _ message: String) {
      testFailures.append(expectation.failure(message))
    }

    for a in unhandledAnnotations {
      switch a.command {
      case "diagnostic":
        if diagnosticsByExpectation[a]?.popLast() != nil {
        } else {
          fail(a, "missing expected diagnostic\(a.argument.map({": '\($0)'"}) ?? "")")
        }
      case "expect-failure":
        if processing.ranToCompletion {
          fail(a, "compilation stages succeeded, but failure was expected")
        }
      case "expect-success":
        if !processing.ranToCompletion {
          fail(a, "compilation stages failed, but success was expected")
        }
      default:
        fail(a, "unexpected test command: '\(a.command)'")
      }
    }

    testFailures += diagnosticsByExpectation.values.joined().lazy.map {
      XCTIssue(.error("unexpected diagnostic: '\($0.message)'", at: $0.site, notes: $0.notes))
    }
    return testFailures
  }

}