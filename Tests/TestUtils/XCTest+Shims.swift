import XCTest

/// This file declares a lightweight compatibility layer to fill some of the missing parts of
/// swift-corelibs-xctest API. See https://github.com/apple/swift-corelibs-xctest/issues/348
///
/// Do not modify the name of any type or property. Those are meant to match Apple's API.

#if !os(macOS)

  /// An object that represents a test failure.
  public struct XCTIssue {

    /// Constants that indicate types of test failures.
    enum IssueType: Int {

      /// A test failure due to a failed test assertion or related API.
      case assertionFailure

      /// A test failure due to an expected test failure that doesn’t occur.
      case unmatchedExpectedFailure

    }

    /// A value for classifying an issue that occurs during testing.
    var type: IssueType

    /// A concise description of the issue with no transient data, suitable for use in test run
    /// summaries and results aggregation across multiple test runs.
    var compactDescription: String

    /// The source code location for the issue.
    var sourceCodeContext: XCTSourceCodeContext?

  }

  /// An object that contains call stack and source code location details to provide context for a
  /// point of execution in a test.
  struct XCTSourceCodeContext {

    /// A representation of a location in source code where a test issue occurred.
    var location: XCTSourceCodeLocation?

  }

  /// An object that contains a file URL and line number that represents a distinct location in
  /// source code.
  public final class XCTSourceCodeLocation: Hashable {

    /// A file URL that represents the file-system location of the source code file.
    var fileURL: URL

    /// An integer that represents a line of code in a source code file.
    var lineNumber: Int

    /// Initializes a new instance with a file URL and a line number.
    init(fileURL: URL, lineNumber: Int) {
      self.fileURL = fileURL
      self.lineNumber = lineNumber
    }

    func hash(into hasher: inout Hasher) {
      hasher.combine(fileURL)
      hasher.combine(lineNumber)
    }

    static func == (l: XCTSourceCodeLocation, r: XCTSourceCodeLocation) -> Bool {
      (l.fileURL == r.fileURL) && (l.lineNumber == r.lineNumber)
    }

  }

  /// A proxy for the current testing context.
  struct XCTContext {

    /// Creates and runs an activity with the provided block of code.
    static func runActivity<Result>(
      named name: String,
      block: (XCTActivity) throws -> Result
    ) rethrows -> Result {
      try block(XCTActivity(name: name))
    }

  }

  /// A named substep of a test method.
  struct XCTActivity {

    /// A human-readable name for the activity.
    var name: String

  }

  extension XCTestCase {

    func record(_ issue: XCTIssue) {
      let location = issue.sourceCodeContext!.location!
      recordFailure(
        withDescription: issue.compactDescription,
        inFile: location.fileURL.path, atLine: location.lineNumber,
        expected: true)
    }

  }

#endif
