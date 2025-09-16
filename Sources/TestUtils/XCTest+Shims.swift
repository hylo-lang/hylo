import XCTest

/// This file declares a lightweight compatibility layer to fill some of the missing parts of
/// swift-corelibs-xctest API. See https://github.com/apple/swift-corelibs-xctest/issues/348

/// Don't modify this part, it's meant to replicate the exact Swift API:
#if !os(macOS) && !os(iOS) && !os(tvOS) && !os(watchOS)

  /// An object that represents a test failure.
  public struct XCTIssue {

    /// Constants that indicate types of test failures.
    public enum IssueType: Int, Sendable {

      /// A test failure due to a failed test assertion or related API.
      case assertionFailure

      /// A test failure due to an expected test failure that doesn’t occur.
      case unmatchedExpectedFailure

    }

    /// A value for classifying an issue that occurs during testing.
    public var type: IssueType

    /// A concise description of the issue with no transient data, suitable for use in test run
    /// summaries and results aggregation across multiple test runs.
    public var compactDescription: String

    /// The source code location for the issue.
    public var sourceCodeContext: XCTSourceCodeContext?

  }

  /// An object that contains call stack and source code location details to provide context for a
  /// point of execution in a test.
  public struct XCTSourceCodeContext: Sendable {

    /// A representation of a location in source code where a test issue occurred.
    public var location: XCTSourceCodeLocation?

  }

  /// An object that contains a file URL and line number that represents a distinct location in
  /// source code.
  public final class XCTSourceCodeLocation: Hashable, Sendable {

    /// A file URL that represents the file-system location of the source code file.
    public let fileURL: URL

    /// An integer that represents a line of code in a source code file.
    public let lineNumber: Int

    /// Initializes a new instance with a file URL and a line number.
    public init(fileURL: URL, lineNumber: Int) {
      self.fileURL = fileURL
      self.lineNumber = lineNumber
    }

    public func hash(into hasher: inout Hasher) {
      hasher.combine(fileURL)
      hasher.combine(lineNumber)
    }

    public static func == (l: XCTSourceCodeLocation, r: XCTSourceCodeLocation) -> Bool {
      (l.fileURL == r.fileURL) && (l.lineNumber == r.lineNumber)
    }

  }

  extension XCTestCase {

    public func record(_ issue: XCTIssue) {
      let location = issue.sourceCodeContext!.location!
      recordFailure(
        withDescription: issue.compactDescription,
        inFile: location.fileURL.fileSystemPath, atLine: location.lineNumber,
        expected: true)
    }

  }

#endif

/// The following code is slightly modified because Swift 6 broke our replicated API of Swift 5, e.g. by
/// requiring runActivity(named:block:) to be @MainActor and converting the XCTActivity from
/// a struct to a protocol. See the original commit at
/// https://github.com/hylo-lang/hylo/commit/5b05ff1b2a359efdcaecb0d9abb86ea4842eb7ad
//
// They are deliberately named `XCT*Hylo` to avoid conflicts with the original XCTest API.

/// A proxy for the current testing context.
public struct XCTContextHylo: Sendable {

  /// Creates and runs an activity with the provided block of code.
  public static func runActivity<Result>(
    named debugName: String,
    block: (XCTActivityHylo) throws -> Result
  ) rethrows -> Result {
    try block(XCTActivityHylo(debugName: debugName))
  }

}

/// A named substep of a test method.
public struct XCTActivityHylo: Sendable {

  /// A human-readable name for the activity.
  public var debugName: String

}
