import Foundation

/// A channel that accumulates reported `Diagnostic`s, optionally writing each unique one to
/// standard error when it is reported.
public struct Diagnostics {

  /// Whether reported diagnostics are to be sent immediately to the standard error stream.
  public let reportingToStderr: Bool

  /// All reported diagnostics.
  public private(set) var log: Set<Diagnostic> = []

  /// Whether an error was reported.
  public private(set) var errorReported: Bool = false

  /// Creates an instance that sends reported diagnostics to standard error iff `reportingToStderr`
  /// is `true`.
  ///
  /// - Note: the default is `false` because we build many more tests than command-line tools.
  public init(reportingToStderr: Bool = false) {
    self.reportingToStderr = reportingToStderr
  }

  /// Writes `d` into this diagnostic channel, setting `errorReported` iff `d` is an error.
  public mutating func report(_ d: Diagnostic) {
    if d.level == .error { errorReported = true }
    if log.insert(d).inserted && reportingToStderr {
      FileHandle.standardError.write(
        // When lossy conversion is allowed, result is always non-nil
        "\(d)".data(using: .utf8, allowLossyConversion: true)!)
    }
  }

  /// Reports each diagnostic in `batch`.
  public mutating func report<B: Collection<Diagnostic>>(_ batch: B) {
    for d in batch { report(d) }
  }

  /// Throws `self` if any errors were reported.
  public func throwOnError() throws {
    if errorReported { throw self }
  }

  /// Creates an instance reporting `d`.
  public init(_ d: Diagnostic) {
    self = Diagnostics()
    report(d)
  }

  /// Creates an instance reporting the elements of `batch`.
  public init<B: Collection<Diagnostic>>(_ batch: B, reportingToStderr: Bool = false) {
    self = .init(reportingToStderr: reportingToStderr)
    report(batch)
  }
}

extension Diagnostics: CustomStringConvertible {

  public var description: String {
    "\(list: log.sorted(by: Diagnostic.isLoggedBefore), joinedBy: "\n")"
  }

}

extension Diagnostics: Error {

  var localizedDescription: String { description }

}
