import Foundation

/// A channel that accumulates reported `Diagnostic`s.
public struct Diagnostics {

  /// All reported diagnostics.
  public private(set) var log: Set<Diagnostic> = []

  /// Whether an error was reported.
  public private(set) var errorReported: Bool = false

  /// Creates an instance.
  public init() {}

  /// Creates an instance reporting `d`.
  public init(_ d: Diagnostic) {
    self = Diagnostics()
    report(d)
  }

  /// Creates an instance reporting the elements of `batch`.
  public init<B: Collection<Diagnostic>>(_ batch: B) {
    report(batch)
  }

  /// Writes `d` into this diagnostic channel, setting `errorReported` iff `d` is an error.
  public mutating func report(_ d: Diagnostic) {
    if d.level == .error { errorReported = true }
    log.insert(d)
  }

  /// Reports each diagnostic in `batch`.
  public mutating func report<B: Collection<Diagnostic>>(_ batch: B) {
    for d in batch { report(d) }
  }

  /// Inserts the diagnostics reported to `other` into the `self`.
  public mutating func formUnion(_ other: Self) {
    log.formUnion(other.log)
    errorReported = errorReported || other.errorReported
  }

  /// Throws `self` if any errors were reported.
  public func throwOnError() throws {
    if errorReported { throw self }
  }

}

extension Diagnostics: ExpressibleByArrayLiteral {

  public init(arrayLiteral batch: Diagnostic...) {
    self.init(batch)
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
