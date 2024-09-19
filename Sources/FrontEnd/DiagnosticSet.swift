/// A set of `Diagnostic` that can answer the question “was there an error?” in O(1).
public struct DiagnosticSet: Error {

  /// The elements of `self`.
  public private(set) var elements: Set<Diagnostic> = []

  /// Whether an error was reported.
  public private(set) var containsError: Bool = false

  /// Creates an empty instance.
  public init() {}

  /// Creates an instance containing the elements of `batch`.
  public init<B: Collection<Diagnostic>>(_ batch: B) {
    formUnion(batch)
  }

  /// Inserts `d` into `self`, returning `true` iff `d` was not already present.
  @discardableResult
  public mutating func insert(_ d: Diagnostic) -> Bool {
    if d.level == .error { containsError = true }
    return elements.insert(d).inserted
  }

  /// Inserts the elements of `batch`.
  public mutating func formUnion<B: Sequence<Diagnostic>>(_ batch: B) {
    for d in batch { insert(d) }
  }

  /// Inserts the elements of `other`.
  public mutating func formUnion(_ other: Self) {
    elements.formUnion(other.elements)
    containsError = containsError || other.containsError
  }

  /// Removes from `self` the elements that are not also contained in `other`.
  public mutating func formIntersection(_ other: Self) {
    self = .init(elements.filter(other.elements.contains(_:)))
  }

  /// Throws `self` if any errors were reported.
  public func throwOnError() throws {
    if containsError { throw self }
  }

  /// Returns the result of calling `action` or captures the Hylo diagnostics it has thrown and
  /// returns `nil`, rethrowing any other errors.
  public mutating func capturingErrors<T>(
    thrownBy action: (inout Self) throws -> T
  ) rethrows -> T? {
    do {
      return try action(&self)
    } catch let d as DiagnosticSet {
      assert(d.containsError, "non-error diagnostics were thrown")
      self.formUnion(d)
      return nil
    }
  }

  /// Whether `self` contains no elements.
  public var isEmpty: Bool { elements.isEmpty }

}

extension DiagnosticSet: ExpressibleByArrayLiteral {

  public init(arrayLiteral batch: Diagnostic...) {
    self.init(batch)
  }

}

extension DiagnosticSet: CustomStringConvertible {

  public var description: String {
    "\(list: elements.sorted(by: Diagnostic.isLoggedBefore), joinedBy: "\n")"
  }

}

extension DiagnosticSet: Equatable {}
