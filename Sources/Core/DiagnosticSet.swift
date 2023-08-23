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

  /// Throws `self` if any errors were reported.
  public func throwOnError() throws {
    if containsError { throw self }
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
