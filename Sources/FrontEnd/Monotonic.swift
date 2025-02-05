import Utils

/// A type whose instances can be updated without loss of information.
protocol Monotonic: Equatable, Sendable {

  /// Assigns `self` to `other`, asserting that the resulting value contains at least as much
  /// information as `self`.
  mutating func updateMonotonically(_ other: Self)

}

extension Monotonic where Self: Equatable {

  /// Asserts that `self == other`.
  func updateMonotonically(_ other: Self) {
    assert(self == other, "non-monotonic update of \(type(of: self))")
  }

}

extension Optional: Monotonic where Wrapped: Monotonic {

  mutating func updateMonotonically(_ other: Self) {
    guard let v = other else {
      assert(self == nil, "non-monotonic update")
      return
    }

    if var u = self {
      u.updateMonotonically(v)
      self = .some(u)
    } else {
      self = other
    }
  }

}

extension Array: Monotonic where Element: Monotonic {

  mutating func updateMonotonically(_ other: [Element]) {
    assert(elementsEqual(other.prefix(count)))
    append(contentsOf: other.suffix(from: count))
  }

}

extension Set: Monotonic where /* Element: Sendable, */ Element: Monotonic {

  mutating func updateMonotonically(_ other: Self) {
    formUnion(other)
  }

}

extension Dictionary: Monotonic where Key: Monotonic, Value: Monotonic {

  mutating func updateMonotonically(_ other: Self) {
    for (k, v) in other {
      self[k].updateMonotonically(v)
    }
  }

}

extension AnyType: Monotonic {}

extension DeclReference: Monotonic {}

extension FoldedSequenceExpr: Monotonic {}

extension GenericEnvironment: Monotonic {}

extension ImplicitCapture: Monotonic {}

extension Memo: Monotonic where T: Monotonic {

  mutating func updateMonotonically(_ other: Self) {
    guard let v = other.computed else {
      assert(self == .inProgress, "non-monotonic update")
      return
    }

    if var u = self.computed {
      u.updateMonotonically(v)
      self = .computed(u)
    } else {
      self = other
    }
  }

}

extension FunctionAttributes: Monotonic {}
