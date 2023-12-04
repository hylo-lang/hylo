/// A type whose instances are callable entities.
public protocol CallableType {

  /// The parameters of the callable value.
  var inputs: [CallableTypeParameter] { get }

  /// The return type of the callable value.
  var output: AnyType { get }

  /// `true` if `Self` is the type of a function.
  var isArrow: Bool { get }

}

extension CallableType {

  /// The labels of the type.
  public var labels: LazyMapSequence<[CallableTypeParameter], String?> {
    inputs.lazy.map(\.label)
  }

  /// Returns `true` iff instances of `self` accept run-time argument lists with given `labels`.
  public func accepts<S: Collection<String?>>(_ labels: S) -> Bool {
    Core.accepts(inputs[...], labels)
  }

}

/// Returns `true` iff `lhs` matches an argument list labeled by `rhs`.
private func accepts<S: Collection>(
  _ lhs: ArraySlice<CallableTypeParameter>,
  _ rhs: S
) -> Bool where S.Element == String? {
  if lhs.isEmpty {
    return rhs.isEmpty
  } else if lhs.first!.label == rhs.first {
    return accepts(lhs.dropFirst(), rhs.dropFirst())
  } else if lhs.first!.hasDefault {
    return accepts(lhs.dropFirst(), rhs)
  } else {
    return false
  }
}
