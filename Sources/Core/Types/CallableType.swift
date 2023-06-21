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

}
