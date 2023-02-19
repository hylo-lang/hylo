/// A type whose instances are callable entities.
///
/// Do not declare new conformances to `CallableType`. Only the `LambdaType` and `MethodType` are
/// are valid conforming types.
public protocol CallableType {

  /// The parameters of the callable value.
  var inputs: [CallableTypeParameter] { get }

  /// The return type of the callable value.
  var output: AnyType { get }

}

extension CallableType {

  /// The labels of the type.
  public var labels: LazyMapSequence<[CallableTypeParameter], String?> {
    inputs.lazy.map(\.label)
  }

}
