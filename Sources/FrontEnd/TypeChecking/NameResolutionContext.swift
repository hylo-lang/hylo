import Core

/// The context in which a component of a name expression gets resolved.
struct NameResolutionContext {

  /// The type of the receiver.
  let type: AnyType

  /// The arguments parameterizing the generic environment of the receiver.
  let arguments: GenericArguments

  /// The expression of the receiver, unless it is elided.
  let receiver: DeclReference.Receiver?

  /// Creates a context denoting the declaration of space of `type`, parameterized by `arguments`
  /// and optionally expressed by `receiver`.
  init(type: AnyType, arguments: GenericArguments, receiver: DeclReference.Receiver?) {
    self.type = type
    self.arguments = arguments
    self.receiver = receiver
  }

}
