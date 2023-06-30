import Core

/// The context in which a component of a name expression gets resolved.
struct NameResolutionContext {

  /// The type of the receiver.
  let type: AnyType

  /// The arguments parameterizing the generic environment of the receiver.
  let arguments: GenericArguments

  /// The expression of the receiver, unless it is elided.
  let receiver: DeclReference.Receiver?

}
