/// The context in which a component of a name expression gets resolved.
///
/// This structure is used during name resolution to identify the type of which an entity is member
/// and the generic arguments captured by that entity. The following invariants are maintained:
/// - `arguments` contains the arguments of `type`'s generic parameters (if any) and the arguments
///   to generic parameters captured by the scope in which name resolution takes place.
/// - if `type` is a bound generic type, `type.arguments[k] = arguments[k]` for all keys in `type`.
struct NameResolutionContext {

  /// The type of the receiver.
  let type: AnyType

  /// The arguments parameterizing the generic environment of the receiver.
  let arguments: GenericArguments

  /// The expression of the receiver, unless it is elided.
  let receiver: DeclReference.Receiver?

  /// Creates an instance with the given properties.
  init(type: AnyType, arguments: GenericArguments = .empty, receiver: DeclReference.Receiver?) {
    self.type = type
    self.receiver = receiver

    let a = type.specialization
    if arguments.isEmpty {
      self.arguments = a
    } else {
      self.arguments = arguments.merging(a)
    }
  }

}
