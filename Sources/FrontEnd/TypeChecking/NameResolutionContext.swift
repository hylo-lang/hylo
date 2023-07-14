import Core

/// The context in which a component of a name expression gets resolved.
struct NameResolutionContext {

  /// The type of the receiver.
  let type: AnyType

  /// The arguments parameterizing the generic environment of the receiver.
  ///
  /// This property contains not only the arguments to `receiver`'s generic parameters (if it has
  /// any), but also the arguments to generic parameters implicitly inherited from the context in
  /// which `receiver` has been declared.
  let arguments: GenericArguments

  /// The expression of the receiver, unless it is elided.
  let receiver: DeclReference.Receiver?

  /// Creates an instance with the given properties.
  ///
  /// - Requires: if `type` is a bound generic type, the keys shared between its parameterization
  ///   and `arguments` are assigned to the same values.
  init(type: AnyType, arguments: GenericArguments, receiver: DeclReference.Receiver?) {
    self.type = type
    self.receiver = receiver

    if let g = BoundGenericType(type) {
      self.arguments = arguments.merging(g.arguments) { (a, b) in
        precondition(a.equals(b))
        return a
      }
    } else {
      self.arguments = arguments
    }
  }

}
