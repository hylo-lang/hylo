/// The declaration of an entity that may capture bindings.
public protocol CapturingDecl: Decl {

  /// The explicit capture declarations of the entity.
  var explicitCaptures: [BindingDecl.ID] { get }

}
