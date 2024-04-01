/// The declaration of an entity that may capture bindings.
public protocol CapturingDecl: Decl, LexicalScope {

  /// The explicit capture declarations of the entity.
  var explicitCaptures: [BindingDecl.ID] { get }

  /// The part of the declaration that may have implicit captures.
  var sourcesOfImplicitCaptures: [AnyNodeID] { get }

}
