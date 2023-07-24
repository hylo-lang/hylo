/// A bundled declaration.
public protocol BundleDecl: Decl {

  /// The type of a variant in the bundle.
  associatedtype Variant: Decl

  /// The variants of the bundle.
  var impls: [Variant.ID] { get }

}
