/// A declaration bundle.
public protocol BundleDecl: ExposableDecl {

  /// The type of a variant in the bundle.
  associatedtype Variant: BundleImpl

  /// The variants of the bundle.
  var impls: [Variant.ID] { get }

}

/// A varient in a declaration bundle.
public protocol BundleImpl: Decl {

  /// The introducer of the variant.
  var introducer: SourceRepresentable<AccessEffect> { get }

}
