import Core

/// The callee of a function call in the IR.
enum Callee {

  /// A direct reference to a function, initializer, or method implementation.
  case direct(FunctionReference)

  /// A reference to a variant in a method bundle.
  case bundle(BundleReference<MethodDecl>)

  /// An arrow.
  case arrow(Operand)

  /// Creates an instance representing a reference to the lowered form of `d` in `module`,
  /// specialized by`a` in `scopeOfUse`.
  init(
    _ d: AnyDeclID, specializedBy a: GenericArguments,
    in module: inout Module, usedIn scopeOfUse: AnyScopeID
  ) {
    if let m = MethodDecl.ID(d) {
      self = .bundle(BundleReference(to: m, specializedBy: a))
    } else {
      self = .direct(FunctionReference(to: d, in: &module, specializedBy: a, in: scopeOfUse))
    }
  }

}
