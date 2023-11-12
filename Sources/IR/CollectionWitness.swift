import Core

/// The witness of a type's conformance to the `Collection` trait from the standard library.
struct CollectionWitness {

  /// The implementation of the `Collection.Position`.
  let position: AnyType

  /// The implementation of the `Collection.Element`.
  let element: AnyType

  /// The implementation of `Collection.start_position`.
  let startPosition: Operand

  /// The implementation of `Collection.end_position`.
  let endPosition: Operand

  /// The implementation of `Collection.position(after:)`.
  let positionAfter: Operand

  /// The implementation of `Collection.[].let`.
  let access: Function.ID

  /// Creates the lowered witness of the conformance `c` for use in `module`.
  init(_ c: Core.Conformance, in module: inout Module) {
    let collection = module.program.ast.core.collection

    self.position = module.program.associatedType(collection.position, for: c)
    self.element = module.program.associatedType(collection.element, for: c)
    self.startPosition = .constant(
      module.reference(toImplementationOf: collection.startPosition, for: c))
    self.endPosition = .constant(
      module.reference(toImplementationOf: collection.endPosition, for: c))
    self.positionAfter = .constant(
      module.reference(toImplementationOf: collection.positionAfter, for: c))
    self.access = module.demandImplementation(of: collection.access, for: c)
  }

}
