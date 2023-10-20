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
  let accessLet: Function.ID

  /// Creates the witness of the conformance `c` for use in `module`.
  init(_ c: Core.Conformance, in module: inout Module) {
    self.position = module.program.associatedType("Position", for: c)
    self.element = module.program.associatedType("Element", for: c)

    self.startPosition = .constant(
      module.reference(toImplementationOf: Name(stem: "start_position"), for: c))
    self.endPosition = .constant(
      module.reference(toImplementationOf: Name(stem: "end_position"), for: c))
    self.positionAfter = .constant(
      module.reference(toImplementationOf: Name(stem: "position", labels: ["after"]), for: c))

    self.accessLet = module.demandImplementation(
      of: Name(stem: "[]", labels: [nil], introducer: .let), for: c)
  }

}
