import Core

/// The witness of a type's conformance to the `Collection` trait from the standard library.
struct CollectionWitness {

  /// The implementation of the `Collection.Index`.
  let index: AnyType

  /// The implementation of the `Collection.Element`.
  let element: AnyType

  /// The implementation of `Collection.start_index`.
  let startIndex: Operand

  /// The implementation of `Collection.end_index`.
  let endIndex: Operand

  /// The implementation of `Collection.index_after(_:)`.
  let indexAfter: Operand

  /// The implementation of `Collection.[].let`.
  let accessLet: Function.ID

  /// Creates the witness of the conformance `c` for use in `module`.
  init(_ c: Core.Conformance, in module: inout Module) {
    self.index = module.program.associatedType("Index", for: c)
    self.element = module.program.associatedType("Element", for: c)

    self.startIndex = .constant(
      module.reference(toImplementationOf: Name(stem: "start_index"), for: c))
    self.endIndex = .constant(
      module.reference(toImplementationOf: Name(stem: "end_index"), for: c))
    self.indexAfter = .constant(
      module.reference(toImplementationOf: Name(stem: "index", labels: ["after"]), for: c))

    self.accessLet = module.demandImplementation(
      of: Name(stem: "[]", labels: [nil], introducer: .let), for: c)
  }

}
