import FrontEnd

/// The witness of a type's conformance to the `Iterator` trait from the standard library.
struct IteratorWitness {

  /// The implementation of the `Iterator.Element`.
  let element: AnyType

  /// The implementation of `Iterator.next`.
  let next: Operand

  /// Creates the lowered witness of the conformance `c` for use in `module`.
  init(_ c: FrontEnd.Conformance, in module: inout Module) {
    let iterator = module.program.ast.core.iterator
    self.element = module.program.associatedType(iterator.element, for: c)
    self.next = .constant(module.reference(toImplementationOf: iterator.next, for: c))
  }

}
