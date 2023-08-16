import Core
import FrontEnd

extension TypedProgram {

  /// Returns a table mapping a union type discriminator to its corresponding member.
  public func discriminatorToElement(in union: UnionType) -> [AnyType] {
    union.elements.sorted(by: isOrderedBefore)
  }

  /// Returns `true` iff the mangled representation of `a` lexicographically precedes that of `b`.
  private func isOrderedBefore(_ a: AnyType, _ b: AnyType) -> Bool {
    mangled(a).lexicographicallyPrecedes(mangled(b))
  }

}
