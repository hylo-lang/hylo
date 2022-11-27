/// A type serving as a witness to another type's conformance to `Hashable`.
public protocol HashableWitness<Element> {

  /// The element for which `Self` serves as a witness.
  associatedtype Element

  /// Hashes the salient features of `element` by feeding them into `hasher`.
  static func hash(_ element: Element, into hasher: inout Hasher)

  /// Returns whether `left` is equal to `right`.
  static func isEqual(_ left: Element, to right: Element) -> Bool

}
