/// A type serving as a witness to another type's conformance to `Equatable`.
public protocol EquatableWitness<Element> {

  /// The element for which `Self` serves as a witness.
  associatedtype Element: Sendable

  /// Returns whether `left` is equal to `right`.
  static func isEqual(_ left: Element, to right: Element) -> Bool

}
