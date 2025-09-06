import Utils

/// A constraint `L.i == R` stating that `L` is a tuple type whose `i`-th element has type `R`.
struct TupleMemberConstraint: Constraint, Hashable {

  /// The base type of the left operand.
  private(set) var subject: AnyType

  /// The component of `subject` that must have type `memberType`.
  let component: Int

  /// The type of subject's element.
  private(set) var elementType: AnyType

  let origin: ConstraintOrigin

  /// Creates a constraint requiring `tuple` to be a tuple type with an element of type
  /// `memberType` at given `index`.
  init(
    _ tuple: AnyType,
    component: Int,
    hasType element: AnyType,
    origin: ConstraintOrigin
  ) {
    self.subject = tuple
    self.component = component
    self.elementType = element
    self.origin = origin
  }

  /// Inserts the type variables that occur free in `self` into `s`.
  func collectOpenVariables(in s: inout Set<TypeVariable>) {
    subject.collectOpenVariables(in: &s)
    elementType.collectOpenVariables(in: &s)
  }

  mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&subject, with: transform)
    update(&elementType, with: transform)
  }

}

extension TupleMemberConstraint: CustomStringConvertible {

  var description: String {
    "\(subject).\(component) == \(elementType)"
  }

}
