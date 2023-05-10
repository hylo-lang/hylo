import Core
import Utils

/// A constraint `L.i == R` stating that `L` is a tuple type whose `i`-th element has type `R`.
public struct TupleMemberConstraint: Constraint, Hashable {

  /// The base type of the left operand.
  public private(set) var subject: AnyType

  /// The index of the element in `subject` that must have type `memberType`.
  public let elementIndex: Int

  /// The type of subject's element.
  public private(set) var elementType: AnyType

  public let origin: ConstraintOrigin

  /// Creates a constraint requiring `tuple` to be a tuple type with an element of type
  /// `memberType` at given `index`.
  public init(
    _ tuple: AnyType,
    at index: Int,
    hasType element: AnyType,
    origin: ConstraintOrigin
  ) {
    self.subject = tuple
    self.elementIndex = index
    self.elementType = element
    self.origin = origin
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&subject, with: transform)
    update(&elementType, with: transform)
  }

}

extension TupleMemberConstraint: CustomStringConvertible {

  public var description: String { "\(subject).\(elementIndex) == \(elementType)" }

}
