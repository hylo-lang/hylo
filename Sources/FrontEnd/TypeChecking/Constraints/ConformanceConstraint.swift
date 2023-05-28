import Core
import Utils

/// A constraint `L : T1 & ... & Tn` specifying that `L` conforms to the traits `T1, ..., Tn`.
struct ConformanceConstraint: Constraint, Hashable {

  /// The type that must conform to the traits in `traits`.
  private(set) var subject: AnyType

  /// The traits to which `subject` must conform.
  let traits: Set<TraitType>

  let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  init(
    _ subject: AnyType, conformsTo traits: Set<TraitType>,
    origin: ConstraintOrigin
  ) {
    self.subject = subject
    self.traits = traits
    self.origin = origin
  }

  mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&subject, with: transform)
  }

}

extension ConformanceConstraint: CustomStringConvertible {

  var description: String {
    "\(subject) : \(list: traits)"
  }

}
