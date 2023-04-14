import Utils

/// A constraint `L : T1 & ... & Tn` specifying that `L` conforms to the traits `T1, ..., Tn`.
public struct ConformanceConstraint: Constraint, Hashable {

  /// The type that must conform to the traits in `traits`.
  public private(set) var subject: AnyType

  /// The traits to which `subject` must conform.
  public let traits: Set<TraitType>

  public let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  public init(
    _ subject: AnyType,
    conformsTo traits: Set<TraitType>,
    origin: ConstraintOrigin
  ) {
    self.subject = subject
    self.traits = traits
    self.origin = origin
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&subject, with: transform)
  }

}

extension ConformanceConstraint: CustomStringConvertible {

  public var description: String {
    "\(subject) : \(list: traits)"
  }

}
