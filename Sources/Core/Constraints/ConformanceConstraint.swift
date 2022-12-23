/// A constraint `L : T1 & ... & Tn` specifying that `L` conforms to the traits `T1, ..., Tn`.
public struct ConformanceConstraint: Constraint, Hashable {

  /// The type subject of the constraint.
  public private(set) var subject: AnyType

  /// The traits to which `subject` must conform.
  public let traits: Set<TraitType>

  public let cause: ConstraintCause

  /// Creates an instance with the given properties.
  public init(
    _ subject: AnyType,
    conformsTo traits: Set<TraitType>,
    because cause: ConstraintCause
  ) {
    self.subject = subject
    self.traits = traits
    self.cause = cause
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    subject = transform(subject)
  }

  public func depends(on variable: TypeVariable) -> Bool {
    subject == variable
  }

}

extension ConformanceConstraint: CustomStringConvertible {

  public var description: String {
    let ts = traits.descriptions(joinedBy: ", ")
    return "\(subject) : \(ts)"
  }

}
