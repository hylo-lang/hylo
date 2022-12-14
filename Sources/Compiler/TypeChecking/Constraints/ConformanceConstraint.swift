/// A constraint `L : T1 & ... & Tn` specifying that `L` conforms to the traits `T1, ..., Tn`.
struct ConformanceConstraint: Constraint, Hashable {

  /// The type subject of the constraint.
  private(set) var subject: AnyType

  /// The traits to which `subject` must conform.
  let traits: Set<TraitType>

  var cause: ConstraintCause

  /// Creates an instance with the given properties.
  init(
    _ subject: AnyType,
    conformsTo traits: Set<TraitType>,
    because cause: ConstraintCause
  ) {
    self.subject = subject
    self.traits = traits
    self.cause = cause
  }

  mutating func modifyTypes(_ modify: (inout AnyType) -> Void) {
    modify(&subject)
  }

  func depends(on variable: TypeVariable) -> Bool {
    subject == variable
  }

}

extension ConformanceConstraint: CustomStringConvertible {

  public var description: String {
    let ts = traits.descriptions(joinedBy: ", ")
    return "\(subject) : \(ts)"
  }

}
