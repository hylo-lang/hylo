import Utils

/// A constraint specifying that a type must be expressible by a literal expression.
public struct LiteralConstraint: Constraint, Hashable {

  /// The type that must conform to `literalTrait`.
  public private(set) var subject: AnyType

  /// The default type used to infer `subject`.
  public private(set) var defaultSubject: AnyType

  /// The trait to which `subject` must conform.
  public let literalTrait: TraitType

  public let cause: ConstraintCause

  /// Creates an instance with the given properties.
  ///
  /// - Requires: `literalTrait` must be one of the `ExpressibleBy***Literal` in the core library
  ///   and `defaultSubject` must conform to `literalTrait`.
  public init(
    _ subject: AnyType,
    defaultsTo defaultSubject: AnyType,
    conformsTo literalTrait: TraitType,
    because cause: ConstraintCause
  ) {
    self.subject = subject
    self.defaultSubject = defaultSubject
    self.literalTrait = literalTrait
    self.cause = cause
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    modify(&subject, with: transform)
    modify(&defaultSubject, with: transform)
  }

  public func depends(on variable: TypeVariable) -> Bool {
    subject == variable
  }

}

extension LiteralConstraint: CustomStringConvertible {

  public var description: String {
    "(\(subject) ?? \(defaultSubject)) : \(literalTrait)"
  }

}
