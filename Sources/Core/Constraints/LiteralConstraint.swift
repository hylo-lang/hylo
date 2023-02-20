import Utils

/// A constraint specifying that a type must be expressible by a literal expression.
public struct LiteralConstraint: Constraint, Hashable {

  /// The type that must conform to `literalTrait`.
  public private(set) var subject: AnyType

  /// The default type used to infer `subject`.
  public private(set) var defaultSubject: AnyType

  /// The trait to which `subject` must conform.
  public let literal: TraitType

  public let cause: ConstraintOrigin

  /// Creates an instance with the given properties.
  ///
  /// - Requires: `literal` must be one of the `ExpressibleBy***Literal` in the core library and
  ///  `defaultSubject` must conform to `literal`.
  public init(
    _ subject: AnyType,
    defaultsTo defaultSubject: AnyType,
    conformsTo literal: TraitType,
    because cause: ConstraintOrigin
  ) {
    self.subject = subject
    self.defaultSubject = defaultSubject
    self.literal = literal
    self.cause = cause
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    modify(&subject, with: transform)
    modify(&defaultSubject, with: transform)
  }

}

extension LiteralConstraint: CustomStringConvertible {

  public var description: String {
    "(\(subject) ?? \(defaultSubject)) : \(literal)"
  }

}
