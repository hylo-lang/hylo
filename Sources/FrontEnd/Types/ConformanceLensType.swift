/// A wrapper around a type that is seen through the lens of its conformance to a trait.
public struct ConformanceLensType: TypeProtocol {

  /// The subject type.
  public let subject: AnyType

  /// The trait through which the subject is viewed.
  public let lens: TraitType

  public let flags: ValueFlags

  /// Creates a new conformance lens.
  public init(viewing subject: AnyType, through lens: TraitType) {
    self.subject = subject
    self.lens = lens
    self.flags = subject.flags
  }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    ConformanceLensType(viewing: subject.transform(mutating: &m, transformer), through: lens)
  }

}

extension ConformanceLensType: CustomStringConvertible {

  public var description: String { "\(subject)::\(lens)" }

}
