/// A wrapper around a type that is seen through the lens of its conformance to a trait.
public struct ConformanceLensType: TypeProtocol {

  /// The wrapped type.
  public let wrapped: AnyType

  /// The trait in which the lens focuses.
  public let focus: TraitType

  public let flags: TypeFlags

  /// Creates a new conformance lens.
  public init(wrapped: AnyType, focus: TraitType) {
    self.wrapped = wrapped
    self.focus = focus
    self.flags = wrapped.flags
  }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    ConformanceLensType(wrapped: wrapped.transform(transformer), focus: focus)
  }
  
}

extension ConformanceLensType: CustomStringConvertible {

  public var description: String { "\(wrapped)::\(focus)" }

}
