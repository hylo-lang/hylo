/// A wrapper around a type that is seen through the lens of its conformance to a trait.
public struct ConformanceLensType: TypeProtocol, Hashable {

  /// The wrapped type.
  public let wrapped: Type

  /// The trait in which the lens focuses.
  public let focus: TraitType

  public let flags: TypeFlags

  /// Creates a new conformance lens.
  public init(wrapped: Type, focus: TraitType) {
    self.wrapped = wrapped
    self.focus = focus
    self.flags = wrapped.flags
  }
  
}

extension ConformanceLensType: CustomStringConvertible {

  public var description: String { "\(wrapped)::\(focus)" }

}
