import Utils

/// A type that refers to a type member of an existentially quantified generic type parameter.
public struct AssociatedType: TypeProtocol, Hashable {

  public let flags: TypeFlags = [.isCanonical]

}
