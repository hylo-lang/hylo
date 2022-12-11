/// A skolem (a.k.a. rigid) type variable.
public struct SkolemType: TypeProtocol {

  /// The type of the skolem before skolemization.
  public let base: AnyType

  /// Creates an instance denoting the existential quantification of `base`.
  public init<T: TypeProtocol>(quantifying base: T) { self.base = ^base }

  /// Creates an instance denoting the existential quantification of `base`.
  public init(quantifying base: AnyType) { self.base = base }

  public var flags: TypeFlags { .isCanonical }

}

extension SkolemType: CustomStringConvertible {

  public var description: String { "$\(base)" }

}
