/// The type of a type.
public struct MetatypeType: TypeProtocol {

  /// The type of which `self` is a type.
  public let instance: AnyType

  /// Creates a type denoting the type of `instance`.
  public init<T: TypeProtocol>(of instance: T) {
    self.init(of: ^instance)
  }

  /// Creates a type denoting the type of `instance`.
  public init(of instance: AnyType) {
    self.instance = instance
  }

  public var flags: TypeFlags { instance.flags }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    MetatypeType(of: instance.transform(transformer))
  }

}

extension MetatypeType: CustomStringConvertible {

  public var description: String {
    "Metatype<\(instance)>"
  }

}
