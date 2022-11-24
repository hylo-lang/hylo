/// The type of a type.
public struct MetaTypeType: TypeProtocol {

  /// The type of which `self` is a type.
  public let instance: AnyType

  /// Creates a type denoting the type of `instance`.
  public init<T: TypeProtocol>(_ instance: T) {
    self.instance = ^instance
  }

  /// Creates a type denoting the type of `instance`.
  public init(_ instance: AnyType) {
    self.instance = instance
  }

  public var flags: TypeFlags { instance.flags }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    MetaTypeType(instance.transform(transformer))
  }

}

extension MetaTypeType: CustomStringConvertible {

  public var description: String {
    "MetaType<\(instance)>"
  }

}
