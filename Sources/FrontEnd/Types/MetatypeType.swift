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

  public var flags: ValueFlags { instance.flags }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    MetatypeType(of: instance.transform(mutating: &m, transformer))
  }
}

extension MetatypeType: CustomStringConvertible {

  public var description: String {
    "Metatype<\(instance)>"
  }

}
