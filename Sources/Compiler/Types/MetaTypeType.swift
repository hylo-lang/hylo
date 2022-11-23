/// The type of a type.
public struct MetaTypeType: TypeProtocol, Hashable {

  /// The type of which `self` is a type.
  public let instance: Type

  /// Creates a type denoting the type of `instance`.
  public init(_ instance: Type) {
    self.instance = instance
  }

  public var flags: TypeFlags { instance.flags }

}

extension MetaTypeType: CustomStringConvertible {

  public var description: String {
    "MetaType<\(instance)>"
  }

}
