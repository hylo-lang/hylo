/// A parameter in a callable type.
public struct CallableTypeParameter: Hashable {

  /// The label of the parameter.
  public let label: String?

  /// The type of the parameter.
  public let type: AnyType

  /// Creates an instance with the given properties.
  public init(label: String? = nil, type: AnyType) {
    self.label = label
    self.type = type
  }

}

extension CallableTypeParameter: CustomStringConvertible {

  public var description: String {
    if let label = label {
      return "\(label): \(type)"
    } else {
      return "\(type)"
    }
  }

}
