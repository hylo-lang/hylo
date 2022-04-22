/// A parameter in a callable type.
public struct CallableTypeParameter: Hashable {

  /// The label of the parameter.
  public let label: String?

  /// The type of the parameter.
  public let type: Type

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
