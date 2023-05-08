/// A parameter in a callable type.
public struct CallableTypeParameter: Hashable {

  /// The label of the parameter.
  public let label: String?

  /// The type of the parameter.
  public let type: AnyType

  /// `true` iff the parameter has a default argument.
  public let hasDefault: Bool

  /// Creates an instance with the given properties.
  public init(label: String? = nil, type: AnyType, hasDefault: Bool = false) {
    self.label = label
    self.type = type
    self.hasDefault = hasDefault
  }

  /// Returns a copy of this instance whose type has been transformed by `transformer`.
  ///
  /// - SeeAlso: `TypeProtocol.transform(_:)`
  public func transform<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    .init(label: label, type: type.transform(mutating: &m, transformer), hasDefault: hasDefault)
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
