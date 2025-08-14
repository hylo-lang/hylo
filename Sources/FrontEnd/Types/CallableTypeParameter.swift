/// A parameter in a callable type.
public struct CallableTypeParameter: Hashable, Sendable {

  /// The label of the parameter.
  public let label: String?

  /// The type of the parameter.
  public let type: AnyType

  /// `true` iff the parameter has a default argument.
  public let hasDefault: Bool

  /// `true` if arguments to the parameter can be passed implicitly.
  public let isImplicit: Bool

  /// Creates an instance with the given properties.
  public init(
    label: String? = nil, type: AnyType, hasDefault: Bool = false, isImplicit: Bool = false
  ) {
    self.label = label
    self.type = type
    self.hasDefault = hasDefault
    self.isImplicit = isImplicit
  }

  /// `true` iff the parameter is implicit or has a default argument.
  public var isElidible: Bool {
    hasDefault || isImplicit
  }

  /// Returns a copy of this instance with its type replaced by `transformer(&m, t)`.
  ///
  /// - SeeAlso: `TypeProtocol.transform(mutating:_:)`
  public func transform<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    let t = type.transform(mutating: &m, transformer)
    return .init(label: label, type: t, hasDefault: hasDefault, isImplicit: isImplicit)
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
