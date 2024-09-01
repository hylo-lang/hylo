/// The type of a subscript implementation.
public struct SubscriptImplType: TypeProtocol {

  /// Indicates whether the subscript denotes a computed property.
  public let isProperty: Bool

  /// The effect of the subscript's call operator.
  public let receiverEffect: AccessEffect

  /// The environment of the subscript implementation.
  public let environment: AnyType

  /// The parameter labels and types of the subscript implementation.
  public let inputs: [CallableTypeParameter]

  /// The type of the value projected by the subscript implementation.
  public let output: AnyType

  public let flags: ValueFlags

  /// Creates an instance with the given properties.
  public init(
    isProperty: Bool,
    receiverEffect: AccessEffect,
    environment: AnyType,
    inputs: [CallableTypeParameter],
    output: AnyType
  ) {
    self.isProperty = isProperty
    self.receiverEffect = receiverEffect
    self.environment = environment
    self.inputs = inputs
    self.output = output
    self.flags = inputs.reduce(output.flags | environment.flags, { (fs, p) in fs | p.type.flags })
  }

  /// Indicates whether `self` has an empty environment.
  public var isThin: Bool { environment.isVoid }

  /// Accesses the individual elements of the arrow's environment.
  public var captures: [TupleType.Element] { TupleType(environment)?.elements ?? [] }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    SubscriptImplType(
      isProperty: isProperty,
      receiverEffect: receiverEffect,
      environment: environment.transform(mutating: &m, transformer),
      inputs: inputs.map({ $0.transform(mutating: &m, transformer) }),
      output: output.transform(mutating: &m, transformer))
  }

}

extension SubscriptImplType: CustomStringConvertible {

  public var description: String {
    if isProperty {
      return "property [\(environment)] \(receiverEffect) : \(output)"
    } else {
      return "subscript [\(environment)] (\(list: inputs)) \(receiverEffect) : \(output)"
    }
  }

}
