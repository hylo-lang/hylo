/// The type of a subscript implementation.
public struct SubscriptImplType: TypeProtocol {

  /// Indicates whether the subscript denotes a computed property.
  public let isProperty: Bool

  /// The property of the subscrit implementation's call operator.
  public let receiverEffect: AccessEffect?

  /// The environment of the subscript implementation.
  public let environment: AnyType

  /// The parameter labels and types of the subscript implementation.
  public let inputs: [CallableTypeParameter]

  /// The type of the value projected by the subscript implementation.
  public let output: AnyType

  public let flags: TypeFlags

  /// Creates an instance with the given properties.
  public init(
    isProperty: Bool,
    receiverEffect: AccessEffect?,
    environment: AnyType,
    inputs: [CallableTypeParameter],
    output: AnyType
  ) {
    self.isProperty = isProperty
    self.receiverEffect = receiverEffect
    self.environment = environment
    self.inputs = inputs
    self.output = output

    var fs = environment.flags
    inputs.forEach({ fs.merge($0.type.flags) })
    fs.merge(output.flags)
    flags = fs
  }

  /// Indicates whether `self` has an empty environment.
  public var isThin: Bool { environment == .void }

  /// Accesses the individual elements of the lambda's environment.
  public var captures: [TupleType.Element] { TupleType(environment)?.elements ?? [] }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    SubscriptImplType(
      isProperty: isProperty,
      receiverEffect: receiverEffect,
      environment: environment.transform(transformer),
      inputs: inputs.map({ (p) -> CallableTypeParameter in
        .init(label: p.label, type: p.type.transform(transformer))
      }),
      output: output.transform(transformer))
  }

  private static func makeTransformer() -> (AnyType) -> TypeTransformAction {
    { (t) in
      if let type = RemoteType(t), type.capability == .yielded {
        return .stepInto(^RemoteType(.let, type.bareType))
      } else {
        return .stepInto(t)
      }
    }
  }

}

extension SubscriptImplType: CustomStringConvertible {

  public var description: String {
    let fx = receiverEffect.map(String.init(describing:)) ?? "_"
    if isProperty {
      return "property [\(environment)] \(output) \(fx)"
    } else {
      return "subscript [\(environment)] (\(list: inputs)) \(fx) : \(output)"
    }
  }

}
