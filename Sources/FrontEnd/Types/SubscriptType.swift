import Utils

/// The overarching type of a subscript declaration.
public struct SubscriptType: TypeProtocol {

  /// Indicates whether the subscript denotes a computed property.
  public let isProperty: Bool

  /// The capabilities of the subscript.
  public let capabilities: AccessEffectSet

  /// The environment of the subscript implementation.
  public let environment: AnyType

  /// The parameter labels and types of the subscript.
  public let inputs: [CallableTypeParameter]

  /// The type of the value projected by the subscript.
  public let output: AnyType

  public let flags: ValueFlags

  /// Creates an instance with the given properties.
  public init(
    isProperty: Bool,
    capabilities: AccessEffectSet,
    environment: AnyType = .void,
    inputs: [CallableTypeParameter],
    output: AnyType
  ) {
    self.isProperty = isProperty
    self.capabilities = capabilities
    self.environment = environment
    self.inputs = inputs
    self.output = output
    self.flags = inputs.reduce(output.flags | environment.flags, { (fs, p) in fs | p.type.flags })
  }

  /// Accesses the individual elements of the subscript's environment.
  public var captures: [TupleType.Element] { TupleType(environment)?.elements ?? [] }

  /// Returns the type of the thin function corresponding to `self`.
  public var pure: ArrowType {
    let captures = TupleType(environment).map(\.elements) ?? [.init(label: nil, type: environment)]
    let p = captures.map { (e) -> CallableTypeParameter in
      if let t = RemoteType(e.type) {
        return .init(label: e.label, type: ^ParameterType(t))
      } else {
        return .init(label: e.label, type: ^ParameterType(.yielded, e.type))
      }
    }
    let o = RemoteType(.yielded, output)
    return .init(receiverEffect: .let, environment: .void, inputs: p + inputs, output: ^o)
  }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    SubscriptType(
      isProperty: isProperty,
      capabilities: capabilities,
      environment: environment.transform(mutating: &m, transformer),
      inputs: inputs.map({ $0.transform(mutating: &m, transformer) }),
      output: output.transform(mutating: &m, transformer))
  }

}

extension SubscriptType: CallableType {

  public var isArrow: Bool { false }

}

extension SubscriptType: CustomStringConvertible {

  public var description: String {
    let cs = capabilities.elements.descriptions(joinedBy: " ")
    if isProperty {
      return "property [\(environment)] \(output) { \(cs) }"
    } else {
      return "subscript [\(environment)] (\(list: inputs)): \(output) { \(cs) }"
    }
  }

}
