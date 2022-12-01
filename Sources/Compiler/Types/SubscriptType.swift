import Utils

/// The overarching type of a subscript declaration.
public struct SubscriptType: TypeProtocol {

  /// Indicates whether the subscript denotes a computed property.
  public let isProperty: Bool

  /// The capabilities of the subscript.
  public let capabilities: Set<ImplIntroducer>

  public let environment: AnyType

  public let inputs: [CallableTypeParameter]

  public let output: AnyType

  public let flags: TypeFlags

  /// Creates an instance with the given properties.
  public init(
    isProperty: Bool,
    capabilities: Set<ImplIntroducer>,
    environment: AnyType = .void,
    inputs: [CallableTypeParameter],
    output: AnyType
  ) {
    self.isProperty = isProperty
    self.capabilities = capabilities
    self.environment = environment
    self.inputs = inputs
    self.output = output

    var fs = environment.flags
    inputs.forEach({ fs.merge($0.type.flags) })
    fs.merge(output.flags)
    flags = fs
  }

  /// Accesses the individual elements of the subscript's environment.
  public var captures: [TupleType.Element] { TupleType(environment)?.elements ?? [] }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    SubscriptType(
      isProperty: isProperty,
      capabilities: capabilities,
      inputs: inputs.map({ (p) -> CallableTypeParameter in
        .init(label: p.label, type: p.type.transform(transformer))
      }),
      output: output.transform(transformer))
  }

}

extension SubscriptType: CustomStringConvertible {

  public var description: String {
    let cs = capabilities
      .map(String.init(describing:))
      .sorted()
      .joined(separator: " ")

    if isProperty {
      return "property [\(environment)] \(output) { \(cs) }"
    } else {
      let i = inputs.descriptions(joinedBy: ", ")
      return "subscript [\(environment)] (\(i)): \(output) { \(cs) }"
    }
  }

}
