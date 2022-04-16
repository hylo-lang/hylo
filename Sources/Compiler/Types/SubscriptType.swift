/// The overarching type of a subscript declaration.
public struct SubscriptType: TypeProtocol, Hashable {

  /// A parameter in a subscript type.
  public struct Parameter: Hashable {

    /// The label of the parameter.
    public let label: String?

    /// The type of the parameter.
    public let type: Type

  }

  /// Indicates whether the subscript denotes a computed property.
  public let isProperty: Bool

  /// The capabilities of the subscript.
  public let capabilities: Set<SubscriptImplDecl.Introducer>

  /// The input types of the subscript.
  public let inputs: [Parameter]

  /// The output type of the subscript.
  public let output: Type

  public let flags: TypeFlags

  public init(
    isProperty: Bool,
    capabilities: Set<SubscriptImplDecl.Introducer>,
    inputs: [Parameter],
    output: Type
  ) {
    self.isProperty = isProperty
    self.capabilities = capabilities
    self.inputs = inputs
    self.output = output
    self.flags = inputs.reduce(into: output.flags, { (fs, p) in
      fs.merge(p.type.flags)
    })
  }

}

extension SubscriptType: CustomStringConvertible {

  public var description: String {
    let capabilities = capabilities.map({ "\($0)" }).sorted().joined(separator: " ")
    if isProperty {
      return "property \(output) { \(capabilities) }"
    } else {
      let inputs = inputs.map({ "\($0)" }).joined(separator: ", ")
      return "subscript (\(inputs)): \(output) { \(capabilities) }"
    }
  }

}

extension SubscriptType.Parameter: CustomStringConvertible {

  public var description: String {
    if let label = label {
      return "\(label): \(type)"
    } else {
      return "\(type)"
    }
  }

}
