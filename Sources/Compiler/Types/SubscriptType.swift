import Utils

/// The overarching type of a subscript declaration.
public struct SubscriptType: CallableType, Hashable {

  /// Indicates whether the subscript denotes a computed property.
  public let isProperty: Bool

  /// The capabilities of the subscript.
  public let capabilities: Set<SubscriptImplDecl.Introducer>

  public let environment: Type

  public let inputs: [CallableTypeParameter]

  public let output: Type

  public let flags: TypeFlags

  public init(
    isProperty: Bool,
    capabilities: Set<SubscriptImplDecl.Introducer>,
    environment: Type = .unit,
    inputs: [CallableTypeParameter],
    output: Type
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

}

extension SubscriptType: CustomStringConvertible {

  public var description: String {
    let c = capabilities.map({ "\($0)" }).sorted().joined(separator: " ")
    let e = (environment == .unit) ? "thin" : "[\(environment)]"
    if isProperty {
      return "\(e) property \(output) { \(c) }"
    } else {
      let i = String.joining(inputs, separator: ", ")
      let o = "\(output)"
      return "\(e) subscript (\(i)): \(o) { \(c) }"
    }
  }

}
