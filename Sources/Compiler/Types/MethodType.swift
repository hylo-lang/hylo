import Utils

/// The overarching type of a method declaration.
public struct MethodType: TypeProtocol, Hashable {

  /// The capabilities of the subscript.
  public let capabilities: Set<MethodImplDecl.Introducer>

  /// The type of the receiver.
  public let receiver: Type

  /// The inout labels and types of the method.
  public let inputs: [CallableTypeParameter]

  /// The output type of the method.
  public let output: Type

  public let flags: TypeFlags

  public init(
    capabilities: Set<MethodImplDecl.Introducer>,
    receiver: Type,
    inputs: [CallableTypeParameter],
    output: Type
  ) {
    self.capabilities = capabilities
    self.receiver = receiver
    self.inputs = inputs
    self.output = output

    var fs = receiver.flags
    inputs.forEach({ fs.merge($0.type.flags) })
    fs.merge(output.flags)
    flags = fs
  }

}

extension MethodType: CustomStringConvertible {

  public var description: String {
    let c = capabilities.map({ "\($0)" }).sorted().joined(separator: " ")
    let i = String.joining(inputs, separator: ", ")
    let o = "\(output)"
    return "method[\(receiver)] (\(i)) -> \(o) { \(c) }"
  }

}
