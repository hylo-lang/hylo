import Utils

/// The overarching type of a method declaration.
public struct MethodType: TypeProtocol, CallableType {

  /// The capabilities of the subscript.
  public let capabilities: Set<ImplIntroducer>

  /// The type of the receiver.
  public let receiver: AnyType

  /// The inout labels and types of the method.
  public let inputs: [CallableTypeParameter]

  /// The output type of the method.
  public let output: AnyType

  public let flags: TypeFlags

  /// Creates an instance with the given properties.
  public init(
    capabilities: Set<ImplIntroducer>,
    receiver: AnyType,
    inputs: [CallableTypeParameter],
    output: AnyType
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

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    MethodType(
      capabilities: capabilities,
      receiver: receiver.transform(transformer),
      inputs: inputs.map({ (p) -> CallableTypeParameter in
        .init(label: p.label, type: p.type.transform(transformer))
      }),
      output: output.transform(transformer))
  }

}

extension MethodType: CustomStringConvertible {

  public var description: String {
    let cs =
      capabilities
      .map(String.init(describing:))
      .sorted()
      .joined(separator: " ")
    return "method[\(receiver)] (\(list: inputs)) -> \(output) { \(cs) }"
  }

}
