import Utils

/// The overarching type of a method declaration.
public struct MethodType: TypeProtocol {

  /// The capabilities of the subscript.
  public let capabilities: AccessEffectSet

  /// The type of the receiver.
  public let receiver: AnyType

  /// The inout labels and types of the method.
  public let inputs: [CallableTypeParameter]

  /// The output type of the method.
  public let output: AnyType

  public let flags: TypeFlags

  /// Creates an instance with the given properties.
  public init(
    capabilities: AccessEffectSet,
    receiver: AnyType,
    inputs: [CallableTypeParameter],
    output: AnyType
  ) {
    self.capabilities = capabilities
    self.receiver = receiver
    self.inputs = inputs
    self.output = output

    var fs = receiver.flags
    for i in inputs { fs.merge(i.type.flags) }
    fs.merge(output.flags)
    flags = fs
  }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    MethodType(
      capabilities: capabilities,
      receiver: receiver.transform(mutating: &m, transformer),
      inputs: inputs.map({ $0.transform(mutating: &m, transformer) }),
      output: output.transform(mutating: &m, transformer))
  }
}

extension MethodType: CallableType {

  public var isArrow: Bool { true }

}

extension MethodType: CustomStringConvertible {

  public var description: String {
    let cs = capabilities.elements.descriptions(joinedBy: " ")
    return "method[\(receiver)] (\(list: inputs)) -> \(output) { \(cs) }"
  }

}
