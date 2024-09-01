import Utils

/// The overarching type of a method declaration.
public struct MethodType: TypeProtocol {

  /// The capabilities of the bundle.
  public let capabilities: AccessEffectSet

  /// The type of the receiver.
  public let receiver: AnyType

  /// The inout labels and types of the method.
  public let inputs: [CallableTypeParameter]

  /// The output type of the method.
  public let output: AnyType

  public let flags: ValueFlags

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
    self.flags = inputs.reduce(output.flags | receiver.flags, { (fs, p) in fs | p.type.flags })
  }

  /// Returns the type of the variant `k` in this bundle.
  ///
  /// - Requires: `k` is in the capabilities of `self`.
  public func variant(_ k: AccessEffect) -> ArrowType {
    precondition(capabilities.contains(k))

    switch k {
    case .let:
      return makeType(^RemoteType(k, receiver), makeFunctionalOutput())
    case .sink:
      return makeType(receiver, makeFunctionalOutput())
    case .set, .inout:
      return makeType(^RemoteType(k, receiver), output)
    case .yielded:
      unreachable()
    }

    /// Returns the output type of a `let` or `sink` variant.
    func makeFunctionalOutput() -> AnyType {
      if output.isVoid {
        return receiver
      } else {
        return ^TupleType([.init(label: "self", type: receiver), .init(label: nil, type: output)])
      }
    }

    /// Returns the type of a method implementation with receiver `r` and output `o`.
    func makeType(_ r: AnyType, _ o: AnyType) -> ArrowType {
      ArrowType(
        receiverEffect: k, environment: ^TupleType([.init(label: "self", type: r)]),
        inputs: inputs, output: o)
    }
  }

  /// Returns the output type of a reference to a method having this type and being used mutably
  /// iff `isMutating` is `true`.
  public func outputOfUse(mutable isMutating: Bool) -> AnyType {
    let s = AccessEffectSet.forUseOfBundle(performingInPlaceMutation: isMutating)
    if let k = capabilities.intersection(s).weakest {
      return variant(k).output
    } else {
      return output
    }
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
