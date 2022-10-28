import Utils

/// The type of a lambda.
public struct LambdaType: TypeProtocol, Hashable {

  /// The property of the lambda's call operator.
  public let receiverEffect: ReceiverEffect?

  /// The environment of the lambda.
  ///
  /// - Note: Environments are represented as tuples whose individual elements correspond to the
  ///   lambda's captures. Thus, this property must be assigned to either a tuple type, a type
  ///   variable or the error type.
  public let environment: Type

  /// The input labels and types of the lambda.
  public let inputs: [CallableTypeParameter]

  /// The output type of the lambda.
  public let output: Type

  public let flags: TypeFlags

  public init(
    receiverEffect: ReceiverEffect? = nil,
    environment: Type = .unit,
    inputs: [CallableTypeParameter],
    output: Type
  ) {
    switch environment {
    case .tuple, .variable, .error:
      break
    default:
      preconditionFailure("invalid environment type")
    }

    self.receiverEffect = receiverEffect
    self.environment = environment
    self.inputs = inputs
    self.output = output

    var fs = environment.flags
    inputs.forEach({ fs.merge($0.type.flags) })
    fs.merge(output.flags)
    flags = fs
  }

  /// Creates an instance identifying `x`, failing if `x` is not a lambda type.
  public init?(_ x: Type) {
    if case .lambda(let t) = x {
      self = t
    } else {
      return nil
    }
  }

  /// Creates the type of the `let` implementation of `method`; fails if `method` doesn't have a
  /// let capability.
  public init?(letImplOf method: MethodType) {
    if !method.capabilities.contains(.let) { return nil }

    let projectedReceiver = Type.remote(RemoteType(.let, method.receiver))
    self.init(
      environment: .tuple(TupleType(labelsAndTypes: [("self", projectedReceiver)])),
      inputs: method.inputs,
      output: method.output)
  }

  /// Creates the type of the `inout` implementation of `method`; fails if `method` doesn't have an
  /// inout capability.
  public init?(inoutImplOf method: MethodType) {
    if !method.capabilities.contains(.inout) && !method.capabilities.contains(.sink) { return nil }

    let projectedReceiver = Type.remote(RemoteType(.inout, method.receiver))
    self.init(
      environment: .tuple(TupleType(labelsAndTypes: [("self", projectedReceiver)])),
      inputs: method.inputs,
      output: method.output)
  }

  /// Creates the type of the `sink` implementation of `method`; fails if `method` doesn't have a
  /// sink capability.
  public init?(sinkImplOf method: MethodType) {
    if !method.capabilities.contains(.inout) && !method.capabilities.contains(.sink) { return nil }

    self.init(
      receiverEffect: .sink,
      environment: .tuple(TupleType(labelsAndTypes: [("self", method.receiver)])),
      inputs: method.inputs,
      output: method.output)
  }

  /// Transforms `self` into a constructor type if `self` has the shape of an initializer type.
  /// Otherwise, returns `nil`.
  public func ctor() -> LambdaType? {
    guard (receiverEffect == nil) && (environment == .unit) && (output == .unit),
          let receiverParameter = inputs.first,
          case .parameter(let receiverType) = receiverParameter.type,
          receiverType.convention == .set
    else { return nil }
    return LambdaType(inputs: Array(inputs[1...]), output: receiverType.bareType)
  }

  /// Indicates whether `self` has an empty environment.
  public var isThin: Bool { environment == .unit }

  /// Accesses the individual elements of the lambda's environment.
  public var captures: [TupleType.Element] {
    if case .tuple(let type) = environment {
      return type.elements
    } else {
      return []
    }
  }

}

extension LambdaType: CustomStringConvertible {

  public var description: String {
    let p = receiverEffect.map({ "\($0) " }) ?? ""
    let e = (environment == .unit) ? "thin" : "[\(environment)]"
    return "\(p)\(e) (\(inputs.descriptions())) -> \(output)"
  }


}
