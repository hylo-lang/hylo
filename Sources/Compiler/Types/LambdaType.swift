import Utils

/// The type of a lambda.
public struct LambdaType: TypeProtocol {

  /// The property of the lambda's call operator.
  public let receiverEffect: ReceiverEffect?

  /// The environment of the lambda.
  ///
  /// - Note: Environments are represented as tuples whose individual elements correspond to the
  ///   lambda's captures. Thus, this property must be assigned to either a tuple type, a type
  ///   variable or the error type.
  public let environment: AnyType

  /// The input labels and types of the lambda.
  public let inputs: [CallableTypeParameter]

  /// The output type of the lambda.
  public let output: AnyType

  public let flags: TypeFlags

  /// Creates an instance with the given properties.
  public init(
    receiverEffect: ReceiverEffect? = nil, environment: AnyType = .void,
    inputs: [CallableTypeParameter], output: AnyType
  ) {
    switch environment.base {
    case is TupleType, is TypeVariable, is ErrorType: break
    default: preconditionFailure("invalid environment type")
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

  /// Creates the type of the `let` implementation of `method`; fails if `method` doesn't have a
  /// let capability.
  public init?(letImplOf method: MethodType) {
    if !method.capabilities.contains(.let) { return nil }

    let projectedReceiver = ^RemoteType(.let, method.receiver)
    self.init(
      environment: ^TupleType(labelsAndTypes: [("self", projectedReceiver)]), inputs: method.inputs,
      output: method.output)
  }

  /// Creates the type of the `inout` implementation of `method`; fails if `method` doesn't have an
  /// inout capability.
  public init?(inoutImplOf method: MethodType) {
    if !method.capabilities.contains(.inout) && !method.capabilities.contains(.sink) { return nil }

    let projectedReceiver = ^RemoteType(.inout, method.receiver)
    self.init(
      environment: ^TupleType(labelsAndTypes: [("self", projectedReceiver)]), inputs: method.inputs,
      output: method.output)
  }

  /// Creates the type of the `sink` implementation of `method`; fails if `method` doesn't have a
  /// sink capability.
  public init?(sinkImplOf method: MethodType) {
    if !method.capabilities.contains(.inout) && !method.capabilities.contains(.sink) { return nil }

    self.init(
      receiverEffect: .sink, environment: ^TupleType(labelsAndTypes: [("self", method.receiver)]),
      inputs: method.inputs, output: method.output)
  }

  /// Transforms `self` into a constructor type if `self` has the shape of an initializer type.
  /// Otherwise, returns `nil`.
  public func ctor() -> LambdaType? {
    guard (receiverEffect == nil) && (environment == .void) && (output == .void),
      let receiverType = ParameterType(inputs.first?.type), receiverType.convention == .set
    else { return nil }
    return LambdaType(inputs: Array(inputs[1...]), output: receiverType.bareType)
  }

  /// Indicates whether `self` has an empty environment.
  public var isThin: Bool { environment == .void }

  /// Accesses the individual elements of the lambda's environment.
  public var captures: [TupleType.Element] { TupleType(environment)?.elements ?? [] }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    LambdaType(
      receiverEffect: receiverEffect, environment: environment.transform(transformer),
      inputs: inputs.map({ (p) -> CallableTypeParameter in
        .init(label: p.label, type: p.type.transform(transformer))
      }), output: output.transform(transformer))
  }

}

extension LambdaType: CustomStringConvertible {

  public var description: String {
    if let fx = receiverEffect {
      return "[\(environment)] (\(inputs.descriptions())) \(fx) -> \(output)"
    } else {
      return "[\(environment)] (\(inputs.descriptions())) -> \(output)"
    }
  }

}
