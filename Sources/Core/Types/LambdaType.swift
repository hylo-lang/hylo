import Utils

/// The type of a lambda.
public struct LambdaType: TypeProtocol, CallableType {

  /// The effect of the lambda's call operator.
  public let receiverEffect: AccessEffect

  /// The environment of the lambda.
  public let environment: AnyType

  /// The input labels and types of the lambda.
  public let inputs: [CallableTypeParameter]

  /// The output type of the lambda.
  public let output: AnyType

  public let flags: TypeFlags

  /// Creates an instance with the given properties.
  public init(
    receiverEffect: AccessEffect = .let,
    environment: AnyType = .void,
    inputs: [CallableTypeParameter],
    output: AnyType
  ) {
    self.receiverEffect = receiverEffect
    self.environment = environment
    self.inputs = inputs
    self.output = output

    var fs = environment.flags
    inputs.forEach({ fs.merge($0.type.flags) })
    fs.merge(output.flags)
    flags = fs
  }

  /// Creates the type of a function accepting `inputs` as `let` parameters and returning `output`.
  public init(_ inputs: AnyType..., to output: AnyType) {
    self.init(
      inputs: inputs.map({ (t) in .init(type: ^ParameterType(.let, t)) }),
      output: ^output)
  }

  /// Given an initializer type `initializer`, creates the corresponding constructor type.
  ///
  /// - Requires: `initializer` is an initializer type of the form `[](set A, B...) -> Void`.
  public init(constructorFormOf initializer: LambdaType) {
    let r = ParameterType(initializer.inputs.first!.type)!
    precondition(r.convention == .set)

    self.init(
      receiverEffect: .set, environment: .void,
      inputs: Array(initializer.inputs[1...]), output: r.bareType)
  }

  /// Creates the type of variant in `bundle` corresponding to `access` or fails if `bundle`
  /// doesn't offer that capability.
  ///
  /// - Requires: `effect` is not `yielded`.
  public init?(_ bundle: MethodType, for effect: AccessEffect) {
    precondition(effect != .yielded)
    if !bundle.capabilities.contains(effect) { return nil }

    let e = (effect == .sink)
      ? TupleType(labelsAndTypes: [("self", bundle.receiver)])
      : TupleType(labelsAndTypes: [("self", ^RemoteType(effect, bundle.receiver))])
    self.init(
      receiverEffect: effect, environment: ^e, inputs: bundle.inputs, output: bundle.output)
  }

  /// Accesses the individual elements of the lambda's environment.
  public var captures: [TupleType.Element] { TupleType(environment)?.elements ?? [] }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    LambdaType(
      receiverEffect: receiverEffect,
      environment: environment.transform(transformer),
      inputs: inputs.map({ (p) -> CallableTypeParameter in
        .init(label: p.label, type: p.type.transform(transformer))
      }),
      output: output.transform(transformer))
  }

}

extension LambdaType: CustomStringConvertible {

  public var description: String {
    "[\(environment)] (\(list: inputs)) \(receiverEffect) -> \(output)"
  }

}
