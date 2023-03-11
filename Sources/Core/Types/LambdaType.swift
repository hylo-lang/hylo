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

  /// Given an initializer type `t`, creates the corresponding constructor type.
  ///
  /// - Requires: `t` is an initializer type of the form `[](self: set A, B...) -> Void`.
  public init(constructorFormOf t: LambdaType) {
    let r = ParameterType(t.inputs.first!.type)!
    precondition(r.access == .set)
    self.init(inputs: Array(t.inputs[1...]), output: r.bareType)
  }

  /// Given an constructor type `t`, creates the corresponding initializer type.
  ///
  /// - Requires: `t` is a constructor type of the form `[](A...) -> B`.
  public init(initializerFormOf t: LambdaType) {
    let r = CallableTypeParameter(label: "self", type: ^ParameterType(.set, t.output))
    self.init(receiverEffect: .let, inputs: [r] + t.inputs, output: .void)
  }

  /// Returns a thin type accepting `self`'s environment as parameters.
  ///
  /// - Requires: `environment` is a `TupleType`.
  public var lifted: LambdaType {
    let p = TupleType(environment)!.elements.map { (e) -> CallableTypeParameter in
      if let t = RemoteType(e.type) {
        return .init(label: e.label, type: ^ParameterType(t))
      } else {
        return .init(label: e.label, type: ^ParameterType(receiverEffect, e.type))
      }
    }
    return .init(receiverEffect: .let, environment: .void, inputs: p + inputs, output: output)
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
