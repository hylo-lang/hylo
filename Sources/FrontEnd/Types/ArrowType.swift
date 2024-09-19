import Utils

/// A type expressed with an arrow.
public struct ArrowType: TypeProtocol {

  /// The effect of the arrow's call operator.
  public let receiverEffect: AccessEffect

  /// The environment of the arrow.
  public let environment: AnyType

  /// The input labels and types of the arrow.
  public let inputs: [CallableTypeParameter]

  /// The output type of the arrow.
  public let output: AnyType

  public let flags: ValueFlags

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
    self.flags = inputs.reduce(output.flags | environment.flags, { (fs, p) in fs | p.type.flags })
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
  public init(constructorFormOf t: ArrowType) {
    let r = ParameterType(t.inputs.first!.type)!
    precondition(r.access == .set)
    self.init(inputs: Array(t.inputs[1...]), output: r.bareType)
  }

  /// Given an constructor type `t`, creates the corresponding initializer type.
  ///
  /// - Requires: `t` is a constructor type of the form `[](A...) -> B`.
  public init(initializerFormOf t: ArrowType) {
    let r = CallableTypeParameter(label: "self", type: ^ParameterType(.set, t.output))
    self.init(receiverEffect: .let, inputs: [r] + t.inputs, output: .void)
  }

  /// Returns a thin type accepting `self`'s environment as parameters.
  public var lifted: ArrowType {
    let elements = TupleType(environment).map(\.elements) ?? [.init(label: nil, type: environment)]
    let p = elements.map { (e) -> CallableTypeParameter in
      if let t = RemoteType(e.type) {
        return .init(label: e.label, type: ^ParameterType(t))
      } else {
        return .init(label: e.label, type: ^ParameterType(receiverEffect, e.type))
      }
    }
    return .init(receiverEffect: .let, environment: .void, inputs: p + inputs, output: output)
  }

  /// `self` sans environment.
  public var strippingEnvironment: ArrowType {
    ArrowType(receiverEffect: receiverEffect, environment: .void, inputs: inputs, output: output)
  }

  /// `self` transformed as the type of a member of `receiver`, which is existential.
  public func asMember(of receiver: ExistentialType) -> AnyType {
    var r = captures[0].type
    if let s = RemoteType(r) {
      r = ^RemoteType(s.access, ^receiver)
    } else {
      r = ^receiver
    }

    return ^ArrowType(
      receiverEffect: receiverEffect,
      environment: ^TupleType(labelsAndTypes: [("self", r)]),
      inputs: inputs, output: output)
  }

  /// Accesses the individual elements of the lambda's environment.
  public var captures: [TupleType.Element] {
    func elements(_ t: AnyType) -> [TupleType.Element] {
      switch t.base {
      case let u as TupleType:
        return u.elements
      case let u as TypeAliasType:
        return elements(u.resolved)
      default:
        return [.init(label: nil, type: t)]
      }
    }

    return elements(environment)
  }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    ArrowType(
      receiverEffect: receiverEffect,
      environment: environment.transform(mutating: &m, transformer),
      inputs: inputs.map({ $0.transform(mutating: &m, transformer) }),
      output: output.transform(mutating: &m, transformer))
  }
}

extension ArrowType: CallableType {

  public var isArrow: Bool { true }

}

extension ArrowType: CustomStringConvertible {

  public var description: String {
    "[\(environment)] (\(list: inputs)) \(receiverEffect) -> \(output)"
  }

}
