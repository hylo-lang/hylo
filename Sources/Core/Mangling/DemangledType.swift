/// The payload of a `DemangledSymbol.type`.
public indirect enum DemangledType {

  /// A lambda type.
  case lambda(
    effect: AccessEffect,
    environment: DemangledType,
    inputs: [(label: String?, type: DemangledType)],
    output: DemangledType)

  /// A parameter type.
  case parameter(access: AccessEffect, value: DemangledType)

  /// A product type.
  case product(DemangledEntity)

  /// The `Never` type.
  case never

  /// The `Void` type.
  case void

}

extension DemangledType: CustomStringConvertible {

  public var description: String {
    switch self {
    case .lambda(let effect, let environment, let inputs, let output):
      let i = inputs.map { (p) -> String in
        (p.label.map({ $0 + ": " }) ?? "") + p.type.description
      }
      return "[\(environment)](\(list: i) \(effect) -> \(output)"

    case .parameter(let access, let value):
      return "\(access) \(value)"

    case .product(let e):
      return e.description

    case .never:
      return "Never"

    case .void:
      return "Void"
    }
  }

}
