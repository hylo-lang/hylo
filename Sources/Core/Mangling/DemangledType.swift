/// The payload of a `DemangledSymbol.type`.
public indirect enum DemangledType: Hashable {

  /// The `Never` type.
  case never

  /// The `Void` type.
  case void

  /// A bound generic type.
  case boundGeneric(base: DemangledType, arguments: [DemangledSymbol])

  /// A lambda type.
  case lambda(
    effect: AccessEffect,
    environment: DemangledType,
    inputs: [Parameter],
    output: DemangledType)

  /// A parameter type.
  case parameter(access: AccessEffect, value: DemangledType)

  /// A product type.
  case product(DemangledEntity)

  /// A remote type.
  case remote(access: AccessEffect, value: DemangledType)

  /// A subscript type.
  case subscriptBundle(
    capabilities: AccessEffectSet,
    environment: DemangledType,
    inputs: [Parameter],
    output: DemangledType)

  /// A sum type.
  case sum([DemangledType])

  /// A tuple type.
  case tuple([Parameter])

  /// A parameter of a callable symbol.
  public struct Parameter: Hashable {

    /// The argument label of the parameter.
    let label: String?

    /// The type of the parameter.
    let type: DemangledType

  }

}

extension DemangledType: CustomStringConvertible {

  public var description: String {
    switch self {
    case .never:
      return "Never"
    case .void:
      return "Void"

    case .boundGeneric(let base, let arguments):
      return "\(base)<\(list: arguments)>"

    case .lambda(let effect, let environment, let inputs, let output):
      let i = inputs.map { (p) -> String in
        (p.label.map({ $0 + ": " }) ?? "") + p.type.description
      }
      return "[\(environment)](\(list: i) \(effect) -> \(output)"

    case .parameter(let access, let value):
      return "\(access) \(value)"

    case .product(let e):
      return e.description

    case .remote(let access, let value):
      return "\(access) \(value)"

    case .subscriptBundle(let capabilities, let environment, let inputs, let output):
      let i = inputs.map { (p) -> String in
        (p.label.map({ $0 + ": " }) ?? "") + p.type.description
      }
      let cs = capabilities.elements.descriptions(joinedBy: " ")
      return "[\(environment)](\(list: i) : \(output) { \(cs) }"

    case .sum(let elements):
      return "Sum<\(list: elements)>"

    case .tuple(let elements):
      let i = elements.map { (p) -> String in
        (p.label.map({ $0 + ": " }) ?? "") + p.type.description
      }
      return "{\(list: i)}"
    }
  }

}
