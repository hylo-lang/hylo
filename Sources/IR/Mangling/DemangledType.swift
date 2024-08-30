import FrontEnd
import Utils

/// The payload of a `DemangledSymbol.type`.
public indirect enum DemangledType: Hashable {

  /// The `Any` type.
  case any

  /// The existential metatype (`any Metatype`).
  case anyMetatype

  /// The `Never` type.
  case never

  /// The `Void` type.
  case void

  /// An arrow type.
  case arrow(
    effect: AccessEffect,
    environment: DemangledType,
    inputs: [Parameter],
    output: DemangledType)

  /// An associated type.
  case associatedType(domain: DemangledType, name: String)

  /// A bound generic type.
  case boundGeneric(base: DemangledType, arguments: [DemangledSymbol])

  /// A buffer type.
  case buffer(element: DemangledType, count: Int)

  /// A built-in type.
  case builtin(BuiltinType)

  /// An existential generic type.
  case existentialGeneric(DemangledType)

  /// An existential trait type.
  case existentialTrait([DemangledType])

  /// An existential trait type.

  /// A metatype.
  case metatype(DemangledType)

  /// A nominal type.
  case nominal(DemangledEntity)

  /// A parameter type.
  case parameter(access: AccessEffect, value: DemangledType)

  /// A remote type.
  case remote(access: AccessEffect, value: DemangledType)

  /// A subscript type.
  case subscriptBundle(
    capabilities: AccessEffectSet,
    environment: DemangledType,
    inputs: [Parameter],
    output: DemangledType)

  /// A union type.
  case union([DemangledType])

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
    case .any:
      return "Any"
    case .anyMetatype:
      return "any Metatype"
    case .never:
      return "Never"
    case .void:
      return "Void"

    case .arrow(let effect, let environment, let inputs, let output):
      let i = inputs.map { (p) -> String in
        (p.label.map({ $0 + ": " }) ?? "") + p.type.description
      }
      return "[\(environment)](\(list: i)) \(effect) -> \(output)"

    case .associatedType(let domain, let name):
      return "\(domain).\(name)"

    case .boundGeneric(let base, let arguments):
      return "\(base)<\(list: arguments)>"

    case .buffer(let element, let count):
      return "\(element)[\(count)]"

    case .builtin(let t):
      return t.description

    case .existentialGeneric(let interface):
      return "any \(interface)"

    case .existentialTrait(let interface):
      return "any \(list: interface)"

    case .metatype(let t):
      return "Metatype<\(t)>"

    case .nominal(let e):
      return e.description

    case .parameter(let access, let value):
      return "\(access) \(value)"

    case .remote(let access, let value):
      return "\(access) \(value)"

    case .subscriptBundle(let capabilities, let environment, let inputs, let output):
      let i = inputs.map { (p) -> String in
        (p.label.map({ $0 + ": " }) ?? "") + p.type.description
      }
      let cs = capabilities.elements.descriptions(joinedBy: " ")
      return "[\(environment)](\(list: i)) : \(output) { \(cs) }"

    case .union(let elements):
      return "Union<\(list: elements)>"

    case .tuple(let elements):
      let i = elements.map { (p) -> String in
        (p.label.map({ $0 + ": " }) ?? "") + p.type.description
      }
      return "{\(list: i)}"
    }
  }

}
