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
