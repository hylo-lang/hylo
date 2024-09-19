import Utils

/// The demangled description of a or entity.
public indirect enum DemangledSymbol: Hashable {

  /// Creates an instance decoding the symbol mangled in `s`, returning `nil` if decoding failed.
  public init?(_ s: String) {
    guard let i = String(assemblySanitized: s) else { return nil }
    var m = Demangler()
    var x = i[...]

    if let value = m.demangle(from: &x) {
      self = value
    } else {
      return nil
    }
  }

  /// Creates an instance decoding a reserved symbol identifier.
  init(reserved: ReservedSymbol) {
    switch reserved {
    case .hylo:
      self = .entity(.hylo)
    case .bool:
      self = .entity(.init(coreType: "Bool"))
    case .int:
      self = .entity(.init(coreType: "Int"))
    case .float64:
      self = .entity(.init(coreType: "Float64"))
    case .string:
      self = .entity(.init(coreType: "String"))
    case .any:
      self = .type(.any)
    case .never:
      self = .type(.never)
    case .void:
      self = .type(.void)
    }
  }

  /// The declaration of an entity or bundle.
  case entity(DemangledEntity)

  /// A binding declaration.
  case binding(names: [DemangledEntity])

  /// A synthesized declaration.
  case synthesized(DemangledSynthesizedFunction)

  /// A monomorphized symbols.
  case monomorphized(DemangledSymbol, arguments: [DemangledSymbol])

  /// A type.
  case type(DemangledType)

  /// The entity wrapped in `self` if its payload is `.entity`, or `nil` otherwise.
  public var entity: DemangledEntity? {
    if case .entity(let e) = self {
      return e
    } else {
      return nil
    }
  }

}

extension DemangledSymbol: CustomStringConvertible {

  public var description: String {
    switch self {
    case .entity(let e):
      return e.description
    case .binding(let n):
      return n.description
    case .synthesized(let d):
      return d.description
    case .monomorphized(let e, let z):
      return "\(e) for \(z)"
    case .type(let t):
      return t.description
    }
  }

}
