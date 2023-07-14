import Utils

/// The payload of a `DemangledSymbol.entity`.
public struct DemangledEntity: Hashable {

  /// The qualification of the symbol, if any.
  public let qualification: Indirect<DemangledEntity>?

  /// The kind of the symbol.
  public let kind: NodeKind

  /// The name of the symbol.
  public let name: Name

  /// The arguments of the symbol's generic parameters if it is a function or subscript.
  public let genericArgumentLabels: [String?]

  /// The type of the symbol, if known.
  public let type: DemangledType?

  /// Creates an instance with the given properties.
  public init(
    qualification: DemangledEntity?,
    kind: NodeKind,
    name: Name,
    genericArgumentLabels: [String?] = [],
    type: DemangledType? = nil
  ) {
    self.qualification = qualification.map(Indirect.init(_:))
    self.kind = kind
    self.name = name
    self.genericArgumentLabels = genericArgumentLabels
    self.type = type
  }

  /// `true` if `self` denotes a lexical scope.
  public var isScope: Bool {
    kind.value is LexicalScope.Type
  }

}

extension DemangledEntity: CustomStringConvertible {

  public var description: String {
    let q = Self.describe(qualification: qualification)

    switch kind {
    case FunctionDecl.self:
      return q + functionDescription
    case TranslationUnit.self:
      return q + name.stem
    default:
      break
    }

    if kind.value is SingleEntityDecl.Type {
      return q + name.stem
    }

    return "???"
  }

  /// A textual representation of `self` assuming it is a function declaration.
  private var functionDescription: String {
    guard case .lambda(_, _, let inputs, _) = type else {
      return "???"
    }

    let i = inputs.reduce(into: "", { (s, p) in s += (p.label ?? "_") + ":" })
    return "\(name)(\(i))"
  }


  /// Returns the textual description of a qualification.
  private static func describe(qualification: Indirect<DemangledEntity>?) -> String {
    guard let q = qualification else {
      return ""
    }

    if q.value.kind == TranslationUnit.self {
      return describe(qualification: q.value.qualification)
    } else {
      return q.description + "."
    }
  }

}
