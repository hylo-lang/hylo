import Utils

/// The payload of a `DemangledSymbol.entity`.
public struct DemangledEntity {

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
