import Utils

/// The payload of a `DemangledSymbol.entity`.
public struct DemangledEntity: Hashable {

  /// The qualification of the symbol, if any.
  public let qualification: Indirect<DemangledEntity>?

  /// The kind of the symbol.
  public let kind: NodeKind

  /// The name of the symbol, if any.
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

  /// Creates an instance representing a core type declaration.
  public init(coreType: String) {
    self.init(
      qualification: .val, kind: NodeKind(ProductTypeDecl.self), name: Name(stem: coreType))
  }

  /// `true` if `self` denotes a lexical scope.
  public var isScope: Bool {
    kind.value is LexicalScope.Type
  }

  /// The `Val` module.
  static var val: DemangledEntity {
    .init(qualification: nil, kind: NodeKind(ModuleDecl.self), name: Name(stem: "Val"))
  }

}

extension DemangledEntity: CustomStringConvertible {

  public var description: String {
    let q = Self.describe(qualification: qualification)

    switch kind {
    case FunctionDecl.self:
      return q + functionDescription
    case InitializerDecl.self:
      return q + initializerDescription
    case SubscriptDecl.self:
      return q + subscriptBundleDescription
    case SubscriptImpl.self:
      return q + name.stem
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

  /// A textual representation of `self` assuming it is an initializer declaration.
  private var initializerDescription: String {
    guard case .lambda(_, _, let inputs, _) = type else {
      return "init"
    }

    let i = inputs.reduce(into: "", { (s, p) in s += (p.label ?? "_") + ":" })
    return "init(\(i))"
  }

  /// A textual representation of `self` assuming it is a subscript bundle declaration.
  private var subscriptBundleDescription: String {
    guard case .subscriptBundle(_, _, let inputs, _) = type else {
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
