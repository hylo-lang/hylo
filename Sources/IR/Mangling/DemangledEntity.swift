import FrontEnd
import Utils

/// The payload of a `DemangledSymbol.entity`.
public struct DemangledEntity: Hashable {

  /// The qualification of the symbol, if any.
  public let qualification: DemangledQualification?

  /// The kind of the symbol, if known.
  public let kind: NodeKind?

  /// The name of the symbol.
  public let name: Name

  /// The arguments of the symbol's generic parameters if it is a function or subscript.
  public let genericArgumentLabels: [String?]

  /// The type of the symbol, if known.
  public let type: DemangledType?

  /// `true` if `self` identifies a scope.
  public let isScope: Bool

  /// Creates an instance with the given properties.
  public init(
    qualification: DemangledQualification?,
    kind: NodeKind,
    name: Name,
    genericArgumentLabels: [String?] = [],
    type: DemangledType? = nil
  ) {
    self.qualification = qualification
    self.kind = kind
    self.name = name
    self.genericArgumentLabels = genericArgumentLabels
    self.type = type
    self.isScope = kind.value is LexicalScope.Type
  }

  /// Creates an instance identifying an anonymous scope.
  public init(anonymousScope id: Int, qualifiedBy q: DemangledQualification) {
    self.qualification = q
    self.kind = nil
    self.name = Name(stem: id.description)
    self.genericArgumentLabels = []
    self.type = nil
    self.isScope = true
  }

  /// Creates an instance representing a core type declaration.
  public init(coreType: String) {
    self.init(
      qualification: .entity(.hylo),
      kind: NodeKind(ProductTypeDecl.self),
      name: Name(stem: coreType))
  }

  /// The `Hylo` module.
  static var hylo: DemangledEntity {
    .init(qualification: nil, kind: NodeKind(ModuleDecl.self), name: Name(stem: "Hylo"))
  }

}

extension DemangledEntity: CustomStringConvertible {

  public var description: String {
    let q = Self.describe(qualification: qualification)

    guard let k = kind else {
      return q + "$\(name)"
    }

    switch k {
    case ConformanceDecl.self, ExtensionDecl.self:
      return q + "[\(name.stem)]"
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

    if k.value is SingleEntityDecl.Type {
      return q + name.stem
    }

    return "???"
  }

  /// A textual representation of `self` assuming it is a function declaration.
  private var functionDescription: String {
    guard case .arrow(_, _, let inputs, _) = type else {
      return "\(name)(???)"
    }

    let i = inputs.reduce(into: "", { (s, p) in s += (p.label ?? "_") + ":" })
    return "\(name)(\(i))"
  }

  /// A textual representation of `self` assuming it is an initializer declaration.
  private var initializerDescription: String {
    guard case .arrow(_, _, let inputs, _) = type else {
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
    return "\(name)[\(i)]"
  }

  /// Returns the textual description of a qualification.
  private static func describe(qualification: DemangledQualification?) -> String {
    switch qualification {
    case .some(.entity(let q)):
      if q.kind?.value == TranslationUnit.self {
        return describe(qualification: q.qualification)
      } else {
        return q.description + "."
      }

    case .some(let q):
      return q.description

    case nil:
      return ""
    }

  }

}
