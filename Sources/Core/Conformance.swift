/// Where, how, and under what conditions a type satisfies the requirements of a trait.
public struct Conformance {

  /// A map from requirement to their implementation.
  public typealias ImplementationMap = DeclProperty<Implementation>

  /// The implementation of a requirement.
  public enum Implementation: Hashable {

    /// Concrete implementation.
    case concrete(AnyDeclID)

    /// Synthesized implementation with given type.
    case synthetic(SynthesizedDecl)

    /// The payload of `.concrete` or `nil` if `self == .synthetic`.
    public var decl: AnyDeclID? {
      if case .concrete(let d) = self {
        return d
      } else {
        return nil
      }
    }

  }

  /// The type on the left-hand side of this conformance.
  public let model: AnyType

  /// The trait to which `model` conforms.
  public let concept: TraitType

  /// The generic (a.k.a., compile-time) arguments of the conformance.
  public let arguments: GenericArguments

  /// The conditions under which this conformance holds.
  public let conditions: [GenericConstraint]

  /// The outermost scope in which this conformance is exposed.
  public let scope: AnyScopeID

  /// A map from requirement of `concept` to the declaration implementing it in `model`.
  public let implementations: ImplementationMap

  /// The site at which the conformance is declared.
  public let site: SourceRange

  /// `true` iff the conformance is implicitly synthesized for a structural type.
  public let isStructural: Bool

  /// Creates an instance with the given properties.
  public init(
    model: AnyType,
    concept: TraitType,
    arguments: GenericArguments,
    conditions: [GenericConstraint],
    scope: AnyScopeID,
    implementations: ImplementationMap,
    isStructural: Bool,
    site: SourceRange
  ) {
    self.model = model
    self.concept = concept
    self.arguments = arguments
    self.conditions = conditions
    self.scope = scope
    self.implementations = implementations
    self.isStructural = isStructural
    self.site = site
  }

}

extension Conformance: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    (l.model == r.model) && (l.concept == r.concept)
  }

}

extension Conformance: Hashable {

  public func hash(into hasher: inout Hasher) {
    model.hash(into: &hasher)
    concept.hash(into: &hasher)
  }

}
