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

  /// The conditions under which this conformance holds.
  public let conditions: [GenericConstraint]

  /// The declaration that establishes this conformance.
  public let source: AnyDeclID

  /// The outermost scope in which this conformance is exposed.
  public let scope: AnyScopeID

  /// A map from requirement of `concept` to the declaration implementing it in `model`.
  public let implementations: ImplementationMap

  /// The site at which the conformance is declared.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  public init(
    model: AnyType,
    concept: TraitType,
    conditions: [GenericConstraint],
    source: AnyDeclID,
    scope: AnyScopeID,
    implementations: ImplementationMap,
    site: SourceRange
  ) {
    self.model = model
    self.concept = concept
    self.conditions = conditions
    self.source = source
    self.scope = scope
    self.implementations = implementations
    self.site = site
  }

}

extension Conformance: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    (l.model == r.model) && (l.concept == r.concept) && (l.source == r.source)
  }

}

extension Conformance: Hashable {

  public func hash(into hasher: inout Hasher) {
    model.hash(into: &hasher)
    concept.hash(into: &hasher)
    source.hash(into: &hasher)
  }

}
