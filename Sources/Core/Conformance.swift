/// Where, how, and under what conditions a type satisfies the requirements of a trait.
public struct Conformance {

  /// A map from requirement to their implementation.
  public typealias ImplementationMap = DeclProperty<Implementation>

  /// The implementation of a conformance.
  public enum Implementation: Hashable {

    /// Concrete implementation.
    case concrete(AnyDeclID)

    /// Synthesized implementation.
    case synthetic

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
  public let conditions: [Constraint]

  /// The outermost scope in which this conformance holds.
  public let scope: AnyScopeID

  /// A map from requirement of `concept` to the declaration implementing it in `model`.
  public let implementations: ImplementationMap

  /// The site at which the conformance is declared.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  public init(
    model: AnyType,
    concept: TraitType,
    conditions: [Constraint],
    scope: AnyScopeID,
    implementations: ImplementationMap,
    site: SourceRange
  ) {
    self.model = model
    self.concept = concept
    self.conditions = conditions
    self.scope = scope
    self.implementations = implementations
    self.site = site
  }

}
