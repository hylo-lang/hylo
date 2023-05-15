import Core

/// Where, how, and under what conditions a type satisfies the requirements of a trait.
public struct LoweredConformance {

  /// A map from requirement to their implementation.
  public typealias ImplementationMap = DeclProperty<Implementation>

  /// The lowered implementation of a requirement.
  public enum Implementation {

    /// The implementation of a function or subscript requirement.
    case function(FunctionReference)

    /// The implementation of an associated type or value requirement.
    case value(any CompileTimeValue)

  }

  /// The trait associated with this conformance.
  public let concept: TraitType

  /// The declaration that establishes this conformance.
  public let source: AnyDeclID

  /// A map from requirement of `concept` to the declaration implementing it.
  public let implementations: ImplementationMap

  /// Creates an instance with the given properties.
  public init(
    concept: TraitType,
    source: AnyDeclID,
    implementations: ImplementationMap
  ) {
    self.concept = concept
    self.source = source
    self.implementations = implementations
  }

}

extension LoweredConformance: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    (l.concept == r.concept) && (l.source == r.source)
  }

}

extension LoweredConformance: Hashable {

  public func hash(into hasher: inout Hasher) {
    concept.hash(into: &hasher)
    source.hash(into: &hasher)
  }

}
