import Core

/// Where, how, and under what conditions a type satisfies the requirements of a trait.
public struct Conformance {

  /// A map from requirement to their implementation.
  public typealias ImplementationMap = DeclProperty<Implementation>

  /// The lowered implementation of a requirement.
  public enum Implementation {

    /// The implementation of a function or subscript requirement.
    case function(FunctionReference)

    /// The implementation of an associated type or value requirement.
    case value(CompileTimeValue)

  }

  /// The trait associated with this conformance.
  public let concept: TraitType

  /// A map from requirement of `concept` to the declaration implementing it.
  public let implementations: ImplementationMap

  /// Creates an instance with the given properties.
  public init(concept: TraitType, implementations: ImplementationMap) {
    self.concept = concept
    self.implementations = implementations
  }

}

extension IR.Conformance: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    l.concept == r.concept
  }

}

extension IR.Conformance: Hashable {

  public func hash(into hasher: inout Hasher) {
    concept.hash(into: &hasher)
  }

}
