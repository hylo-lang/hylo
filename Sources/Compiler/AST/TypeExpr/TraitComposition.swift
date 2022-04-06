import Utils

/// A trait composition (e.g., `Copyable & Equatable`).
///
/// - Note: A trait composition does not denote a type.
public struct TraitComposition: SourceRepresentable {

  public var range: SourceRange?

  /// The names of traits in the composition.
  public var traits: [NameTypeExpr]

}

extension TraitComposition: CustomStringConvertible {

  public var description: String {
    .joining(traits, separator: " & ")
  }

}
