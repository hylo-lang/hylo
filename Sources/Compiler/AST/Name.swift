/// The name of an entity.
public struct Name: Hashable {

  /// The representation of a name.
  private indirect enum Repr: Hashable {

    /// An unqualified identifier.
    case unqualified(stem: String)

    /// An identifier qualified by a domain.
    case qualified(domain: Repr, stem: String)

  }

  /// The internal representation of the name.
  private var repr: Repr

  /// Creates a new, optionally qualified, bare name.
  public init(domain: Name? = nil, stem: String) {
    if let domain = domain {
      repr = .qualified(domain: domain.repr, stem: stem)
    } else {
      repr = .unqualified(stem: stem)
    }
  }

}
