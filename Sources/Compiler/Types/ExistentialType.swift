/// An existential type, optionally bound by traits and constraints on associated types.
public struct ExistentialType: TypeProtocol, Hashable {

  /// The traits to which the witness is known to conform.
  public let traits: Set<TraitType>

  /// The constraints on the associated types of the witness.
  ///
  /// - Note: This set shall only contain equality and conformance constraints.
  public let constraints: Set<Constraint>

  public let flags: TypeFlags

  /// Creates a new existential type bound by the given traits and constraints.
  public init<S: Sequence>(
    traits: Set<TraitType>,
    constraints: S
  ) where S.Element == Constraint {
    self.traits = traits

    var cs: Set<Constraint> = []
    cs.reserveCapacity(constraints.underestimatedCount)
    for c in constraints {
      switch c {
      case .equality, .conformance:
        cs.insert(c)
      default:
        preconditionFailure("type may only be contained by equality or conformance")
      }
    }
    self.constraints = cs

    let flags = traits.reduce(into: TypeFlags.isCanonical, { (a, b) in a.merge(b.flags) })
    self.flags = flags // FIXME
  }

}

extension ExistentialType: CustomStringConvertible {

  public var description: String {
    if traits.isEmpty && constraints.isEmpty { return "Any" }

    let t = traits.map({ "\($0)" }).joined(separator: " & ")
    if constraints.isEmpty {
      return "any \(t)"
    } else {
      let c = constraints.map({ "\($0)" }).joined(separator: ", ")
      return "any \(t) where \(c)"
    }
  }

}
