import FrontEnd
import OrderedCollections

/// Branches to one of several basic blocks based on the discriminator of a union.
public struct UnionSwitch: Terminator {

  /// The type of a map from payload type to its target.
  public typealias Targets = OrderedDictionary<AnyType, Block.ID>

  /// The union container whose discriminator is read.
  public private(set) var scrutinee: Operand

  /// A map from payload type to its target.
  public private(set) var targets: Targets

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(scrutinee: Operand, targets: Targets, site: SourceRange) {
    self.scrutinee = scrutinee
    self.targets = targets
    self.site = site
  }

  

  public var operands: [Operand] {
    [scrutinee]
  }

  public var successors: [Block.ID] {
    Array(targets.values)
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    scrutinee = new
  }

  mutating func replaceSuccessor(_ old: Block.ID, with new: Block.ID) -> Bool {
    precondition(new.function == successors[0].function)
    for (t, b) in targets {
      if b == old { targets[t] = b; return true }
    }
    return false
  }

}

extension UnionSwitch: CustomStringConvertible {

  public var description: String {
    var s = "union_switch \(scrutinee)"
    for (t, b) in targets {
      s.write(", \(t) => \(b)")
    }
    return s
  }

}

extension Module {

  /// Creates a `union_switch` anchored at `site` that jumps to the block assigned to the type of
  /// `scrutinee`'s payload in `targets`.
  ///
  /// - Requires: `scrutinee` is a union container and `targets` has a key defined for each of the
  ///   elements in scrutinee's type.
  func makeUnionSwitch(
    on scrutinee: Operand, toOneOf targets: UnionSwitch.Targets, at site: SourceRange
  ) -> UnionSwitch {
    let t = type(of: scrutinee)
    guard t.isAddress, let u = UnionType(t.ast) else {
      preconditionFailure("invalid type '\(t)'")
    }
    precondition(u.elements.allSatisfy({ (e) in targets[e] != nil }))

    return .init(scrutinee: scrutinee, targets: targets, site: site)
  }

}
