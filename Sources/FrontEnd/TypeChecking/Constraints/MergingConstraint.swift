import Core
import Utils

/// A constraint `L :> (R1, ..., Rn)` specifying that `L` is the type of a conditional or match
/// expression whose branches have types `R1, ..., Rn`.
public struct MergingConstraint: Constraint, Hashable {

  /// The type of the expression.
  public private(set) var supertype: AnyType

  /// The types of the branches.
  public private(set) var branches: [AnyType]

  public let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  public init(_ supertype: AnyType, _ branches: [AnyType], origin: ConstraintOrigin) {
    self.supertype = supertype
    self.branches = branches
    self.origin = origin
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&supertype, with: transform)
    for i in 0 ..< branches.count {
      update(&branches[i], with: transform)
    }
  }

}

extension MergingConstraint: CustomStringConvertible {

  public var description: String { "\(supertype) :> \(list: branches)" }

}
