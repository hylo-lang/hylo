import Core
import Utils

/// A constraint `L : T` specifying that `L` conforms to trait `T`.
struct ConformanceConstraint: Constraint, Hashable {

  /// The type that must conform to `concept`.
  private(set) var model: AnyType

  /// The trait to which `model` must conform.
  let concept: TraitType

  let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  init(_ model: AnyType, conformsTo concept: TraitType, origin: ConstraintOrigin) {
    self.model = model
    self.concept = concept
    self.origin = origin
  }

  /// Inserts the type variables that occur free in `self` into `s`.
  func collectOpenVariables(in s: inout Set<TypeVariable>) {
    model.collectOpenVariables(in: &s)
  }

  mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&model, with: transform)
  }

}

extension ConformanceConstraint: CustomStringConvertible {

  var description: String {
    "\(model) : \(concept)"
  }

}
