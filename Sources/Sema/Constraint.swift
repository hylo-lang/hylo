import AST

/// A type constraint.
///
/// A constraint is a basic block that serves to describe facts the properties and relationships of
/// the types of a program's statements and expressions.
public protocol Constraint {

  /// The locator of the constraint.
  var locator: ConstraintLocator? { get }

  /// The constraint's precedence.
  var precedence: Int { get }

  /// Returns whether this constraints directly depends on the specified variable.
  func depends(on tau: TypeVar) -> Bool

}

/// A relational type constraint `T ◇ U`, which relates two types.
public protocol RelationalConstraint: Constraint {

  /// A type.
  var lhs: ValType { get }

  /// Another type.
  var rhs: ValType { get }

}

/// A disjunction of two or more constraints.
struct DisjunctionConstraint: Constraint {

  typealias Element = (constraint: Constraint, weight: Int)

  init<S>(_ elements: S) where S: Sequence, S.Element == Element {
    self.elements = Array(elements)
  }

  let elements: [Element]

  var locator: ConstraintLocator? { nil }

  var precedence: Int { 1000 }

  func depends(on tau: TypeVar) -> Bool {
    return elements.contains(where: { elem in elem.constraint.depends(on: tau) })
  }

}

extension DisjunctionConstraint: CustomStringConvertible {

  var description: String {
    let elems = elements.map({ (elem) -> String in
      "(\(elem.constraint), \(elem.weight))"
    })
    return elems.joined(separator: " | ")
  }

}

/// A constraint `T == U` specifying that `T` is excatly the same type as `U`.
///
/// This is the only commutative relational constraint.
struct EqualityConstraint: RelationalConstraint {

  init(_ lhs: ValType, isEqualTo rhs: ValType, at locator: ConstraintLocator?) {
    assert(!(lhs is UnresolvedType) && !(rhs is UnresolvedType))

    self.lhs = lhs
    self.rhs = rhs
    self.locator = locator
  }

  init<T>(converting other: T) where T: RelationalConstraint {
    self.lhs = other.lhs
    self.rhs = other.rhs
    self.locator = other.locator
  }

  let lhs: ValType

  let rhs: ValType

  let locator: ConstraintLocator?

  var precedence: Int { 0 }

  func depends(on tau: TypeVar) -> Bool {
    return (lhs === tau) || (rhs === tau)
  }

}

extension EqualityConstraint: CustomStringConvertible {

  var description: String { "\(lhs) == \(rhs)" }

}

/// A constraint `T ≤ U` specifying that `T` is a subtype of `U`.
///
/// Subtyping is denoted by the notion of substitutability. If `T ≤ U` and `Γ, x: U ⊢ e: V`, then
/// `Γ, x: T ⊢ e: V'`.
struct SubtypingConstraint: RelationalConstraint {

  init(_ lhs: ValType, isSubtypeOf rhs: ValType, at locator: ConstraintLocator?) {
    assert(!(lhs is UnresolvedType) && !(rhs is UnresolvedType))

    self.lhs = lhs
    self.rhs = rhs
    self.locator = locator
  }

  init<T>(converting other: T) where T: RelationalConstraint {
    self.lhs = other.lhs
    self.rhs = other.rhs
    self.locator = other.locator
  }

  let lhs: ValType

  let rhs: ValType

  let locator: ConstraintLocator?

  var precedence: Int { 20 }

  func depends(on tau: TypeVar) -> Bool {
    return (lhs === tau) || (rhs === tau)
  }

}

extension SubtypingConstraint: CustomStringConvertible {

  var description: String { "\(lhs) ≤ \(rhs)" }

}

/// A constraint `T ⊏ U` specifying that `T` is expressible by `U`.
///
/// Type conversion subsumes subtying and additionally holds when `T` is expressible by `U`. The
/// latter requires that `T` conform to the `ExpressibleBy***` view corresponding to `U`.
struct ConversionConstraint: RelationalConstraint {

  init(_ lhs: ValType, convertsTo rhs: ValType, at locator: ConstraintLocator?) {
    assert(!(lhs is UnresolvedType) && !(rhs is UnresolvedType))

    self.lhs = lhs
    self.rhs = rhs
    self.locator = locator
  }

  init<T>(converting other: T) where T: RelationalConstraint {
    self.lhs = other.lhs
    self.rhs = other.rhs
    self.locator = other.locator
  }

  let lhs: ValType

  let rhs: ValType

  let locator: ConstraintLocator?

  var precedence: Int { 20 }

  func depends(on tau: TypeVar) -> Bool {
    return (lhs === tau) || (rhs === tau)
  }

}

extension ConversionConstraint: CustomStringConvertible {

  var description: String { "\(lhs) ⊏ \(rhs)" }

}

/// A constraint `T : V` specifying that `T` conforms to the view `V`.
struct ConformanceConstraint: Constraint {

  init(_ type: ValType, conformsTo view: ViewType, at locator: ConstraintLocator?) {
    assert(!(type is UnresolvedType))

    self.type = type
    self.view = view
    self.locator = locator
  }

  /// A type.
  let type: ValType

  /// A view type.
  let view: ViewType

  let locator: ConstraintLocator?

  var precedence: Int { 20 }

  func depends(on tau: TypeVar) -> Bool {
    return type === tau
  }

}

extension ConformanceConstraint: CustomStringConvertible {

  var description: String { "\(type) : \(view)" }

}

/// A constraint `T[.x] == U` specifying that `T` has a value member `x` with type `U`.
struct ValueMemberConstraint: Constraint {

  init(
    _ lhs: ValType,
    hasValueMember memberName: String,
    ofType rhs: ValType,
    at locator: ConstraintLocator?
  ) {
    assert(!(lhs is UnresolvedType) && !(rhs is UnresolvedType))

    self.lhs = lhs
    self.memberName = memberName
    self.rhs = rhs
    self.locator = locator
  }

  /// A type.
  let lhs: ValType

  /// A member name.
  let memberName: String

  /// Another type.
  let rhs: ValType

  let locator: ConstraintLocator?

  var precedence: Int { 10 }

  func depends(on tau: TypeVar) -> Bool {
    return (lhs === tau) || (rhs === tau)
  }

}

extension ValueMemberConstraint: CustomStringConvertible {

  var description: String { "\(lhs)[.\(memberName)] == \(rhs)" }

}
