import AST

/// A type constraint.
public protocol Constraint {

  /// The locator of the constraint.
  var locator: ConstraintLocator? { get }

  /// The constraint's precedence.
  var precedence: Int { get }

  /// Returns whether this constraints directly depends on the specified variable.
  func depends(on tau: TypeVar) -> Bool

}

/// A disjunction of two or more constraints.
struct DisjunctionCons: Constraint {

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

extension DisjunctionCons: CustomStringConvertible {

  var description: String {
    let elems = elements.map({ (elem) -> String in
      "(\(elem.constraint), \(elem.weight))"
    })
    return elems.joined(separator: " | ")
  }

}

/// A constraint `T == U` specifying that `T` is excatly the same type as `U`.
struct EqualityCons: Constraint {

  init(_ first: ValType, isEqualTo second: ValType, at locator: ConstraintLocator?) {
    self.first = first
    self.second = second
    self.locator = locator
  }

  init(strengthening subtyping: SubtypingCons) {
    self.first = subtyping.first
    self.second = subtyping.second
    self.locator = subtyping.locator
  }

  /// A type.
  let first: ValType

  /// Another type
  let second: ValType

  let locator: ConstraintLocator?

  var precedence: Int { 0 }

  func depends(on tau: TypeVar) -> Bool {
    return (first === tau) || (second === tau)
  }

}

extension EqualityCons: CustomStringConvertible {

  var description: String { "\(first) == \(second)" }

}

/// A constraint `T ≤ U` specifying that `T` is a subtype of `U`.
///
/// Subtyping is denoted by the notion of substitutability. If `T ≤ U` and `Γ, x: U ⊢ e: V`, then
/// `Γ, x: T ⊢ e: V'`.
struct SubtypingCons: Constraint {

  init(_ first: ValType, isSubtypeOf second: ValType, at locator: ConstraintLocator?) {
    self.first = first
    self.second = second
    self.locator = locator
  }

  /// A type.
  let first: ValType

  /// Another type
  let second: ValType

  let locator: ConstraintLocator?

  var precedence: Int { 20 }

  func depends(on tau: TypeVar) -> Bool {
    return (first === tau) || (second === tau)
  }

}

extension SubtypingCons: CustomStringConvertible {

  var description: String { "\(first) ≤ \(second)" }

}

/// A constraint `T : V` specifying that `T` conforms to the view `V`.
struct ConformanceCons: Constraint {

  init(_ type: ValType, conformsTo view: ViewType, at locator: ConstraintLocator?) {
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

extension ConformanceCons: CustomStringConvertible {

  var description: String { "\(type) : \(view)" }

}

/// A constraint `T[.x] == U` specifying that `T` has a value member `x` with type `U`.
struct ValueMemberCons: Constraint {

  init(
    _ first: ValType,
    hasValueMember memberName: String,
    ofType second: ValType,
    at locator: ConstraintLocator?
  ) {
    self.first = first
    self.memberName = memberName
    self.second = second
    self.locator = locator
  }

  /// A type.
  let first: ValType

  /// A member name.
  let memberName: String

  /// Another type.
  let second: ValType

  let locator: ConstraintLocator?

  var precedence: Int { 10 }

  func depends(on tau: TypeVar) -> Bool {
    return (first === tau) || (second === tau)
  }

}

extension ValueMemberCons: CustomStringConvertible {

  var description: String { "\(first)[.\(memberName)] == \(second)" }

}
