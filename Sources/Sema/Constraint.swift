import AST

/// A type constraint.
///
/// A constraint is a basic block that serves to describe the properties and relationships of the
/// types of a program's declarations, statements and expressions.
protocol Constraint {

  /// The locator of the constraint.
  var locator: ConstraintLocator { get }

  /// The constraint's precedence.
  var precedence: Int { get }

  /// Returns whether this constraints directly depends on the specified variable.
  func depends(on tau: TypeVar) -> Bool

}

/// A relational type constraint `T ◇ U`, which relates two types.
struct RelationalConstraint: Constraint, CustomStringConvertible {

  init(kind: Kind, lhs: ValType, rhs: ValType, at locator: ConstraintLocator) {
    assert(!lhs.isUnresolved && !rhs.isUnresolved)
    assert(kind != .conformance || rhs is ViewType)
    assert(kind != .conversion  || rhs is BuiltinLiteral)

    self.kind = kind
    self.lhs = lhs
    self.rhs = rhs
    self.locator = locator
  }

  /// Creates a type constraint from prototype returned by generic environments.
  ///
  /// - Parameters:
  ///   - prototype: The prototype of a constraint on an opened generic parameter.
  ///   - locator: A locator for the constraint.
  init(prototype: GenericEnv.ConstraintPrototype, at locator: ConstraintLocator) {
    switch prototype.kind {
    case .equality:
      self.init(kind: .equality, lhs: prototype.lhs, rhs: prototype.rhs, at: locator)
    case .conformance:
      self.init(kind: .conformance, lhs: prototype.lhs, rhs: prototype.rhs, at: locator)
    }
  }

  /// A kind of relational constraint.
  enum Kind: Int {

    /// A constraint `T == U` specifying that `T` is exactly the same type as `U`.
    ///
    /// This is the only commutative relational constraint.
    case equality = 0

    /// A constraint `T : V` specifying that `T` conforms to the view `V`.
    ///
    /// The right hand side of a conformance constraint must always be a view.
    case conformance

    /// A constraint `T <: U` specifying that `T` is a subtype of `U`.
    ///
    /// Subtyping describes a notion of substitutability. If `T <: U` and `Γ, x: U ⊢ e: V`, then
    /// `Γ, x: T ⊢ e: V'`.
    ///
    /// For nominal types, subtyping is equivalent to view conformance: a value of type `T` can
    /// substituted for a value of type `U` if `T` conforms to all the views to which `U` conforms.
    /// Variance additionally describes how structural types relate to one another.
    case subtyping

    /// A constraint `T ⊏ U` specifying that `T` is expressible by `U`.
    ///
    /// Type conversion relates to literal expressions. It relaxes subtying by also including cases
    /// where `T` is *expressible* by `U`, i.e., when `T` conforms to the `ExpressibleBy***` view
    /// corresponding to a literal type `U`.
    case conversion

    /// A constraint `T == U` that requires `U` to be (partially) determined before the constraint
    /// is solved like a standard equality constraint.
    case oneWayEquality

  }

  /// The kind of relation described by the constraint.
  let kind: Kind

  /// A type.
  let lhs: ValType

  /// Another type.
  let rhs: ValType

  let locator: ConstraintLocator

  var precedence: Int { kind.rawValue }

  func depends(on tau: TypeVar) -> Bool {
    return (lhs === tau) || (rhs === tau)
  }

  var description: String {
    switch kind {
    case .equality, .oneWayEquality:
      return "\(lhs) == \(rhs)"
    case .subtyping:
      return "\(lhs) <: \(rhs)"
    case .conformance:
      return "\(lhs) : \(rhs)"
    case .conversion:
      return "\(lhs) ⊏ \(rhs)"
    }
  }

}

/// A constraint `T := {D1, ..., D2}` specifying that `T` is the type of an expression that refers
/// to a particular overloaded declaration `Di`.
///
/// This typically results from a reference to an overloaded symbol.
struct OverloadBindingConstraint: Constraint, CustomStringConvertible {

  init(
    _ type: ValType,
    declSet: [ValueDecl],
    useSite: DeclSpace,
    at locator: ConstraintLocator
  ) {
    self.type = type
    self.declSet = declSet
    self.useSite = useSite
    self.locator = locator
  }

  /// A type.
  let type: ValType

  /// A set of declaration to which `type` may bind.
  let declSet: [ValueDecl]

  /// The declaration space from which the declaration is being referred.
  let useSite: DeclSpace

  let locator: ConstraintLocator

  var precedence: Int { 1000 }

  func depends(on tau: TypeVar) -> Bool {
    return type === tau
  }

  var description: String {
    let decls = declSet
      .map({ decl in decl.debugID })
      .joined(separator: " | ")
    return "\(type) == " + decls
  }

}

/// A constraint `T[.x] == U` specifying that `T` has a value member `x` with type `U`.
struct ValueMemberConstraint: Constraint, CustomStringConvertible {

  init(
    _ lhs: ValType,
    hasValueMember memberName: String,
    ofType rhs: ValType,
    useSite: DeclSpace,
    at locator: ConstraintLocator
  ) {
    assert(!lhs.isUnresolved && !rhs.isUnresolved)

    self.lhs = lhs
    self.memberName = memberName
    self.rhs = rhs
    self.useSite = useSite
    self.locator = locator
  }

  /// A type.
  let lhs: ValType

  /// A member name.
  let memberName: String

  /// Another type.
  let rhs: ValType

  /// The declaration space from which the declaration is being referred.
  let useSite: DeclSpace

  let locator: ConstraintLocator

  var precedence: Int { 10 }

  func depends(on tau: TypeVar) -> Bool {
    return (lhs === tau) || (rhs === tau)
  }

  var description: String { "\(lhs)[.\(memberName)] == \(rhs)" }

}

/// A constraint `T[.i] == U` specifying that `T` is a tuple whose `i`-th element has type `U`.
struct TupleMemberConstraint: Constraint, CustomStringConvertible {

  init(
    _ lhs: ValType,
    hasMemberAt memberIndex: Int,
    ofType rhs: ValType,
    at locator: ConstraintLocator
  ) {
    assert(!lhs.isUnresolved && !rhs.isUnresolved)

    self.lhs = lhs
    self.memberIndex = memberIndex
    self.rhs = rhs
    self.locator = locator
  }

  /// A type.
  let lhs: ValType

  /// A member index.
  let memberIndex: Int

  /// Another type.
  let rhs: ValType

  let locator: ConstraintLocator

  var precedence: Int { 10 }

  func depends(on tau: TypeVar) -> Bool {
    return (lhs === tau) || (rhs === tau)
  }

  var description: String { "\(lhs)[.\(memberIndex)] == \(rhs)" }

}

/// A disjunction of two or more constraints.
struct DisjunctionConstraint: Constraint, CustomStringConvertible {

  typealias Element = (constraint: Constraint, weight: Int)

  init<S>(_ elements: S) where S: Sequence, S.Element == Element {
    self.elements = Array(elements)
  }

  let elements: [Element]

  var locator: ConstraintLocator { elements[0].constraint.locator }

  var precedence: Int { 1000 }

  func depends(on tau: TypeVar) -> Bool {
    return elements.contains(where: { elem in elem.constraint.depends(on: tau) })
  }

  var description: String {
    let elems = elements.map({ (elem) -> String in
      "(\(elem.constraint), \(elem.weight))"
    })
    return elems.joined(separator: " | ")
  }

}
