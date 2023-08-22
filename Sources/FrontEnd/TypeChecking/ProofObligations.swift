import Core

/// A set of formulae to be proven for type checking a declaration, expression, or pattern.
struct ProofObligations {

  /// The scope in which this set is defined.
  let scope: AnyScopeID

  /// A map from variable declaration to its type.
  private(set) var declType = DeclProperty<AnyType>()

  /// A map from expression to its type.
  private(set) var exprType = ExprProperty<AnyType>()

  /// A map from name expression to its declaration.
  private(set) var referredDecl: BindingMap = [:]

  /// A set of type constraints.
  private(set) var constraints: ConstraintSet = []

  /// `true` iff a this set cannot be discharged because.
  private(set) var isUnsatisfiable = false

  /// Assigns `true` to `self.setUnsatisfiable`.
  mutating func setUnsatisfiable() {
    self.isUnsatisfiable = true
  }

  /// Assigns `t` to `self.varType[d]`.
  mutating func assign(_ t: AnyType, to d: AnyDeclID) {
    let b = declType.updateValue(t, forKey: d)
    assert(b == nil, "non-monotonic update")
  }

  /// Assigns `t` to `self.exprType[e]`.
  mutating func assign(_ t: AnyType, to e: AnyExprID) {
    let b = exprType.updateValue(t, forKey: e)
    assert(b == nil, "non-monotonic update")
  }

  /// Assigns `r` to `self.referredDecl[n]`.
  mutating func assign(_ r: DeclReference, to n: NameExpr.ID) {
    let b = referredDecl.updateValue(r, forKey: n)
    assert(b == nil, "non-monotonic update")
  }

  /// Inserts `c` into this set.
  mutating func insert(_ c: Constraint) {
    constraints.insert(c)
  }

  /// Inserts `batch` into this set.
  mutating func insert(_ batch: ConstraintSet) {
    constraints.formUnion(batch)
  }

}
