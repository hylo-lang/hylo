import Core
import Utils

extension TypeChecker {

  /// A deferred type checking query on a node that should be applied after the types of its
  /// constituent parts have been inferred.
  ///
  /// This type is represents closures capturing the nodes on which they apply. For example:
  ///
  ///     let n: VarDecl.ID = foo()
  ///     let deferredQuery: DeferredQuery = { (c, s) in
  ///       c.checkDeferred(varDecl: n, s)
  ///     }
  typealias DeferredQuery = (_ checker: inout TypeChecker, _ solution: Solution) -> Bool

  /// The types inferred by constraint generation for the visited expressions, along with the
  /// constraints between these types.
  struct InferenceFacts {

    /// A map from visited expression to its inferred type.
    private(set) var inferredTypes = ExprProperty<AnyType>()

    /// A map from name expression to its inferred declaration.
    private(set) var inferredBindings: [NameExpr.ID: DeclReference] = [:]

    /// The set of type constraints between the types involved in the visited expressions.
    private(set) var constraints: [Constraint] = []

    /// True iff a constraint could not be solved.
    private(set) var foundConflict = false

    /// Creates an empty base of facts.
    fileprivate init() {}

    /// Assigns the error type to `subject` and returns the error type.
    fileprivate mutating func assignErrorType<ID: ExprID>(to subject: ID) -> AnyType {
      foundConflict = true
      let ty = AnyType.error
      inferredTypes[subject] = ty
      return ty
    }

    /// Assigns `type` to `subject`.
    fileprivate mutating func assign<ID: ExprID>(_ type: AnyType, to subject: ID) {
      inferredTypes[subject] = type
    }

    /// Assigns `n` to `r`.
    fileprivate mutating func assign(_ n: NameExpr.ID, to r: DeclReference) {
      inferredBindings[n] = r
    }

    /// Constrains `subject` to have type `inferredType` and returns either `inferredType` or the
    /// type currently assigned to `subject` in the AST.
    ///
    /// - Note: Unlike `assign(_:to:)`, this method doesn't override `inferredTypes[subject]` if
    ///   it isn't `nil` but creates an equality constraint instead.
    fileprivate mutating func constrain<ID: ExprID, T: TypeProtocol>(
      _ subject: ID,
      in ast: AST,
      toHaveType inferredType: T
    ) -> AnyType {
      if let ty = inferredTypes[subject] {
        if ty != inferredType {
          constraints.append(
            EqualityConstraint(
              ^inferredType, ty, origin: .init(.structural, at: ast[subject].site)))
        }
        return ty
      } else {
        let ty = ^inferredType
        inferredTypes[subject] = ty
        return ty
      }
    }

    /// Adds `constraint` to this instance.
    fileprivate mutating func append(_ constraint: Constraint) {
      constraints.append(constraint)
    }

    /// Adds `constraints` to this instance.
    fileprivate mutating func append<S: Sequence>(_ constraints: S) where S.Element == Constraint {
      self.constraints.append(contentsOf: constraints)
    }

    /// Indicates that a conflict has been found.
    fileprivate mutating func setConflictFound() {
      foundConflict = true
    }

  }

  /// The common state of all `inferTypes(...)` methods as they recursively visit the AST.
  private struct Context {

    /// Facts about the AST generated during constraint generation.
    var facts: InferenceFacts

    /// A set of type checking queries to be run after type inference.
    var deferred: [DeferredQuery]

  }

  // MARK: Expressions

  /// Knowing `subject` is shaped by `shape`, returns its inferred type along with constraints on
  /// its sub-expressions and deferred type checking requests.
  ///
  /// The returned type is not suitable to annotate the AST; it may contain open variables to be
  /// resolved by solving type constraints. Use `checkedType(of:shapedBy:in:)` to get the deduced
  /// type of an expression after type inference.
  ///
  /// - Parameters:
  ///   - subject: The expression whose type should be deduced.
  ///   - shape: The shape of the type `subject` is expected to have given top-bottom information
  ///     flow, or `nil` of such shape is unknown.
  mutating func inferredType(
    of subject: AnyExprID, shapedBy shape: AnyType?
  ) -> (type: AnyType, facts: InferenceFacts, deferred: [DeferredQuery]) {
    var s = Context(facts: .init(), deferred: [])
    if let t = exprTypes[subject] {
      s.facts.assign(t, to: subject)
    }

    let t = inferredType(of: subject, shapedBy: shape, updating: &s)
    return (t, s.facts, s.deferred)
  }

  /// Knowing `subject` is shaped by `shape`, returns its inferred type, updating `s` with
  /// inference facts and deferred type checking requests.
  private mutating func inferredType(
    of subject: AnyExprID, shapedBy shape: AnyType?,
    updating s: inout Context
  ) -> AnyType {
    defer { assert(s.facts.inferredTypes[subject] != nil) }

    switch subject.kind {
    case BooleanLiteralExpr.self:
      return inferredType(of: BooleanLiteralExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case CastExpr.self:
      return inferredType(of: CastExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case ConditionalExpr.self:
      return inferredType(of: ConditionalExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case ExistentialTypeExpr.self:
      return inferredType(of: ExistentialTypeExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case FloatLiteralExpr.self:
      return inferredType(of: FloatLiteralExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case FunctionCallExpr.self:
      return inferredType(of: FunctionCallExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case InoutExpr.self:
      return inferredType(of: InoutExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case IntegerLiteralExpr.self:
      return inferredType(of: IntegerLiteralExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case LambdaExpr.self:
      return inferredType(of: LambdaExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case MatchExpr.self:
      return inferredType(of: MatchExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case NameExpr.self:
      return inferredType(of: NameExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case PragmaLiteralExpr.self:
      return inferredType(of: PragmaLiteralExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case SequenceExpr.self:
      return inferredType(of: SequenceExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case StringLiteralExpr.self:
      return inferredType(of: StringLiteralExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case SubscriptCallExpr.self:
      return inferredType(of: SubscriptCallExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case TupleExpr.self:
      return inferredType(of: TupleExpr.ID(subject)!, shapedBy: shape, updating: &s)
    case TupleMemberExpr.self:
      return inferredType(of: TupleMemberExpr.ID(subject)!, shapedBy: shape, updating: &s)
    default:
      unexpected(subject, in: ast)
    }
  }

  private mutating func inferredType(
    of subject: BooleanLiteralExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    state.facts.constrain(subject, in: ast, toHaveType: ast.coreType("Bool")!)
  }

  private mutating func inferredType(
    of subject: CastExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    let syntax = ast[subject]

    // Realize the type to which the left operand should be converted.
    guard let target = realize(syntax.right)?.instance else {
      return state.facts.assignErrorType(to: subject)
    }

    let useScope = program[subject].scope
    let rhs = instantiate(target, in: useScope, cause: ConstraintOrigin(.cast, at: syntax.site))
    _ = state.facts.constrain(syntax.right, in: ast, toHaveType: MetatypeType(of: rhs.shape))
    state.facts.append(rhs.constraints)

    let lhs = syntax.left
    switch syntax.direction {
    case .down:
      // Note: constraining the type of the left operand to be above the right operand wouldn't
      // contribute any useful information to the constraint system.
      _ = inferredType(of: lhs, shapedBy: nil, updating: &state)

    case .up:
      // The type of the left operand must be statically known to subtype of the right operand.
      let lhsType = inferredType(of: lhs, shapedBy: ^TypeVariable(), updating: &state)
      state.facts.append(
        SubtypingConstraint(
          lhsType, rhs.shape,
          origin: ConstraintOrigin(.cast, at: syntax.site)))

    case .pointerConversion:
      // The left operand must be a `Builtin.ptr`. The right operand must be a remote type.
      if !(rhs.shape.base is RemoteType) {
        report(.error(invalidPointerConversionAt: ast[syntax.right].site))
        return state.facts.assignErrorType(to: subject)
      }

      let lhsType = inferredType(of: lhs, shapedBy: nil, updating: &state)
      state.facts.append(
        EqualityConstraint(
          lhsType, .builtin(.ptr),
          origin: ConstraintOrigin(.cast, at: syntax.site)))
    }

    // In any case, the expression is assumed to have the type denoted by the right operand.
    return state.facts.constrain(subject, in: ast, toHaveType: rhs.shape)
  }

  private mutating func inferredType(
    of subject: ConditionalExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    let syntax = ast[subject]

    // Visit the condition(s).
    let boolType = AnyType(ast.coreType("Bool")!)
    for item in syntax.condition {
      switch item {
      case .expr(let expr):
        // Condition must be Boolean.
        state.facts.assign(boolType, to: expr)
        _ = inferredType(of: expr, shapedBy: boolType, updating: &state)

      case .decl(let binding):
        if check(binding: binding).isError { state.facts.setConflictFound() }
      }
    }

    let firstBranch = inferredType(of: syntax.success, shapedBy: shape, updating: &state)
    let secondBranch = inferredType(of: syntax.failure, shapedBy: shape, updating: &state)

    let t = ^TypeVariable()
    state.facts.append(
      MergingConstraint(
        t, [firstBranch, secondBranch],
        origin: .init(.branchMerge, at: ast[subject].introducerSite)))
    return state.facts.constrain(subject, in: ast, toHaveType: t)
  }

  private mutating func inferredType(
    of subject: ExistentialTypeExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    // Inferring the type of an existential type expression is the equivalent to evaluating it.
    if let t = realize(existentialType: subject) {
      return state.facts.constrain(subject, in: ast, toHaveType: t)
    } else {
      return state.facts.assignErrorType(to: subject)
    }
  }

  private mutating func inferredType(
    of subject: FloatLiteralExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    let defaultType = ^ast.coreType("Float64")!
    return inferredType(
      ofLiteralExpr: subject, shapedBy: shape, defaultingTo: defaultType, updating: &state)
  }

  private mutating func inferredType(
    of subject: FunctionCallExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    let syntax = ast[subject]
    let callee = inferredType(
      ofCallee: syntax.callee, usedAs: .function, shapedBy: shape, updating: &state)

    // We failed to infer the type of the callee. We can stop here.
    if callee.isError {
      return state.facts.assignErrorType(to: subject)
    }

    // The callee has a callable type or we need inference to determine its type. Either way,
    // constraint the callee and its arguments with a function call constraints.
    if isArrow(callee) || (callee.base is TypeVariable) {
      return inferredType(of: subject, withCallee: callee, shapedBy: shape, updating: &state)
    }

    // In any other case, the callee is known to be not callable.
    report(.error(cannotCall: callee, as: .function, at: ast[syntax.callee].site))
    return state.facts.assignErrorType(to: subject)
  }

  /// Knowing `subject` has a callee of type `callee` and returns a value of type `shape`,
  /// returns its inferred type, updating `state`.
  private mutating func inferredType(
    of subject: FunctionCallExpr.ID,
    withCallee callee: AnyType,
    shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    var arguments: [CallConstraint.Argument] = []
    for a in ast[subject].arguments {
      let p = inferredType(of: a.value, shapedBy: ^TypeVariable(), updating: &state)
      arguments.append(.init(label: a.label, type: p, valueSite: ast[a.value].site))
    }

    let output = ((callee.base as? CallableType)?.output ?? shape) ?? ^TypeVariable()
    state.facts.append(
      CallConstraint(
        arrow: callee, takes: arguments, gives: output,
        origin: ConstraintOrigin(.callee, at: program[subject].callee.site)))

    return state.facts.constrain(subject, in: ast, toHaveType: output)
  }

  private mutating func inferredType(
    of subject: InoutExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    state.facts.constrain(
      subject, in: ast,
      toHaveType: inferredType(of: ast[subject].subject, shapedBy: shape, updating: &state))
  }

  private mutating func inferredType(
    of subject: IntegerLiteralExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    let t = ^ast.coreType("Int")!
    return inferredType(ofLiteralExpr: subject, shapedBy: shape, defaultingTo: t, updating: &state)
  }

  private mutating func inferredType(
    of subject: LambdaExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    let syntax = ast[subject]

    let subjectConventions: [AccessEffect]?
    if let s = shape?.base as? LambdaType {
      // Check that the underlying declaration is structurally compatible with the type.
      let requiredLabels = ast[ast[syntax.decl].parameters].map(\.label?.value)
      if requiredLabels.count != s.inputs.count {
        report(
          .error(
            expectedLambdaParameterCount: s.inputs.count, found: requiredLabels.count,
            at: ast[syntax.decl].introducerSite))
        return state.facts.assignErrorType(to: subject)
      }
      if !requiredLabels.elementsEqual(s.labels) {
        report(
          .error(
            labels: Array(requiredLabels), incompatibleWith: s.labels,
            at: ast[syntax.decl].introducerSite))
        return state.facts.assignErrorType(to: subject)
      }

      subjectConventions = s.inputs.map({ (p) in ParameterType(p.type)?.access ?? .let })
    } else {
      subjectConventions = nil
    }

    // Realize the type of the underlying declaration.
    guard
      let underlyingDeclType = LambdaType(
        realize(underlyingDeclOf: subject, with: subjectConventions))
    else {
      return state.facts.assignErrorType(to: subject)
    }

    // Schedule the underlying declaration to be type-checked.
    state.deferred.append({ (checker, solution) in
      checker.checkDeferred(lambdaExpr: subject, solution)
    })

    // If the underlying declaration's return type is a unknown, infer it from the lambda's body.
    let o = underlyingDeclType.output
    if o.base is TypeVariable {
      if case .expr(let body) = ast[syntax.decl].body {
        let e = inferredType(of: body, shapedBy: o, updating: &state)
        state.facts.append(SubtypingConstraint(e, o, origin: .init(.return, at: ast[body].site)))
      } else {
        report(.error(cannotInferComplexReturnTypeAt: ast[syntax.decl].introducerSite))
        return state.facts.assignErrorType(to: subject)
      }
    }

    return state.facts.constrain(subject, in: ast, toHaveType: underlyingDeclType)
  }

  private mutating func inferredType(
    of subject: MatchExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    let syntax = ast[subject]

    // Visit the subject of the match.
    let subjectType = inferredType(of: syntax.subject, shapedBy: nil, updating: &state)
    if subjectType.isError {
      return state.facts.assignErrorType(to: subject)
    }

    for c in syntax.cases {
      // Each pattern is expected to have the same type as the subject.
      let caseType = inferredType(of: ast[c].pattern, shapedBy: subjectType, updating: &state)
      if caseType.isError {
        return state.facts.assignErrorType(to: subject)
      }
    }

    return state.facts.constrain(subject, in: ast, toHaveType: AnyType.void)
  }

  private mutating func inferredType(
    of subject: NameExpr.ID,
    withImplicitDomain domain: AnyType? = nil,
    usedAs purpose: NameUse = .unapplied,
    shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    let resolution = resolve(subject, usedAs: purpose, withNonNominalPrefix: { (_, _) in nil })
    let unresolved: [NameExpr.ID]
    var lastVisited: AnyType?

    switch resolution {
    case .failed:
      return state.facts.assignErrorType(to: subject)

    case .inexecutable(let suffix):
      switch ast[suffix[0]].domain {
      case .implicit:
        if let t = domain {
          lastVisited = t
        } else {
          report(.error(noContextToResolve: ast[subject].name.value, at: ast[subject].name.site))
          return state.facts.assignErrorType(to: subject)
        }

      case .explicit(let e):
        lastVisited = inferredType(of: e, shapedBy: nil, updating: &state)

      case .none, .operand:
        unreachable()
      }
      unresolved = suffix

    case .done(let prefix, let suffix):
      unresolved = suffix
      for p in prefix {
        lastVisited = bind(p.component, to: p.candidates, updating: &state)
      }
    }

    // Create the necessary constraints to let the solver resolve the remaining components.
    for (i, component) in unresolved.enumerated() {
      let memberType = ^TypeVariable()
      state.facts.append(
        MemberConstraint(
          lastVisited!, hasMember: memberType, referredToBy: component, in: ast,
          usedAs: (i == unresolved.count - 1) ? purpose : .unapplied,
          origin: ConstraintOrigin(.member, at: ast[component].site)))
      lastVisited = state.facts.constrain(component, in: ast, toHaveType: memberType)
    }

    return lastVisited!
  }

  private mutating func inferredType(
    of subject: PragmaLiteralExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    switch program.ast[subject].kind {
    case .file:
      return state.facts.constrain(subject, in: ast, toHaveType: ast.coreType("String")!)
    case .line:
      return state.facts.constrain(subject, in: ast, toHaveType: ast.coreType("Int")!)
    }
  }

  private mutating func inferredType(
    of subject: SequenceExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    // Transform the sequence into a binary tree.
    guard let tree = fold(subject) else {
      return state.facts.assignErrorType(to: subject)
    }

    // Generate constraints from the folded sequence.
    let rootType = inferredType(of: tree, shapedBy: shape, updating: &state)
    return state.facts.constrain(subject, in: ast, toHaveType: rootType)
  }

  private mutating func inferredType(
    of subject: FoldedSequenceExpr, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    switch subject {
    case .infix(let callee, let lhs, let rhs):
      // Infer the types of the operands.
      let lhsType = inferredType(of: lhs, shapedBy: nil, updating: &state)
      let rhsType = inferredType(of: rhs, shapedBy: nil, updating: &state)

      if lhsType.isError || rhsType.isError {
        return .error
      }

      let operatorType = ^TypeVariable()
      let outputType = ^TypeVariable()
      state.facts.assign(operatorType, to: callee.expr)

      // The operator is a member function of the left operand.
      state.facts.append(
        MemberConstraint(
          lhsType, hasMember: operatorType, referredToBy: callee.expr, in: ast,
          usedAs: .unapplied,
          origin: ConstraintOrigin(.member, at: ast[callee.expr].site)))
      state.facts.append(
        CallConstraint(
          arrow: operatorType,
          takes: [.init(label: nil, type: rhsType, valueSite: ast.site(of: rhs))],
          gives: outputType,
          origin: ConstraintOrigin(.callee, at: ast.site(of: subject))))

      return outputType

    case .leaf(let expr):
      return inferredType(of: expr, shapedBy: shape, updating: &state)
    }
  }

  private mutating func inferredType(
    of subject: StringLiteralExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    state.facts.constrain(subject, in: ast, toHaveType: ast.coreType("String")!)
  }

  private mutating func inferredType(
    of subject: SubscriptCallExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    let syntax = ast[subject]
    let callee = inferredType(
      ofCallee: syntax.callee, usedAs: .subscript, shapedBy: shape, updating: &state)

    // We failed to infer the type of the callee. We can stop here.
    if callee.isError {
      return state.facts.assignErrorType(to: subject)
    }

    // The callee has a metatype and is a name expression bound to a nominal type declaration,
    // meaning that the call is actually a sugared buffer type expression.
    if isBoundToNominalTypeDecl(syntax.callee, in: state) {
      fatalError("not implemented")
    }

    // The callee has a callable type or we need inference to determine its type. Either way,
    // constraint the callee and its arguments with a function call constraints.
    if isSubscript(callee) || (callee.base is TypeVariable) {
      return inferredType(of: subject, withCallee: callee, shapedBy: shape, updating: &state)
    }

    // In any other case, the callee is known to be not callable.
    report(.error(cannotCall: callee, as: .subscript, at: ast[syntax.callee].site))
    return state.facts.assignErrorType(to: subject)
  }

  /// Knowing `subject` has a callee of type `callee` and projects a value of type `shape`,
  /// returns its inferred type, updating `state`.
  private mutating func inferredType(
    of subject: SubscriptCallExpr.ID,
    withCallee callee: AnyType,
    shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    var arguments: [CallConstraint.Argument] = []
    for a in ast[subject].arguments {
      let p = inferredType(of: a.value, shapedBy: ^TypeVariable(), updating: &state)
      arguments.append(.init(label: a.label, type: p, valueSite: ast[a.value].site))
    }

    let output = ((callee.base as? CallableType)?.output ?? shape) ?? ^TypeVariable()
    state.facts.append(
      CallConstraint(
        subscript: callee, takes: arguments, gives: output,
        origin: ConstraintOrigin(.callee, at: program[subject].callee.site)))

    return state.facts.constrain(subject, in: ast, toHaveType: output)
  }

  private mutating func inferredType(
    of subject: TupleExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    let elements = ast[subject].elements
    var elementTypes: [TupleType.Element] = []

    // If the expected type is a tuple compatible with the shape of the expression, propagate that
    // information down the expression tree. Otherwise, infer the type of the expression from the
    // leaves and use type constraints to detect potential mismatch.
    if let type = TupleType(shape),
      type.elements.elementsEqual(elements, by: { (a, b) in a.label == b.label?.value })
    {
      for (t, e) in zip(type.elements, elements) {
        let u = inferredType(of: e.value, shapedBy: t.type, updating: &state)
        elementTypes.append(.init(label: e.label?.value, type: u))
      }
    } else {
      for e in elements {
        let u = inferredType(of: e.value, shapedBy: nil, updating: &state)
        elementTypes.append(.init(label: e.label?.value, type: u))
      }
    }

    return state.facts.constrain(subject, in: ast, toHaveType: TupleType(elementTypes))
  }

  private mutating func inferredType(
    of subject: TupleMemberExpr.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    let s = inferredType(of: ast[subject].tuple, shapedBy: nil, updating: &state)
    let t = ^TypeVariable()
    let i = ast[subject].index
    state.facts.append(
      TupleMemberConstraint(s, at: i.value, hasType: t, origin: .init(.member, at: i.site)))
    return state.facts.constrain(subject, in: ast, toHaveType: t)
  }

  /// Returns the inferred type of `callee`, which is the callee of a function, initializer, or
  /// subscript called with `arguments`, updating `state` with inference facts and deferred type
  /// checking requests.
  private mutating func inferredType(
    ofCallee callee: AnyExprID, usedAs purpose: NameUse, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    assert(purpose != .unapplied)
    if let n = NameExpr.ID(callee) {
      return inferredType(
        of: n, withImplicitDomain: shape, usedAs: purpose, shapedBy: nil, updating: &state)
    } else {
      return inferredType(of: callee, shapedBy: nil, updating: &state)
    }
  }

  /// Returns the inferred type of `literal`, which is a literal expression, updating `state` with
  /// inference facts and deferred type checking requests.
  ///
  /// - Parameters:
  ///   - literal: A literal expression.
  ///   - shape: The shape of the type `subject` is expected to have given top-bottom information.
  ///   - defaultType: The type inferred for `literal` if the context isn't sufficient to deduce
  ///     another type.
  ///   - state: A collection of inference facts and deferred type checking requests.
  ///
  /// - Requires: `subject` is a literal expression.
  private mutating func inferredType<T: Expr>(
    ofLiteralExpr subject: T.ID, shapedBy shape: AnyType?,
    defaultingTo defaultType: AnyType,
    updating state: inout Context
  ) -> AnyType {
    // If there's shape, it must conform to `ExpressibleBy***Literal`. Otherwise, constrain the
    // subject to its default type.
    let cause = ConstraintOrigin(.literal, at: ast[subject].site)
    if let e = shape {
      // Fast path if `e` is the default type.
      if relations.areEquivalent(defaultType, e) {
        return state.facts.constrain(subject, in: ast, toHaveType: e)
      }

      // Assume the type of the subject is subtype of the default type. If it isn't, then assume it
      // is expressible by the literal's trait.
      let t = ^TypeVariable()
      let p = ast.coreTrait(forTypesExpressibleBy: T.self)!

      let preferred: ConstraintSet = [
        EqualityConstraint(t, defaultType, origin: cause),
        SubtypingConstraint(defaultType, e, origin: cause),
      ]
      let alternative: ConstraintSet = [
        EqualityConstraint(t, e, origin: cause),
        ConformanceConstraint(e, conformsTo: [p], origin: cause),
      ]

      state.facts.append(
        DisjunctionConstraint(
          between: [
            .init(constraints: preferred, penalties: 0),
            .init(constraints: alternative, penalties: 1),
          ],
          origin: cause))

      return state.facts.constrain(subject, in: ast, toHaveType: t)
    } else {
      return state.facts.constrain(subject, in: ast, toHaveType: defaultType)
    }
  }

  // MARK: Patterns

  /// Knowing `subject` is shaped by `shape`, returns its inferred type, updating `state` with
  /// inference facts and deferred type checking requests.
  mutating func inferredType(
    of subject: AnyPatternID, shapedBy shape: AnyType?
  ) -> (type: AnyType, facts: InferenceFacts, deferred: [DeferredQuery]) {
    var s = Context(facts: .init(), deferred: [])
    let t = inferredType(of: subject, shapedBy: shape, updating: &s)
    return (t, s.facts, s.deferred)
  }

  /// Knowing `subject` is shaped by `shape`, returns its inferred type along, updating `state`
  /// with inference facts and deferred type checking requests.
  private mutating func inferredType(
    of subject: AnyPatternID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    switch subject.kind {
    case BindingPattern.self:
      return inferredType(of: BindingPattern.ID(subject)!, shapedBy: shape, updating: &state)
    case ExprPattern.self:
      return inferredType(of: ExprPattern.ID(subject)!, shapedBy: shape, updating: &state)
    case NamePattern.self:
      return inferredType(of: NamePattern.ID(subject)!, shapedBy: shape, updating: &state)
    case TuplePattern.self:
      return inferredType(of: TuplePattern.ID(subject)!, shapedBy: shape, updating: &state)
    case WildcardPattern.self:
      return shape ?? ^TypeVariable()
    default:
      unreachable()
    }
  }

  private mutating func inferredType(
    of subject: BindingPattern.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    // A binding pattern introduces additional type information when it has a type annotation. In
    // that case, the type denoted by the annotation is used to infer the type of the sub-pattern
    // and constrained to be a subtype of the expected type, if any.
    var subpatternType = shape
    if let a = ast[subject].annotation {
      if let subjectType = realize(a)?.instance {
        if let t = shape {
          state.facts.append(
            SubtypingConstraint(
              subjectType, t,
              origin: ConstraintOrigin(.annotation, at: ast[subject].site)))

        }
        subpatternType = subjectType
      } else {
        return .error
      }
    }

    return inferredType(of: ast[subject].subpattern, shapedBy: subpatternType, updating: &state)
  }

  private mutating func inferredType(
    of subject: ExprPattern.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    return inferredType(of: ast[subject].expr, shapedBy: shape, updating: &state)
  }

  private mutating func inferredType(
    of subject: NamePattern.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    let nameDecl = ast[subject].decl
    state.deferred.append({ (checker, solution) in
      checker.checkDeferred(varDecl: nameDecl, solution)
    })

    if let t = declTypes[nameDecl] {
      // We can only get here if we're visiting the containing pattern more than once.
      return t
    } else {
      let nameType = shape ?? ^TypeVariable()
      setInferredType(nameType, for: nameDecl)
      return nameType
    }
  }

  private mutating func inferredType(
    of subject: TuplePattern.ID, shapedBy shape: AnyType?,
    updating state: inout Context
  ) -> AnyType {
    switch shape?.base {
    case let t as TupleType:
      // The pattern and the expected have a tuple shape.
      if t.elements.count != ast[subject].elements.count {
        // Invalid destructuring.
        report(.error(invalidDestructuringOfType: shape!, at: ast[subject].site))
        return .error
      }

      var lhs: [String?] = []
      var rhs: [String?] = []

      // Visit the elements pairwise.
      for (a, b) in zip(ast[subject].elements, t.elements) {
        let t = inferredType(of: a.pattern, shapedBy: b.type, updating: &state)
        if t.isError { return .error }
        lhs.append(a.label?.value)
        rhs.append(b.label)
      }

      // Check that labels match.
      if lhs != rhs {
        report(.error(labels: lhs, incompatibleWith: rhs, at: ast[subject].site))
        return .error
      }

      return shape!

    case is TypeVariable:
      // If the expected type is a variable, we can't infer anything more at this point.
      return shape!

    case .some:
      // If the expected type doesn't have a tuple shape, the pattern cannot match.
      report(.error(invalidDestructuringOfType: shape!, at: ast[subject].site))
      return .error

    case nil:
      // Infer the shape of the expected type.
      return ^TupleType(
        ast[subject].elements.map { (a) in
          let t = inferredType(of: a.pattern, shapedBy: nil, updating: &state)
          return .init(label: a.label?.value, type: t)
        })
    }
  }

  // MARK: Helpers

  /// Returns `true` iff `t` is known as an arrow type.
  private func isArrow(_ t: AnyType) -> Bool {
    (t.base as? CallableType)?.isArrow ?? false
  }

  /// Returns `true` iff `t` is known as a subscript type.
  private func isSubscript(_ t: AnyType) -> Bool {
    !isArrow(t)
  }

  /// Returns `true` iff `e` is a name assumed bound to a nominal type declaration in `state`.
  private mutating func isBoundToNominalTypeDecl(_ e: AnyExprID, in state: Context) -> Bool {
    guard
      let c = NameExpr.ID(e),
      let d = state.facts.inferredBindings[c]?.decl,
      isNominalTypeDecl(d)
    else { return false }
    return true
  }

  /// If the labels of `arguments` matches those of `parameters`, visit the arguments' expressions
  /// to generate their type constraints assuming they have the corresponding type in `parameters`
  /// and returns `true`. Otherwise, returns `false`.
  private mutating func parametersMatching(
    arguments: [LabeledArgument], of callee: AnyExprID,
    shapedBy parameters: [CallableTypeParameter],
    updating state: inout Context
  ) -> Bool {
    // Collect the argument and parameter labels.
    let argumentLabels = arguments.map(\.label?.value)
    let parameterLabels = parameters.map(\.label)

    // Check that the labels inferred from the callee are consistent with that of the call.
    if argumentLabels != parameterLabels {
      report(
        .error(labels: argumentLabels, incompatibleWith: parameterLabels, at: ast[callee].site))
      return false
    }

    // Create type constraints on arguments and parameters.
    for i in 0 ..< arguments.count {
      let argumentExpr = arguments[i].value

      // Infer the type of the argument, expecting it's the same as the parameter's bare type.
      let argumentType: AnyType
      if let t = ParameterType(parameters[i].type)?.bareType {
        argumentType = inferredType(of: argumentExpr, shapedBy: t, updating: &state)
        if relations.areEquivalent(t, argumentType) { continue }
      } else {
        argumentType = inferredType(of: argumentExpr, shapedBy: nil, updating: &state)
      }

      state.facts.append(
        ParameterConstraint(
          argumentType, parameters[i].type,
          origin: ConstraintOrigin(.argument, at: ast[argumentExpr].site)))
    }

    return true
  }

  /// Visit `arguments` to generate their type constraints and returns a matching parameter list.
  private mutating func parametersMatching(
    arguments: [LabeledArgument],
    updating state: inout Context
  ) -> [CallableTypeParameter] {
    var parameters: [CallableTypeParameter] = []
    parameters.reserveCapacity(arguments.count)

    for a in arguments {
      let p = inferredType(of: a.value, shapedBy: ^TypeVariable(), updating: &state)
      parameters.append(CallableTypeParameter(label: a.label?.value, type: p))
    }

    return parameters
  }

  /// Constrains `name` to be a reference to either of the declarations in `candidates`.
  ///
  /// - Requires: `candidates` is not empty
  private mutating func bind(
    _ name: NameExpr.ID,
    to candidates: [NameResolutionResult.Candidate],
    updating state: inout Context
  ) -> AnyType {
    precondition(!candidates.isEmpty)

    if let candidate = candidates.uniqueElement {
      // Bind the component to the resolved declaration and store its type.
      state.facts.assign(name, to: candidate.reference)
      state.facts.append(candidate.constraints)
      return state.facts.constrain(name, in: ast, toHaveType: candidate.type)
    } else {
      // Create an overload set.
      let overloads: [OverloadConstraint.Predicate] = candidates.map({ (candidate) in
        let p = candidate.reference.decl.map({ program.isRequirement($0) ? 1 : 0 }) ?? 0
        return .init(
          reference: candidate.reference,
          type: candidate.type,
          constraints: candidate.constraints,
          penalties: p)
      })

      // Constrain the name to refer to one of the overloads.
      let nameType = ^TypeVariable()
      state.facts.append(
        OverloadConstraint(
          name, withType: nameType, refersToOneOf: overloads,
          origin: ConstraintOrigin(.binding, at: ast[name].site)))
      return state.facts.constrain(name, in: ast, toHaveType: nameType)
    }
  }

}
