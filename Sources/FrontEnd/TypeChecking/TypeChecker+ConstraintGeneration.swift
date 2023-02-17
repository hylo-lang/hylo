import Core
import Utils

extension TypeChecker {

  /// A deferred type checking query on a node that should be applied after the types of its
  /// constituent parts have been inferred.
  ///
  /// This type is meant to represent closures capturing the nodes on which they apply. For
  /// example:
  ///
  ///     let n: NodeID<VarDecl> = foo()
  ///     let deferredQuery: DeferredQuery = { (c, s) in
  ///       c.checkDeferred(varDecl: n, s)
  ///     }
  typealias DeferredQuery = (_ checker: inout TypeChecker, _ solution: Solution) -> Bool

  /// The types inferred by constraint generation for the visited expressions, along with the
  /// constraints between these types.
  struct InferenceFacts {

    /// A map from visited expression to its inferred type.
    private(set) var inferredTypes = ExprProperty<AnyType>()

    /// The set of type constraints between the types involved in the visited expressions.
    private(set) var constraints: [Constraint] = []

    /// True iff a constraint could not be solved.
    private(set) var foundConflict = false

    /// Creates a base of facts assigning `type` to `subject`.
    fileprivate init<ID: ExprID>(assigning type: AnyType, to subject: ID) {
      assign(type, to: subject)
    }

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
              ^inferredType, ty,
              because: ConstraintCause(.structural, at: ast[subject].site)))
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
  private typealias State = (facts: InferenceFacts, deferred: [DeferredQuery])

  // MARK: Expressions

  /// Knowing `subject` occurs in `scope` and is shaped by `shape`, returns its inferred type
  /// along with constraints on its sub-expressions and deferred type checking requests.
  ///
  /// The returned type is not suitable to annotate the AST; it may contain open variables to be
  /// resolved by solving type constraints. Use `checkedType(of:shapedBy:in:)` to get the deduced
  /// type of an expression after type inference.
  ///
  /// - Parameters:
  ///   - subject: The expression whose type should be deduced.
  ///   - shape: The shape of the type `subject` is expected to have given top-bottom information
  ///     flow, or `nil` of such shape is unknown.
  ///   - scope: The innermost scope containing `subject`.
  mutating func inferredType(
    of subject: AnyExprID,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID
  ) -> (type: AnyType, facts: InferenceFacts, deferred: [DeferredQuery]) {
    var s: State
    if let t = exprTypes[subject] {
      s = (facts: .init(assigning: t, to: subject), deferred: [])
    } else {
      s = (facts: .init(), deferred: [])
    }

    let t = inferredType(of: subject, shapedBy: shape, in: scope, updating: &s)
    return (t, s.facts, s.deferred)
  }

  /// Knowing `subject` occurs in `scope` and is shaped by `shape`, returns its inferred type,
  /// updating `state` with inference facts and deferred type checking requests.
  private mutating func inferredType(
    of subject: AnyExprID,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    defer { assert(state.facts.inferredTypes[subject] != nil) }

    switch subject.kind {
    case BooleanLiteralExpr.self:
      return inferredType(
        ofBooleanLiteralExpr: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case CastExpr.self:
      return inferredType(
        ofCastExpr: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case ConditionalExpr.self:
      return inferredType(
        ofConditionalExpr: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case FloatLiteralExpr.self:
      return inferredType(
        ofFloatLiteralExpr: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case FunctionCallExpr.self:
      return inferredType(
        ofFunctionCallExpr: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case InoutExpr.self:
      return inferredType(
        ofInoutExpr: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case IntegerLiteralExpr.self:
      return inferredType(
        ofIntegerLiteralExpr: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case LambdaExpr.self:
      return inferredType(
        ofLambdaExpr: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case MatchExpr.self:
      return inferredType(
        ofMatchExpr: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case NameExpr.self:
      return inferredType(
        ofNameExpr: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case SequenceExpr.self:
      return inferredType(
        ofSequenceExpr: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case SubscriptCallExpr.self:
      return inferredType(
        ofSubscriptCallExpr: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case TupleExpr.self:
      return inferredType(
        ofTupleExpr: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    default:
      unexpected(subject, in: ast)
    }
  }

  private mutating func inferredType(
    ofBooleanLiteralExpr subject: NodeID<BooleanLiteralExpr>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    state.facts.constrain(subject, in: ast, toHaveType: ast.coreType(named: "Bool")!)
  }

  private mutating func inferredType(
    ofCastExpr subject: NodeID<CastExpr>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    let syntax = ast[subject]

    // Realize the type to which the left operand should be converted.
    guard let target = realize(syntax.right, in: scope)?.instance else {
      return state.facts.assignErrorType(to: subject)
    }

    let rhs = instantiate(target, in: scope, cause: ConstraintCause(.cast, at: syntax.site))
    state.facts.append(rhs.constraints)

    let lhs = syntax.left
    switch syntax.kind {
    case .down:
      // Note: constraining the type of the left operand to be above the right operand wouldn't
      // contribute any useful information to the constraint system.
      _ = inferredType(of: lhs, shapedBy: nil, in: scope, updating: &state)

    case .up:
      // The type of the left operand must be statically known to subtype of the right operand.
      let lhsType = inferredType(of: lhs, shapedBy: ^TypeVariable(), in: scope, updating: &state)
      state.facts.append(
        SubtypingConstraint(
          lhsType, rhs.shape,
          because: ConstraintCause(.cast, at: syntax.site)))

    case .builtinPointerConversion:
      // The type of the left operand must be `Builtin.Pointer`.
      let lhsType = inferredType(of: lhs, shapedBy: nil, in: scope, updating: &state)
      state.facts.append(
        EqualityConstraint(
          lhsType, .builtin(.ptr),
          because: ConstraintCause(.cast, at: syntax.site)))
    }

    // In any case, the expression is assumed to have the type denoted by the right operand.
    return state.facts.constrain(subject, in: ast, toHaveType: rhs.shape)
  }

  private mutating func inferredType(
    ofConditionalExpr subject: NodeID<ConditionalExpr>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    let syntax = ast[subject]

    // Visit the condition(s).
    let boolType = AnyType(ast.coreType(named: "Bool")!)
    for item in syntax.condition {
      switch item {
      case .expr(let expr):
        // Condition must be Boolean.
        state.facts.assign(boolType, to: expr)
        _ = inferredType(of: expr, shapedBy: boolType, in: scope, updating: &state)

      case .decl(let binding):
        if !check(binding: binding) { state.facts.setConflictFound() }
      }
    }

    let t = ^TypeVariable()
    let firstBranch = inferredType(
      of: syntax.success, shapedBy: shape, in: scope, updating: &state)
    state.facts.append(
      SubtypingConstraint(firstBranch, t, because: .init(.branchMerge, at: ast[subject].site)))

    let secondBranch = inferredType(
      of: syntax.failure, shapedBy: shape, in: scope, updating: &state)
    state.facts.append(
      SubtypingConstraint(secondBranch, t, because: .init(.branchMerge, at: ast[subject].site)))

    return state.facts.constrain(subject, in: ast, toHaveType: t)
  }

  private mutating func inferredType(
    ofFloatLiteralExpr subject: NodeID<FloatLiteralExpr>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    let defaultType = ^ast.coreType(named: "Double")!
    return inferredType(
      ofLiteralExpr: subject, shapedBy: shape, defaultingTo: defaultType,
      in: scope, updating: &state)
  }

  private mutating func inferredType(
    ofFunctionCallExpr subject: NodeID<FunctionCallExpr>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    let syntax = ast[subject]

    // Infer the type of the callee.
    let calleeType = inferredType(of: syntax.callee, shapedBy: nil, in: scope, updating: &state)

    // The following cases must be considered:
    //
    // 1. We failed to infer the type of the callee. We can stop here.
    // 2. We couldn't infer the exact type of the callee and must rely on bottom-up inference.
    // 3. We determined the exact type of the callee, and:
    //   a. it's a lambda or method type. In that case, we can use the parameters to validate the
    //      arguments' labels and their types.
    //   b. it's a metatype and the callee is a name expression referring to a nominal type
    //      declaration. In that case, the call is actually a sugared buffer type expression.
    //   c. it's any other type. In that case the callee is not callable.

    // Case 1
    if calleeType.isError {
      return state.facts.assignErrorType(to: subject)
    }

    // Case 2
    if calleeType.base is TypeVariable {
      let parameters = parametersMatching(arguments: syntax.arguments, in: scope, updating: &state)
      let returnType = shape ?? ^TypeVariable()

      state.facts.append(
        FunctionCallConstraint(
          calleeType, takes: parameters, andReturns: returnType,
          because: ConstraintCause(.callee, at: ast[syntax.callee].site)))

      return state.facts.constrain(subject, in: ast, toHaveType: returnType)
    }

    // Case 3a
    if let callable = calleeType.base as? CallableType {
      if parametersMatching(
        arguments: syntax.arguments, of: syntax.callee, in: scope,
        shapedBy: callable.inputs, updating: &state)
      {
        return state.facts.constrain(subject, in: ast, toHaveType: callable.output)
      } else {
        return state.facts.assignErrorType(to: subject)
      }
    }

    // Case 3b
    if let c = NodeID<NameExpr>(syntax.callee),
      let d = referredDecls[c]?.decl,
      isNominalTypeDecl(d)
    {
      let instanceType = MetatypeType(calleeType)!.instance
      let initName = SourceRepresentable(
        value: Name(stem: "init", labels: ["self"] + syntax.arguments.map(\.label?.value)),
        range: ast[c].name.site)
      let initCandidates = resolve(
        initName, withArguments: [], memberOf: instanceType, from: scope)

      // We're done if we couldn't find any initializer.
      if initCandidates.isEmpty {
        _ = state.facts.assignErrorType(to: syntax.callee)
        return state.facts.assignErrorType(to: subject)
      }

      if let pick = initCandidates.uniqueElement {
        // Rebind the callee and constrain its type.
        let ctorType = LambdaType(constructorFormOf: .init(pick.type.shape)!)
        referredDecls[c] = pick.reference
        state.facts.assign(^ctorType, to: c)
        state.facts.append(pick.type.constraints)

        // Visit the arguments.
        if parametersMatching(
          arguments: syntax.arguments, of: syntax.callee, in: scope,
          shapedBy: ctorType.inputs, updating: &state)
        {
          return state.facts.constrain(subject, in: ast, toHaveType: ctorType.output)
        } else {
          return state.facts.assignErrorType(to: subject)
        }
      } else {
        fatalError("not implemented")
      }
    }

    // Case 3c
    report(
      .error(
        nonCallableType: state.facts.inferredTypes[syntax.callee]!,
        at: ast[syntax.callee].site))
    return state.facts.assignErrorType(to: subject)
  }

  private mutating func inferredType(
    ofInoutExpr subject: NodeID<InoutExpr>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    state.facts.constrain(
      subject, in: ast,
      toHaveType: inferredType(
        of: ast[subject].subject, shapedBy: shape, in: scope, updating: &state))
  }

  private mutating func inferredType(
    ofIntegerLiteralExpr subject: NodeID<IntegerLiteralExpr>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    let defaultType = ^ast.coreType(named: "Int")!
    return inferredType(
      ofLiteralExpr: subject, shapedBy: shape, defaultingTo: defaultType,
      in: scope, updating: &state)
  }

  private mutating func inferredType(
    ofLambdaExpr subject: NodeID<LambdaExpr>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
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
      if !requiredLabels.elementsEqual(s.inputs, by: { $0 == $1.label }) {
        report(
          .error(
            labels: Array(requiredLabels), incompatibleWith: s.inputs.map(\.label),
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
    if underlyingDeclType.output.base is TypeVariable {
      if case .expr(let body) = ast[syntax.decl].body {
        _ = inferredType(
          of: body, shapedBy: underlyingDeclType.output, in: AnyScopeID(syntax.decl),
          updating: &state)
      } else {
        report(.error(cannotInferComplexReturnTypeAt: ast[syntax.decl].introducerSite))
        return state.facts.assignErrorType(to: subject)
      }
    }

    return state.facts.constrain(subject, in: ast, toHaveType: underlyingDeclType)
  }

  private mutating func inferredType(
    ofMatchExpr subject: NodeID<MatchExpr>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    let syntax = ast[subject]

    // Visit the subject of the match.
    let subjectType = inferredType(of: syntax.subject, shapedBy: nil, in: scope, updating: &state)
    if subjectType.isError {
      return state.facts.assignErrorType(to: subject)
    }

    for c in syntax.cases {
      // Each pattern is expected to have the same type as the subject.
      let caseType = inferredType(
        of: ast[c].pattern, shapedBy: subjectType, in: scope, updating: &state)

      if caseType.isError {
        return state.facts.assignErrorType(to: subject)
      }
    }

    return state.facts.constrain(subject, in: ast, toHaveType: AnyType.void)
  }

  private mutating func inferredType(
    ofNameExpr subject: NodeID<NameExpr>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    let resolution = resolve(nominalPrefixOf: subject, from: scope)
    let unresolvedComponents: [NodeID<NameExpr>]
    var lastVisitedComponentType: AnyType?

    switch resolution {
    case .failed:
      return state.facts.assignErrorType(to: subject)

    case .inexecutable(let suffix):
      if case .expr(let domainExpr) = ast[subject].domain {
        lastVisitedComponentType = inferredType(
          of: domainExpr, shapedBy: nil, in: scope, updating: &state)
      } else {
        fatalError("not implemented")
      }
      unresolvedComponents = suffix

    case .done(let prefix, let suffix):
      assert(!prefix.isEmpty, "at least one name component should have been resolved")
      for p in prefix {
        lastVisitedComponentType = bind(p.component, to: p.candidates, updating: &state)
      }
      unresolvedComponents = suffix
    }

    // Create the necessary constraints to let the solver resolve the remaining components.
    for component in unresolvedComponents {
      let memberType = ^TypeVariable()
      state.facts.append(
        MemberConstraint(
          lastVisitedComponentType!, hasMemberReferredToBy: component, ofType: memberType,
          in: ast,
          because: ConstraintCause(.member, at: ast[component].site)))
      lastVisitedComponentType = state.facts.constrain(component, in: ast, toHaveType: memberType)
    }

    if let e = shape {
      appendEquality(
        lastVisitedComponentType!, e, causedBy: .init(.binding, at: ast[subject].site),
        to: &state.facts)
    }
    return lastVisitedComponentType!
  }

  private mutating func inferredType(
    ofSequenceExpr subject: NodeID<SequenceExpr>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    // Fold the sequence and visit its sub-expressions.
    let foldedSequence = fold(sequenceExpr: subject, in: scope)
    foldedSequenceExprs[subject] = foldedSequence

    // Generate constraints from the folded sequence.
    let rootType = inferredType(
      ofSequenceExpr: foldedSequence, shapedBy: shape, in: scope, updating: &state)
    return state.facts.constrain(subject, in: ast, toHaveType: rootType)
  }

  private mutating func inferredType(
    ofSequenceExpr subject: FoldedSequenceExpr,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    switch subject {
    case .infix(let callee, let lhs, let rhs):
      // Infer the types of the operands.
      let lhsType = inferredType(ofSequenceExpr: lhs, shapedBy: nil, in: scope, updating: &state)
      let rhsType = inferredType(ofSequenceExpr: rhs, shapedBy: nil, in: scope, updating: &state)

      if lhsType.isError || rhsType.isError {
        return .error
      }

      // Infer the type of the callee.
      let parameterType = ^TypeVariable()
      state.facts.append(
        ParameterConstraint(
          rhsType, parameterType,
          because: ConstraintCause(.argument, at: ast.site(of: rhs))))

      let outputType = ^TypeVariable()
      let calleeType = LambdaType(
        receiverEffect: .let,
        environment: ^TupleType(labelsAndTypes: [("self", ^RemoteType(.let, lhsType))]),
        inputs: [CallableTypeParameter(type: parameterType)],
        output: outputType)
      state.facts.assign(^calleeType, to: callee.expr)

      // Create a member constraint for the operator.
      state.facts.append(
        MemberConstraint(
          lhsType, hasMemberReferredToBy: callee.expr, ofType: ^calleeType,
          in: ast,
          because: ConstraintCause(.member, at: ast[callee.expr].site)))

      return outputType

    case .leaf(let expr):
      return inferredType(of: expr, shapedBy: shape, in: scope, updating: &state)
    }
  }

  private mutating func inferredType(
    ofSubscriptCallExpr subject: NodeID<SubscriptCallExpr>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    let syntax = ast[subject]

    // Infer the type of the callee.
    let calleeType = inferredType(of: syntax.callee, shapedBy: nil, in: scope, updating: &state)

    // The following cases must be considered:
    //
    // 1. We failed to infer the type of the callee. We can stop here.
    // 2. We couldn't infer the exact type of the callee and must rely on bottom-up inference.
    // 3. We determined the exact type of the callee, and:
    //   a. it's a subscript type. In that case, we can use the parameters to validate the
    //      arguments' labels and infer their types.
    //   b. it's a metatype and the the callee is a name expression referring to a nominal type
    //      declaration. In that case, the call is actually a sugared buffer type expression.
    //   c. it's any other type. In that case we must look for an unnamed member subscript in that
    //      type and use it at the callee's type.

    // Case 1
    if state.facts.inferredTypes[syntax.callee]!.isError {
      return state.facts.assignErrorType(to: subject)
    }

    // Case 2
    if calleeType.base is TypeVariable {
      let parameters = parametersMatching(arguments: syntax.arguments, in: scope, updating: &state)
      let returnType = shape ?? ^TypeVariable()
      let assumedCalleeType = SubscriptImplType(
        isProperty: false,
        receiverEffect: nil,
        environment: ^TypeVariable(),
        inputs: parameters,
        output: returnType)

      state.facts.append(
        EqualityConstraint(
          calleeType, ^assumedCalleeType,
          because: ConstraintCause(.callee, at: ast[syntax.callee].site)))

      return state.facts.constrain(subject, in: ast, toHaveType: returnType)
    }

    // Case 3a
    if let callable = SubscriptType(state.facts.inferredTypes[syntax.callee]!) {
      if parametersMatching(
        arguments: syntax.arguments, of: syntax.callee, in: scope,
        shapedBy: callable.inputs, updating: &state)
      {
        return state.facts.constrain(subject, in: ast, toHaveType: callable.output)
      } else {
        return state.facts.assignErrorType(to: subject)
      }
    }

    // Case 3b
    if let c = NodeID<NameExpr>(syntax.callee),
      let d = referredDecls[c]?.decl,
      isNominalTypeDecl(d)
    {
      assert(calleeType.base is MetatypeType)

      // Buffer type expressions shall have exactly one argument.
      if syntax.arguments.count != 1 {
        report(.error(invalidBufferTypeExprArgumentCount: subject, in: ast))
        return state.facts.assignErrorType(to: subject)
      }

      // Note: We'll need some form of compile-time evaluation here.
      fatalError("not implemented")
    }

    // Case 3c
    let candidates = lookup(
      "[]", memberOf: state.facts.inferredTypes[syntax.callee]!, in: scope)
    switch candidates.count {
    case 0:
      report(
        .error(
          noUnnamedSubscriptsIn: state.facts.inferredTypes[syntax.callee]!,
          at: ast[syntax.callee].site))
      return state.facts.assignErrorType(to: subject)

    case 1:
      // If there's a single candidate, we're looking at case 3a.
      let decl = candidates.first!
      let declType = realize(decl: decl)
      assert(decl.kind == SubscriptDecl.self)

      // Bail out if we can't get the type of the referred declaration.
      if declType.isError {
        return state.facts.assignErrorType(to: subject)
      }

      // Contextualize the type of the referred declaration.
      let instantiatedType = instantiate(
        declType, in: scope,
        cause: ConstraintCause(.callee, at: ast[syntax.callee].site))

      // Visit the arguments.
      let calleeType = SubscriptType(instantiatedType.shape)!
      if parametersMatching(
        arguments: syntax.arguments, of: syntax.callee, in: scope,
        shapedBy: calleeType.inputs, updating: &state)
      {
        // Register the callee's constraints.
        state.facts.append(instantiatedType.constraints)

        // Update the referred declaration map if necessary.
        if let c = NodeID<NameExpr>(syntax.callee) {
          referredDecls[c] = .member(decl)
        }

        return state.facts.constrain(subject, in: ast, toHaveType: calleeType.output)
      } else {
        return state.facts.assignErrorType(to: subject)
      }

    default:
      // Note: Create an overload constraint.
      fatalError("not implemented")
    }
  }

  private mutating func inferredType(
    ofTupleExpr subject: NodeID<TupleExpr>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    let elements = ast[subject].elements
    var elementTypes: [TupleType.Element] = []

    // If the expected type is a tuple compatible with the shape of the expression, propagate that
    // information down the expression tree. Otherwise, infer the type of the expression from the
    // leaves and use type constraints to detect potential mismatch.
    if let type = TupleType(shape),
      type.elements.elementsEqual(elements, by: { (a, b) in a.label == b.label?.value })
    {
      for i in 0 ..< elements.count {
        let elementType = inferredType(
          of: elements[i].value, shapedBy: type.elements[i].type, in: scope, updating: &state)
        elementTypes.append(.init(label: elements[i].label?.value, type: elementType))
      }
    } else {
      for i in 0 ..< elements.count {
        let elementType = inferredType(
          of: elements[i].value, shapedBy: nil, in: scope, updating: &state)
        elementTypes.append(.init(label: elements[i].label?.value, type: elementType))
      }
    }

    return state.facts.constrain(subject, in: ast, toHaveType: TupleType(elementTypes))
  }

  /// Returns the inferred type of `literal`, updating `state` with inference facts and deferred
  /// type checking requests.
  ///
  /// - Parameters:
  ///   - literal: A literal expression.
  ///   - shape: The shape of the type `subject` is expected to have given top-bottom information.
  ///   - defaultType: The type inferred for `literal` if the context isn't sufficient to deduce
  ///     another type.
  ///   - scope: The innermost scope in which `literal` occurs.
  ///   - state: A collection of inference facts and deferred type checking requests.
  ///
  /// - Requires: `subject` is a literal expression.
  private mutating func inferredType<T: Expr>(
    ofLiteralExpr subject: NodeID<T>,
    shapedBy shape: AnyType?,
    defaultingTo defaultType: AnyType,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    // If there's shape, it must conform to `ExpressibleBy***Literal`. Otherwise, constrain the
    // subject to its default type.
    let cause = ConstraintCause(.literal, at: ast[subject].site)
    if let e = shape {
      if !relations.areEquivalent(defaultType, e) {
        let literalTrait = ast.coreTrait(forTypesExpressibleBy: T.self)!
        state.facts.append(
          LiteralConstraint(e, defaultsTo: defaultType, conformsTo: literalTrait, because: cause))
      }
      return state.facts.constrain(subject, in: ast, toHaveType: e)
    } else {
      return state.facts.constrain(subject, in: ast, toHaveType: defaultType)
    }
  }

  // MARK: Patterns

  /// Knowing `subject` occurs in `scope` and is shaped by `shape`, returns its inferred type,
  /// updating `state` with inference facts and deferred type checking requests.
  mutating func inferredType(
    of subject: AnyPatternID,
    in scope: AnyScopeID,
    shapedBy shape: AnyType?
  ) -> (type: AnyType, facts: InferenceFacts, deferred: [DeferredQuery]) {
    var s: State = (facts: .init(), deferred: [])
    let t = inferredType(of: subject, shapedBy: shape, in: scope, updating: &s)
    return (t, s.facts, s.deferred)
  }

  /// Knowing `subject` occurs in `scope` and is shaped by `shape`, returns its inferred type
  /// along, updating `state` with inference facts and deferred type checking requests.
  private mutating func inferredType(
    of subject: AnyPatternID,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    switch subject.kind {
    case BindingPattern.self:
      return inferredType(
        ofBindingPattern: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case ExprPattern.self:
      return inferredType(
        ofExprPattern: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case NamePattern.self:
      return inferredType(
        ofNamePattern: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case TuplePattern.self:
      return inferredType(
        ofTuplePattern: NodeID(subject)!, shapedBy: shape, in: scope, updating: &state)
    case WildcardPattern.self:
      return shape ?? ^TypeVariable()
    default:
      unreachable()
    }
  }

  private mutating func inferredType(
    ofBindingPattern subject: NodeID<BindingPattern>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    // A binding pattern introduces additional type information when it has a type annotation. In
    // that case, the type denoted by the annotation is used to infer the type of the sub-pattern
    // and constrained to be a subtype of the expected type, if any.
    var subpatternType = shape
    if let a = ast[subject].annotation {
      if let subjectType = realize(a, in: scope)?.instance {
        if let t = shape {
          state.facts.append(
            SubtypingConstraint(
              subjectType, t,
              because: ConstraintCause(.annotation, at: ast[subject].site)))

        }
        subpatternType = subjectType
      } else {
        return .error
      }
    }

    return inferredType(
      of: ast[subject].subpattern, shapedBy: subpatternType, in: scope, updating: &state)
  }

  private mutating func inferredType(
    ofExprPattern subject: NodeID<ExprPattern>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    inferredType(of: ast[subject].expr, shapedBy: shape, in: scope, updating: &state)
  }

  private mutating func inferredType(
    ofNamePattern subject: NodeID<NamePattern>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
  ) -> AnyType {
    let nameDecl = ast[subject].decl
    let nameType = shape ?? ^TypeVariable()
    setInferredType(nameType, for: nameDecl)
    state.deferred.append({ (checker, solution) in
      checker.checkDeferred(varDecl: nameDecl, solution)
    })

    return nameType
  }

  private mutating func inferredType(
    ofTuplePattern subject: NodeID<TuplePattern>,
    shapedBy shape: AnyType?,
    in scope: AnyScopeID,
    updating state: inout State
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
        let t = inferredType(of: a.pattern, shapedBy: b.type, in: scope, updating: &state)
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
          .init(
            label: a.label?.value,
            type: inferredType(of: a.pattern, shapedBy: nil, in: scope, updating: &state))
        })
    }
  }

  // MARK: Helpers

  /// Adds an equality constraint between `l` and `r` caused by `c` to `facts`.
  private mutating func appendEquality(
    _ l: AnyType, _ r: AnyType, causedBy c: ConstraintCause,
    to facts: inout InferenceFacts
  ) {
    let a = relations.canonical(l)
    let b = relations.canonical(r)
    if a == b {
      return
    } else if !a[.hasVariable] && !b[.hasVariable] {
      report(.error(type: l, incompatibleWith: r, at: c.site))
      facts.setConflictFound()
    } else {
      facts.append(EqualityConstraint(l, r, because: c))
    }
  }

  /// If the labels of `arguments` matches those of `parameters`, visit the arguments' expressions
  /// to generate their type constraints assuming they have the corresponding type in `parameters`
  /// and returns `true`. Otherwise, returns `false`.
  private mutating func parametersMatching(
    arguments: [LabeledArgument],
    of callee: AnyExprID,
    in scope: AnyScopeID,
    shapedBy parameters: [CallableTypeParameter],
    updating state: inout State
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
        argumentType = inferredType(of: argumentExpr, shapedBy: t, in: scope, updating: &state)
        if relations.areEquivalent(t, argumentType) { continue }
      } else {
        argumentType = inferredType(of: argumentExpr, shapedBy: nil, in: scope, updating: &state)
      }

      state.facts.append(
        ParameterConstraint(
          argumentType, parameters[i].type,
          because: ConstraintCause(.argument, at: ast[argumentExpr].site)))
    }

    return true
  }

  /// Visit `arguments` to generate their type constraints and returns a matching parameter list.
  private mutating func parametersMatching(
    arguments: [LabeledArgument],
    in scope: AnyScopeID,
    updating state: inout State
  ) -> [CallableTypeParameter] {
    var parameters: [CallableTypeParameter] = []
    parameters.reserveCapacity(arguments.count)

    for i in 0 ..< arguments.count {
      let argumentExpr = arguments[i].value
      let parameterType = ^TypeVariable()

      // Infer the type of the argument bottom-up.
      let argumentType = inferredType(
        of: argumentExpr, shapedBy: ^TypeVariable(), in: scope, updating: &state)

      state.facts.append(
        ParameterConstraint(
          argumentType, parameterType,
          because: ConstraintCause(.argument, at: ast[argumentExpr].site)))

      let argumentLabel = arguments[i].label?.value
      parameters.append(CallableTypeParameter(label: argumentLabel, type: parameterType))
    }

    return parameters
  }

  /// Constrains `name` to be a reference to either of the declarations in `candidates`.
  ///
  /// - Requires: `candidates` is not empty
  private mutating func bind(
    _ name: NodeID<NameExpr>,
    to candidates: [TypeChecker.NameResolutionResult.Candidate],
    updating state: inout State
  ) -> AnyType {
    precondition(!candidates.isEmpty)

    if let candidate = candidates.uniqueElement {
      // Bind the component to the resolved declaration and store its type.
      referredDecls[name] = candidate.reference
      state.facts.append(candidate.type.constraints)
      return state.facts.constrain(name, in: ast, toHaveType: candidate.type.shape)
    } else {
      // Create an overload set.
      let overloads: [OverloadConstraint.Candidate] = candidates.map({ (candidate) in
        return .init(
          reference: candidate.reference,
          type: candidate.type.shape,
          constraints: candidate.type.constraints,
          penalties: 0)
      })

      // Constrain the name to refer to one of the overloads.
      let nameType = ^TypeVariable()
      state.facts.append(
        OverloadConstraint(
          name, withType: nameType, refersToOneOf: overloads,
          because: ConstraintCause(.binding, at: ast[name].site)))
      return state.facts.constrain(name, in: ast, toHaveType: nameType)
    }
  }

  /// Folds a sequence of binary expressions.
  private mutating func fold(
    sequenceExpr expr: NodeID<SequenceExpr>,
    in scope: AnyScopeID
  ) -> FoldedSequenceExpr {
    let syntax = ast[expr]
    return fold(sequenceExprTail: syntax.tail[0...], into: .leaf(syntax.head), in: scope)
  }

  /// Folds the remainder of a sequence of binary expressions into `initialResult`.
  private mutating func fold(
    sequenceExprTail tail: ArraySlice<SequenceExpr.TailElement>,
    into initialResult: FoldedSequenceExpr,
    in scope: AnyScopeID
  ) -> FoldedSequenceExpr {
    var accumulator = initialResult

    for i in tail.indices {
      // Search for the operator declaration.
      let operatorStem = ast[tail[i].operator].name.value.stem
      let candidates = lookup(operator: operatorStem, notation: .infix, in: scope)

      switch candidates.count {
      case 0:
        report(.error(undefinedOperator: operatorStem, at: ast[tail[i].operator].site))
        accumulator.append(
          operator: (expr: tail[i].operator, precedence: nil),
          right: tail[i].operand)

      case 1:
        let precedence = ast[candidates[0]].precedenceGroup?.value
        accumulator.append(
          operator: (expr: tail[i].operator, precedence: precedence),
          right: tail[i].operand)

      default:
        // TODO: should probably emit a diagnostic. Operator declarations cannot be overloaded.
        fatalError("not implemented")
      }
    }

    return accumulator
  }

}
