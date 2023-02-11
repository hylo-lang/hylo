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

  /// Knowing `subject` occurs in `scope` and has a type compatible with `expectedType`, returns
  /// its inferred type with constraints on its sub-expressions and the nodes visited for which
  /// type checking has been deferred.
  mutating func inferType(
    of subject: AnyExprID,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?
  ) -> (type: AnyType, facts: InferenceFacts, deferred: [DeferredQuery]) {
    var s: State
    if let t = exprTypes[subject] {
      s = (facts: .init(assigning: t, to: subject), deferred: [])
    } else {
      s = (facts: .init(), deferred: [])
    }

    let t = inferType(
      of: subject, in: AnyScopeID(scope), expecting: expectedType, updating: &s)
    return (t, s.facts, s.deferred)
  }

  /// Returns the type of `subject` given it occurs in `scope`, using `expectedType` to propagate
  /// top-bottom type inference, and updating `state` with inference facts and nodes for which
  /// type checking has been deferred.
  private mutating func inferType(
    of subject: AnyExprID,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    defer { assert(state.facts.inferredTypes[subject] != nil) }

    switch subject.kind {
    case BooleanLiteralExpr.self:
      return inferType(
        ofBooleanLiteralExpr: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case CastExpr.self:
      return inferType(
        ofCastExpr: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case CondExpr.self:
      return inferType(
        ofConditionalExpr: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case FunctionCallExpr.self:
      return inferType(
        ofFunctionCallExpr: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case InoutExpr.self:
      return inferType(
        ofInoutExpr: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case IntegerLiteralExpr.self:
      return inferType(
        ofIntegerLiteralExpr: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case LambdaExpr.self:
      return inferType(
        ofLambdaExpr: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case MatchExpr.self:
      return inferType(
        ofMatchExpr: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case NameExpr.self:
      return inferType(
        ofNameExpr: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case SequenceExpr.self:
      return inferType(
        ofSequenceExpr: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case SubscriptCallExpr.self:
      return inferType(
        ofSubscriptCallExpr: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case TupleExpr.self:
      return inferType(
        ofTupleExpr: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    default:
      unexpected(subject, in: program.ast)
    }
  }

  private mutating func inferType(
    ofBooleanLiteralExpr subject: NodeID<BooleanLiteralExpr>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    state.facts.constrain(
      subject, in: program.ast, toHaveType: program.ast.coreType(named: "Bool")!)
  }

  private mutating func inferType(
    ofCastExpr subject: NodeID<CastExpr>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    let syntax = program.ast[subject]

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
      _ = inferType(of: lhs, in: scope, expecting: nil, updating: &state)

    case .up:
      // The type of the left operand must be statically known to subtype of the right operand.
      let lhsType = inferType(
        of: lhs, in: scope, expecting: ^TypeVariable(node: lhs.base), updating: &state)
      state.facts.append(
        SubtypingConstraint(
          lhsType, rhs.shape,
          because: ConstraintCause(.cast, at: syntax.site)))

    case .builtinPointerConversion:
      // The type of the left operand must be `Builtin.Pointer`.
      let lhsType = inferType(of: lhs, in: scope, expecting: nil, updating: &state)
      state.facts.append(
        EqualityConstraint(
          lhsType, .builtin(.ptr),
          because: ConstraintCause(.cast, at: syntax.site)))
    }

    // In any case, the expression is assumed to have the type denoted by the right operand.
    return state.facts.constrain(subject, in: program.ast, toHaveType: rhs.shape)
  }

  private mutating func inferType(
    ofConditionalExpr subject: NodeID<CondExpr>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    let syntax = program.ast[subject]

    // Visit the condition(s).
    let boolType = AnyType(program.ast.coreType(named: "Bool")!)
    for item in syntax.condition {
      switch item {
      case .expr(let expr):
        // Condition must be Boolean.
        state.facts.assign(boolType, to: expr)
        _ = inferType(of: expr, in: scope, expecting: boolType, updating: &state)

      case .decl(let binding):
        if !check(binding: binding) { state.facts.setConflictFound() }
      }
    }

    // Assume the node represents an expression if both branches are single expressions.
    let successType: AnyType?

    // Visit the success branch.
    switch syntax.success {
    case .expr(let expr):
      successType = inferType(of: expr, in: scope, expecting: expectedType, updating: &state)

    case .block(let branch):
      if !check(brace: branch) { state.facts.setConflictFound() }
      successType = nil
    }

    // Visit the failure branch.
    switch syntax.failure {
    case .expr(let expr):
      let failureType = inferType(of: expr, in: scope, expecting: expectedType, updating: &state)
      if let successType = successType {
        // Both branches are single expressions.
        state.facts.append(
          EqualityConstraint(
            successType, failureType,
            because: ConstraintCause(.branchMerge, at: syntax.site)))
        return state.facts.constrain(subject, in: program.ast, toHaveType: successType)
      }

    case .block(let branch):
      if !check(brace: branch) { state.facts.setConflictFound() }

    case nil:
      break
    }

    return state.facts.constrain(subject, in: program.ast, toHaveType: AnyType.void)
  }

  private mutating func inferType(
    ofFunctionCallExpr subject: NodeID<FunctionCallExpr>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    let syntax = program.ast[subject]

    // Infer the type of the callee.
    let calleeType = inferType(of: syntax.callee, in: scope, expecting: nil, updating: &state)

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
      let returnType = expectedType ?? ^TypeVariable(node: AnyNodeID(subject))

      state.facts.append(
        FunctionCallConstraint(
          calleeType, takes: parameters, andReturns: returnType,
          because: ConstraintCause(.callee, at: program.ast[syntax.callee].site)))

      return state.facts.constrain(subject, in: program.ast, toHaveType: returnType)
    }

    // Case 3a
    if let callable = calleeType.base as? CallableType {
      if parametersMatching(
        arguments: syntax.arguments, of: syntax.callee, in: scope,
        expecting: callable.inputs, updating: &state)
      {
        return state.facts.constrain(subject, in: program.ast, toHaveType: callable.output)
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
        range: program.ast[c].name.site)
      let initCandidates = resolve(
        initName, withArguments: [], memberOf: instanceType, from: scope)

      // We're done if we couldn't find any initializer.
      if initCandidates.isEmpty {
        _ = state.facts.assignErrorType(to: syntax.callee)
        return state.facts.assignErrorType(to: subject)
      }

      if let pick = initCandidates.uniqueElement {
        // Rebind the callee and constrain its type.
        let ctorType = LambdaType(pick.type.shape)!.ctor()!
        referredDecls[c] = pick.reference
        state.facts.assign(^ctorType, to: c)
        state.facts.append(pick.type.constraints)

        // Visit the arguments.
        if parametersMatching(
          arguments: syntax.arguments, of: syntax.callee, in: scope,
          expecting: ctorType.inputs, updating: &state)
        {
          return state.facts.constrain(subject, in: program.ast, toHaveType: ctorType.output)
        } else {
          return state.facts.assignErrorType(to: subject)
        }
      } else {
        fatalError("not implemented")
      }
    }

    // Case 3c
    addDiagnostic(
      .error(
        nonCallableType: state.facts.inferredTypes[syntax.callee]!,
        at: program.ast[syntax.callee].site))
    return state.facts.assignErrorType(to: subject)
  }

  private mutating func inferType(
    ofInoutExpr subject: NodeID<InoutExpr>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    let syntax = program.ast[subject]
    let subjectType = inferType(
      of: syntax.subject, in: scope,
      expecting: expectedType, updating: &state)
    return state.facts.constrain(subject, in: program.ast, toHaveType: subjectType)
  }

  private mutating func inferType(
    ofIntegerLiteralExpr subject: NodeID<IntegerLiteralExpr>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    let syntax = program.ast[subject]

    let defaultType = AnyType(program.ast.coreType(named: "Int")!)
    let cause = ConstraintCause(.literal, at: syntax.site)

    // If there's an expected type, constrain it to conform to `ExpressibleByIntegerLiteral`.
    // Otherwise, constraint the literal to have type `Int`.
    if let e = expectedType {
      let literalTrait = program.ast.coreTrait(named: "ExpressibleByIntegerLiteral")!
      state.facts.append(
        LiteralConstraint(e, defaultsTo: defaultType, conformsTo: literalTrait, because: cause))
      return state.facts.constrain(subject, in: program.ast, toHaveType: e)
    } else {
      return state.facts.constrain(subject, in: program.ast, toHaveType: defaultType)
    }
  }

  private mutating func inferType(
    ofLambdaExpr subject: NodeID<LambdaExpr>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    let syntax = program.ast[subject]

    let subjectConventions: [AccessEffect]?
    if let s = expectedType?.base as? LambdaType {
      // Check that the underlying declaration is structurally compatible with the type.
      let requiredLabels = program.ast[syntax.decl].parameters
        .map({ (p) in program.ast[p].label?.value })
      if requiredLabels.count != s.inputs.count {
        addDiagnostic(
          .error(
            expectedLambdaParameterCount: s.inputs.count, found: requiredLabels.count,
            at: program.ast[syntax.decl].introducerSite))
        return state.facts.assignErrorType(to: subject)
      }
      if !requiredLabels.elementsEqual(s.inputs, by: { $0 == $1.label }) {
        addDiagnostic(
          .error(
            labels: requiredLabels, incompatibleWith: s.inputs.map(\.label),
            at: program.ast[syntax.decl].introducerSite))
        return state.facts.assignErrorType(to: subject)
      }

      subjectConventions = s.inputs.map({ (p) in ParameterType(p.type)?.convention ?? .let })
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
      if case .expr(let body) = program.ast[syntax.decl].body {
        _ = inferType(
          of: body, in: AnyScopeID(syntax.decl),
          expecting: underlyingDeclType.output, updating: &state)
      } else {
        addDiagnostic(
          .error(cannotInferComplexReturnTypeAt: program.ast[syntax.decl].introducerSite))
        return state.facts.assignErrorType(to: subject)
      }
    }

    return state.facts.constrain(subject, in: program.ast, toHaveType: underlyingDeclType)
  }

  private mutating func inferType(
    ofMatchExpr subject: NodeID<MatchExpr>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    let syntax = program.ast[subject]

    // Visit the subject of the match.
    let subjectType = inferType(of: syntax.subject, in: scope, expecting: nil, updating: &state)
    if subjectType.isError {
      return state.facts.assignErrorType(to: subject)
    }

    for c in syntax.cases {
      // Each pattern is expected to have the same type as the subject.
      let caseType = inferType(
        of: program.ast[c].pattern, in: scope,
        expecting: subjectType, updating: &state)

      if caseType.isError {
        return state.facts.assignErrorType(to: subject)
      }
    }

    return state.facts.constrain(subject, in: program.ast, toHaveType: AnyType.void)
  }

  private mutating func inferType(
    ofNameExpr subject: NodeID<NameExpr>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    // Resolve the nominal prefix of the expression.
    let resolution = resolve(nominalPrefixOf: subject, from: scope)
    let nameType = inferType(
      ofNameExpr: subject, in: scope, withNameResolutionResult: resolution,
      updating: &state)

    if let e = expectedType {
      state.facts.append(
        EqualityConstraint(
          nameType, e, because: ConstraintCause(.binding, at: program.ast[subject].site)))
    }

    return nameType
  }

  private mutating func inferType(
    ofNameExpr subject: NodeID<NameExpr>,
    in scope: AnyScopeID,
    withNameResolutionResult resolution: TypeChecker.NameResolutionResult,
    updating state: inout State
  ) -> AnyType {
    var lastVisitedComponentType: AnyType?
    let unresolvedComponents: [NodeID<NameExpr>]

    switch resolution {
    case .failed:
      return state.facts.assignErrorType(to: subject)

    case .inexecutable(let suffix):
      if case .expr(let domainExpr) = program.ast[subject].domain {
        lastVisitedComponentType = inferType(
          of: domainExpr, in: scope, expecting: nil, updating: &state)
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
      let memberType = AnyType(TypeVariable(node: AnyNodeID(component)))
      state.facts.append(
        MemberConstraint(
          lastVisitedComponentType!, hasMemberReferredToBy: component, ofType: memberType,
          in: program.ast,
          because: ConstraintCause(.member, at: program.ast[component].site)))
      lastVisitedComponentType = state.facts.constrain(
        component, in: program.ast, toHaveType: memberType)
    }

    return lastVisitedComponentType!
  }

  private mutating func inferType(
    ofSequenceExpr subject: NodeID<SequenceExpr>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    // Fold the sequence and visit its sub-expressions.
    let foldedSequence = fold(sequenceExpr: subject, in: scope)
    foldedSequenceExprs[subject] = foldedSequence

    // Generate constraints from the folded sequence.
    let rootType = inferType(
      ofSequenceExpr: foldedSequence, in: scope,
      expecting: expectedType, updating: &state)
    return state.facts.constrain(subject, in: program.ast, toHaveType: rootType)
  }

  private mutating func inferType(
    ofSequenceExpr subject: FoldedSequenceExpr,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    switch subject {
    case .infix(let callee, let lhs, let rhs):
      // Infer the types of the operands.
      let lhsType = inferType(
        ofSequenceExpr: lhs, in: scope, expecting: nil, updating: &state)
      let rhsType = inferType(
        ofSequenceExpr: rhs, in: scope, expecting: nil, updating: &state)

      if lhsType.isError || rhsType.isError {
        return .error
      }

      // Infer the type of the callee.
      let parameterType = ^TypeVariable()
      state.facts.append(
        ParameterConstraint(
          rhsType, parameterType,
          because: ConstraintCause(.argument, at: program.ast.site(of: rhs))))

      let outputType = ^TypeVariable()
      let calleeType = LambdaType(
        receiverEffect: nil,
        environment: ^TupleType(labelsAndTypes: [("self", ^RemoteType(.let, lhsType))]),
        inputs: [CallableTypeParameter(type: parameterType)],
        output: outputType)
      state.facts.assign(^calleeType, to: callee.expr)

      // Create a member constraint for the operator.
      state.facts.append(
        MemberConstraint(
          lhsType, hasMemberReferredToBy: callee.expr, ofType: ^calleeType,
          in: program.ast,
          because: ConstraintCause(.member, at: program.ast[callee.expr].site)))

      return outputType

    case .leaf(let expr):
      return inferType(of: expr, in: scope, expecting: expectedType, updating: &state)
    }
  }

  private mutating func inferType(
    ofSubscriptCallExpr subject: NodeID<SubscriptCallExpr>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    let syntax = program.ast[subject]

    // Infer the type of the callee.
    let calleeType = inferType(of: syntax.callee, in: scope, expecting: nil, updating: &state)

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
      let returnType = expectedType ?? ^TypeVariable(node: AnyNodeID(subject))
      let assumedCalleeType = SubscriptImplType(
        isProperty: false,
        receiverEffect: nil,
        environment: ^TypeVariable(),
        inputs: parameters,
        output: returnType)

      state.facts.append(
        EqualityConstraint(
          calleeType, ^assumedCalleeType,
          because: ConstraintCause(.callee, at: program.ast[syntax.callee].site)))

      return state.facts.constrain(subject, in: program.ast, toHaveType: returnType)
    }

    // Case 3a
    if let callable = SubscriptType(state.facts.inferredTypes[syntax.callee]!) {
      if parametersMatching(
        arguments: syntax.arguments, of: syntax.callee, in: scope,
        expecting: callable.inputs, updating: &state)
      {
        return state.facts.constrain(subject, in: program.ast, toHaveType: callable.output)
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
        addDiagnostic(.error(invalidBufferTypeExprArgumentCount: subject, in: program.ast))
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
      addDiagnostic(
        .error(
          noUnnamedSubscriptsIn: state.facts.inferredTypes[syntax.callee]!,
          at: program.ast[syntax.callee].site))
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
        cause: ConstraintCause(.callee, at: program.ast[syntax.callee].site))

      // Visit the arguments.
      let calleeType = SubscriptType(instantiatedType.shape)!
      if parametersMatching(
        arguments: syntax.arguments, of: syntax.callee, in: scope,
        expecting: calleeType.inputs, updating: &state)
      {
        // Register the callee's constraints.
        state.facts.append(instantiatedType.constraints)

        // Update the referred declaration map if necessary.
        if let c = NodeID<NameExpr>(syntax.callee) {
          referredDecls[c] = .member(decl)
        }

        return state.facts.constrain(subject, in: program.ast, toHaveType: calleeType.output)
      } else {
        return state.facts.assignErrorType(to: subject)
      }

    default:
      // Note: Create an overload constraint.
      fatalError("not implemented")
    }
  }

  private mutating func inferType(
    ofTupleExpr subject: NodeID<TupleExpr>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    let elements = program.ast[subject].elements
    var elementTypes: [TupleType.Element] = []

    // If the expected type is a tuple compatible with the shape of the expression, propagate that
    // information down the expression tree. Otherwise, infer the type of the expression from the
    // leaves and use type constraints to detect potential mismatch.
    if let type = TupleType(expectedType),
      type.elements.elementsEqual(elements, by: { (a, b) in a.label == b.label?.value })
    {
      for i in 0 ..< elements.count {
        let elementType = inferType(
          of: elements[i].value, in: scope,
          expecting: type.elements[i].type, updating: &state)
        elementTypes.append(.init(label: elements[i].label?.value, type: elementType))
      }
    } else {
      for i in 0 ..< elements.count {
        let elementType = inferType(
          of: elements[i].value, in: scope,
          expecting: nil, updating: &state)
        elementTypes.append(.init(label: elements[i].label?.value, type: elementType))
      }
    }

    return state.facts.constrain(subject, in: program.ast, toHaveType: TupleType(elementTypes))
  }

  // MARK: Patterns

  /// Knowing `subject` occurs in `scope` and has a type compatible with `expectedType`, returns
  /// its inferred type with constraints on its sub-expressions and the nodes visited for which
  /// type checking has been deferred.
  mutating func inferType(
    of subject: AnyPatternID,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?
  ) -> (type: AnyType, facts: InferenceFacts, deferred: [DeferredQuery]) {
    var s: State = (facts: .init(), deferred: [])
    let t = inferType(
      of: subject, in: AnyScopeID(scope), expecting: expectedType, updating: &s)
    return (t, s.facts, s.deferred)
  }

  /// Returns the type of `subject` given it occurs in `scope`, using `expectedType` to propagate
  /// top-bottom type inference, and updating `state` with inference facts and nodes for which
  /// type checking has been deferred.
  private mutating func inferType(
    of subject: AnyPatternID,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    switch subject.kind {
    case BindingPattern.self:
      return inferType(
        ofBindingPattern: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case ExprPattern.self:
      return inferType(
        ofExprPattern: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case NamePattern.self:
      return inferType(
        ofNamePattern: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case TuplePattern.self:
      return inferType(
        ofTuplePattern: NodeID(subject)!, in: scope,
        expecting: expectedType, updating: &state)
    case WildcardPattern.self:
      return expectedType ?? ^TypeVariable()
    default:
      unreachable()
    }
  }

  private mutating func inferType(
    ofBindingPattern subject: NodeID<BindingPattern>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    // A binding pattern introduces additional type information when it has a type annotation. In
    // that case, the type denoted by the annotation is used to infer the type of the sub-pattern
    // and constrained to be a subtype of the expected type, if any.
    var subpatternType = expectedType
    if let a = program.ast[subject].annotation {
      if let subjectType = realize(a, in: scope)?.instance {
        if let t = expectedType {
          state.facts.append(
            SubtypingConstraint(
              subjectType, t,
              because: ConstraintCause(.annotation, at: program.ast[subject].site)))

        }
        subpatternType = subjectType
      } else {
        return .error
      }
    }

    return inferType(
      of: program.ast[subject].subpattern, in: scope,
      expecting: subpatternType, updating: &state)
  }

  private mutating func inferType(
    ofExprPattern subject: NodeID<ExprPattern>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    inferType(of: program.ast[subject].expr, in: scope, expecting: expectedType, updating: &state)
  }

  private mutating func inferType(
    ofNamePattern subject: NodeID<NamePattern>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    let nameDecl = program.ast[subject].decl
    let nameType = expectedType ?? ^TypeVariable(node: AnyNodeID(nameDecl))
    setInferredType(nameType, for: nameDecl)
    state.deferred.append({ (checker, solution) in
      checker.checkDeferred(varDecl: nameDecl, solution)
    })

    return nameType
  }

  private mutating func inferType(
    ofTuplePattern subject: NodeID<TuplePattern>,
    in scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating state: inout State
  ) -> AnyType {
    switch expectedType?.base {
    case let t as TupleType:
      // The pattern and the expected have a tuple shape.
      if t.elements.count != program.ast[subject].elements.count {
        // Invalid destructuring.
        diagnostics.insert(
          .error(invalidDestructuringOfType: expectedType!, at: program.ast[subject].site))
        return .error
      }

      var lLabels: [String?] = []
      var rLabels: [String?] = []

      // Visit the elements pairwise.
      for (a, b) in zip(program.ast[subject].elements, t.elements) {
        let elementType = inferType(of: a.pattern, in: scope, expecting: b.type, updating: &state)
        if elementType.isError { return .error }
        lLabels.append(a.label?.value)
        rLabels.append(b.label)
      }

      // Check that labels match.
      if lLabels != rLabels {
        diagnostics.insert(
          .error(labels: lLabels, incompatibleWith: rLabels, at: program.ast[subject].site))
        return .error
      }

      return expectedType!

    case is TypeVariable:
      // If the expected type is a variable, we can't infer anything more at this point.
      return expectedType!

    case .some:
      // If the expected type doesn't have a tuple shape, the pattern cannot match.
      diagnostics.insert(
        .error(invalidDestructuringOfType: expectedType!, at: program.ast[subject].site))
      return .error

    case nil:
      // Infer the shape of the expected type.
      var elements: [TupleType.Element] = []
      for a in program.ast[subject].elements {
        let elementType = inferType(
          of: a.pattern, in: scope,
          expecting: nil, updating: &state)
        if elementType.isError { return .error }
        elements.append(.init(label: a.label?.value, type: elementType))
      }
      return ^TupleType(elements)
    }
  }

  // MARK: Helpers

  /// If the labels of `arguments` matches those of `parameters`, visit the arguments' expressions
  /// to generate their type constraints assuming they have the corresponding type in `parameters`
  /// and returns `true`. Otherwise, returns `false`.
  private mutating func parametersMatching(
    arguments: [LabeledArgument],
    of callee: AnyExprID,
    in scope: AnyScopeID,
    expecting parameters: [CallableTypeParameter],
    updating state: inout State
  ) -> Bool {
    // Collect the argument and parameter labels.
    let argumentLabels = arguments.map(\.label?.value)
    let parameterLabels = parameters.map(\.label)

    // Check that the labels inferred from the callee are consistent with that of the call.
    if argumentLabels != parameterLabels {
      addDiagnostic(
        .error(
          labels: argumentLabels,
          incompatibleWith: parameterLabels,
          at: program.ast[callee].site))
      return false
    }

    // Create type constraints on arguments and parameters.
    for i in 0 ..< arguments.count {
      let argumentExpr = arguments[i].value

      // Infer the type of the argument, expecting it's the same as the parameter's bare type.
      let argumentType: AnyType
      if let t = ParameterType(parameters[i].type)?.bareType {
        argumentType = inferType(of: argumentExpr, in: scope, expecting: t, updating: &state)
        if areEquivalent(t, argumentType) { continue }
      } else {
        argumentType = inferType(of: argumentExpr, in: scope, expecting: nil, updating: &state)
      }

      state.facts.append(
        ParameterConstraint(
          argumentType, parameters[i].type,
          because: ConstraintCause(.argument, at: program.ast[argumentExpr].site)))
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
      let argumentType = inferType(
        of: argumentExpr, in: scope,
        expecting: ^TypeVariable(node: AnyNodeID(argumentExpr)), updating: &state)

      state.facts.append(
        ParameterConstraint(
          argumentType, parameterType,
          because: ConstraintCause(.argument, at: program.ast[argumentExpr].site)))

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
      return state.facts.constrain(name, in: program.ast, toHaveType: candidate.type.shape)
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
      let nameType = AnyType(TypeVariable(node: AnyNodeID(name)))
      state.facts.append(
        OverloadConstraint(
          name, withType: nameType, refersToOneOf: overloads,
          because: ConstraintCause(.binding, at: program.ast[name].site)))
      return state.facts.constrain(name, in: program.ast, toHaveType: nameType)
    }
  }

  /// Folds a sequence of binary expressions.
  private mutating func fold(
    sequenceExpr expr: NodeID<SequenceExpr>,
    in scope: AnyScopeID
  ) -> FoldedSequenceExpr {
    let syntax = program.ast[expr]
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
      let operatorStem = program.ast[tail[i].operator].name.value.stem
      let candidates = lookup(operator: operatorStem, notation: .infix, in: scope)

      switch candidates.count {
      case 0:
        addDiagnostic(
          .error(undefinedOperator: operatorStem, at: program.ast[tail[i].operator].site))
        accumulator.append(
          operator: (expr: tail[i].operator, precedence: nil),
          right: tail[i].operand)

      case 1:
        let precedence = program.ast[candidates[0]].precedenceGroup?.value
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
