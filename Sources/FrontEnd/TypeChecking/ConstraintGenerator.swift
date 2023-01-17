import Core
import Utils

extension TypeChecker {

  /// The types inferred by constraint generation for the visited expressions, along with the
  /// constraints between these types.
  struct InferenceFacts {

    /// A map from visited expression to its inferred type.
    private(set) var inferredTypes = ExprProperty<AnyType>()

    /// The set of type constraints between the types involved in the visited expressions.
    private(set) var constraints: [Constraint] = []

    /// True iff a constraint could not be solved.
    private(set) var foundConflict = false

    /// Creates empty base of facts assigning `type` to `subject` if `type` is not `nil`, or an
    /// empty base otherwise.
    init<ID: ExprID>(assigning type: AnyType?, to subject: ID) {
      if let t = type {
        assign(t, to: subject)
      }
    }

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
              because: ConstraintCause(.structural, at: ast[subject].origin)))
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

  /// Infers and returns the type of `subject`, writing the inferred types of its sub-expressions
  /// and the constraints thereupon in `facts`.
  mutating func infer(
    typeOf subject: AnyExprID,
    inScope scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    defer { assert(facts.inferredTypes[subject] != nil) }

    switch subject.kind {
    case BooleanLiteralExpr.self:
      return infer(
        typeOfBooleanLiteralExpr: NodeID(rawValue: subject.rawValue), inScope: scope,
        expecting: expectedType, updating: &facts)
    case CastExpr.self:
      return infer(
        typeOfCastExpr: NodeID(rawValue: subject.rawValue), inScope: scope,
        expecting: expectedType, updating: &facts)
    case CondExpr.self:
      return infer(
        typeOfConditionalExpr: NodeID(rawValue: subject.rawValue), inScope: scope,
        expecting: expectedType, updating: &facts)
    case FunctionCallExpr.self:
      return infer(
        typeOfFunctionCallExpr: NodeID(rawValue: subject.rawValue), inScope: scope,
        expecting: expectedType, updating: &facts)
    case InoutExpr.self:
      return infer(
        typeOfInoutExpr: NodeID(rawValue: subject.rawValue), inScope: scope,
        expecting: expectedType, updating: &facts)
    case IntegerLiteralExpr.self:
      return infer(
        typeOfIntegerLiteralExpr: NodeID(rawValue: subject.rawValue), inScope: scope,
        expecting: expectedType, updating: &facts)
    case LambdaExpr.self:
      return infer(
        typeOfLambdaExpr: NodeID(rawValue: subject.rawValue), inScope: scope,
        expecting: expectedType, updating: &facts)
    case NameExpr.self:
      return infer(
        typeOfNameExpr: NodeID(rawValue: subject.rawValue), inScope: scope,
        expecting: expectedType, updating: &facts)
    case SequenceExpr.self:
      return infer(
        typeOfSequenceExpr: NodeID(rawValue: subject.rawValue), inScope: scope,
        expecting: expectedType, updating: &facts)
    case SubscriptCallExpr.self:
      return infer(
        typeOfSubscriptCallExpr: NodeID(rawValue: subject.rawValue), inScope: scope,
        expecting: expectedType, updating: &facts)
    case TupleExpr.self:
      return infer(
        typeOfTupleExpr: NodeID(rawValue: subject.rawValue), inScope: scope,
        expecting: expectedType, updating: &facts)
    default:
      unreachable()
    }
  }

  private mutating func infer(
    typeOfBooleanLiteralExpr subject: NodeID<BooleanLiteralExpr>,
    inScope scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    facts.constrain(subject, in: program.ast, toHaveType: program.ast.coreType(named: "Bool")!)
  }

  private mutating func infer(
    typeOfCastExpr subject: NodeID<CastExpr>,
    inScope scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    let syntax = program.ast[subject]

    // Realize the type to which the left operand should be converted.
    guard let target = realize(syntax.right, inScope: scope)?.instance else {
      return facts.assignErrorType(to: subject)
    }

    let rhs = instantiate(
      target, inScope: scope, cause: ConstraintCause(.cast, at: syntax.origin))
    facts.append(rhs.constraints)

    let lhs = syntax.left
    switch syntax.kind {
    case .down:
      // Note: constraining the type of the left operand to be above the right operand wouldn't
      // contribute any useful information to the constraint system.
      _ = infer(typeOf: lhs, inScope: scope, expecting: nil, updating: &facts)

    case .up:
      // The type of the left operand must be statically known to subtype of the right operand.
      let lhsType = infer(
        typeOf: lhs, inScope: scope, expecting: ^TypeVariable(node: lhs.base), updating: &facts)
      facts.append(
        SubtypingConstraint(
          lhsType, rhs.shape,
          because: ConstraintCause(.cast, at: syntax.origin)))

    case .builtinPointerConversion:
      // The type of the left operand must be `Builtin.Pointer`.
      let lhsType = infer(
        typeOf: lhs, inScope: scope, expecting: nil, updating: &facts)
      facts.append(
        EqualityConstraint(
          lhsType, .builtin(.pointer),
          because: ConstraintCause(.cast, at: syntax.origin)))
    }

    // In any case, the expression is assumed to have the type denoted by the right operand.
    return facts.constrain(subject, in: program.ast, toHaveType: rhs.shape)
  }

  private mutating func infer(
    typeOfConditionalExpr subject: NodeID<CondExpr>,
    inScope scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    let syntax = program.ast[subject]

    // Visit the condition(s).
    let boolType = AnyType(program.ast.coreType(named: "Bool")!)
    for item in syntax.condition {
      switch item {
      case .expr(let expr):
        // Condition must be Boolean.
        facts.assign(boolType, to: expr)
        _ = infer(typeOf: expr, inScope: scope, expecting: boolType, updating: &facts)

      case .decl(let binding):
        if !check(binding: binding) { facts.setConflictFound() }
      }
    }

    // Assume the node represents an expression if both branches are single expressions.
    let successType: AnyType?

    // Visit the success branch.
    switch syntax.success {
    case .expr(let expr):
      successType = infer(
        typeOf: expr, inScope: scope, expecting: expectedType, updating: &facts)

    case .block(let branch):
      if !check(brace: branch) { facts.setConflictFound() }
      successType = nil
    }

    // Visit the failure branch.
    switch syntax.failure {
    case .expr(let expr):
      let failureType = infer(
        typeOf: expr, inScope: scope, expecting: expectedType, updating: &facts)

      if let successType = successType {
        // Both branches are single expressions.
        facts.append(
          EqualityConstraint(
            successType, failureType,
            because: ConstraintCause(.branchMerge, at: syntax.origin)))
        return facts.constrain(subject, in: program.ast, toHaveType: successType)
      }

    case .block(let branch):
      if !check(brace: branch) { facts.setConflictFound() }

    case nil:
      break
    }

    return facts.constrain(subject, in: program.ast, toHaveType: AnyType.void)
  }

  private mutating func infer(
    typeOfFunctionCallExpr subject: NodeID<FunctionCallExpr>,
    inScope scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    let syntax = program.ast[subject]

    // Infer the type of the callee.
    let calleeType = infer(typeOf: syntax.callee, inScope: scope, expecting: nil, updating: &facts)

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
      return facts.assignErrorType(to: subject)
    }

    // Case 2
    if calleeType.base is TypeVariable {
      let parameters = parametersMatching(
        arguments: syntax.arguments, inScope: scope, updating: &facts)
      let returnType = expectedType ?? ^TypeVariable(node: AnyNodeID(subject))

      facts.append(
        FunctionCallConstraint(
          calleeType, takes: parameters, andReturns: returnType,
          because: ConstraintCause(.callee, at: program.ast[syntax.callee].origin)))

      return facts.constrain(subject, in: program.ast, toHaveType: returnType)
    }

    // Case 3a
    if let callable = calleeType.base as? CallableType {
      if parametersMatching(
        arguments: syntax.arguments, of: syntax.callee, inScope: scope,
        expecting: callable.inputs, updating: &facts)
      {
        return facts.constrain(subject, in: program.ast, toHaveType: callable.output)
      } else {
        return facts.assignErrorType(to: subject)
      }
    }

    // Case 3b
    if
      let c = NodeID<NameExpr>(syntax.callee),
      let d = referredDecls[c]?.decl,
      isNominalTypeDecl(d)
    {
      let instanceType = MetatypeType(calleeType)!.instance
      let initName = SourceRepresentable(
        value: Name(stem: "init", labels: ["self"] + syntax.arguments.map({ $0.label?.value })),
        range: program.ast[c].name.origin)
      let initCandidates = resolve(
        initName, withArguments: [], memberOf: instanceType, from: scope)

      // We're done if we couldn't find any initializer.
      if initCandidates.isEmpty {
        addDiagnostic(.diagnose(undefinedName: initName.value, at: initName.origin))
        return facts.assignErrorType(to: syntax.callee)
      }

      if let pick = initCandidates.uniqueElement {
        // Rebind the callee and constrain its type.
        let ctorType = LambdaType(pick.type.shape)!.ctor()!
        referredDecls[c] = pick.reference
        facts.assign(^ctorType, to: c)
        facts.append(pick.type.constraints)

        // Visit the arguments.
        if parametersMatching(
          arguments: syntax.arguments, of: syntax.callee, inScope: scope,
          expecting: ctorType.inputs, updating: &facts)
        {
          return facts.constrain(subject, in: program.ast, toHaveType: ctorType.output)
        } else {
          return facts.assignErrorType(to: subject)
        }
      } else {
        fatalError("not implemented")
      }
    }

    // Case 3c
    addDiagnostic(
      .diagnose(
        nonCallableType: facts.inferredTypes[syntax.callee]!,
        at: program.ast[syntax.callee].origin))
    return facts.assignErrorType(to: subject)
  }

  private mutating func infer(
    typeOfInoutExpr subject: NodeID<InoutExpr>,
    inScope scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    let syntax = program.ast[subject]
    let subjectType = infer(
      typeOf: syntax.subject, inScope: scope,
      expecting: expectedType, updating: &facts)
    return facts.constrain(subject, in: program.ast, toHaveType: subjectType)
  }

  private mutating func infer(
    typeOfIntegerLiteralExpr subject: NodeID<IntegerLiteralExpr>,
    inScope scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    let syntax = program.ast[subject]

    let defaultType = AnyType(program.ast.coreType(named: "Int")!)
    let cause = ConstraintCause(.literal, at: syntax.origin)

    // If there's an expected type, constrain it to conform to `ExpressibleByIntegerLiteral`.
    // Otherwise, constraint the literal to have type `Int`.
    if let e = expectedType {
      let literalTrait = program.ast.coreTrait(named: "ExpressibleByIntegerLiteral")!
      facts.append(
        LiteralConstraint(e, defaultsTo: defaultType, conformsTo: literalTrait, because: cause))
      return facts.constrain(subject, in: program.ast, toHaveType: e)
    } else {
      return facts.constrain(subject, in: program.ast, toHaveType: defaultType)
    }
  }

  private mutating func infer(
    typeOfLambdaExpr subject: NodeID<LambdaExpr>,
    inScope scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    let syntax = program.ast[subject]

    // Realize the type of the underlying declaration.
    guard let declType = LambdaType(realize(underlyingDeclOf: subject)) else {
      return facts.assignErrorType(to: subject)
    }

    // Schedule the underlying declaration to be type-checked.
    deferTypeChecking(subject)

    if let expectedType = LambdaType(expectedType!) {
      // Check that the declaration defines the expected number of parameters.
      if declType.inputs.count != expectedType.inputs.count {
        addDiagnostic(
          .diagnose(
            expectedLambdaParameterCount: expectedType.inputs.count,
            found: declType.inputs.count,
            at: syntax.origin))
        return facts.assignErrorType(to: subject)
      }

      // Check that the declaration defines the expected argument labels.
      if !declType.inputs.elementsEqual(expectedType.inputs, by: { $0.label == $1.label }) {
        addDiagnostic(
          .diagnose(
            labels: declType.inputs.map(\.label),
            incompatibleWith: expectedType.inputs.map(\.label),
            at: syntax.origin))
        return facts.assignErrorType(to: subject)
      }
    } else if declType.output.base is TypeVariable {
      if case .expr(let body) = program.ast[syntax.decl].body {
        // Infer the return type of the lambda from its body.
        facts.assign(declType.output, to: body)
        _ = infer(
          typeOf: body, inScope: AnyScopeID(syntax.decl),
          expecting: declType.output, updating: &facts)
      } else {
        // The system is underspecified.
        addDiagnostic(
          .diagnose(cannotInferComplexReturnTypeAt: program.ast[syntax.decl].introducerRange))
        return facts.assignErrorType(to: subject)
      }
    }

    return facts.constrain(subject, in: program.ast, toHaveType: declType)
  }

  private mutating func infer(
    typeOfNameExpr subject: NodeID<NameExpr>,
    inScope scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    // Resolve the nominal prefix of the expression.
    let resolution = resolve(nominalPrefixOf: subject, from: scope)
    let nameType = infer(
      typeOfNameExpr: subject, inScope: scope, withNameResolutionResult: resolution,
      updating: &facts)

    if let e = expectedType {
      facts.append(
        EqualityConstraint(
          nameType, e, because: ConstraintCause(.binding, at: program.ast[subject].origin)))
    }

    return nameType
  }

  private mutating func infer(
    typeOfNameExpr subject: NodeID<NameExpr>,
    inScope scope: AnyScopeID,
    withNameResolutionResult resolution: TypeChecker.NameResolutionResult,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    var lastVisitedComponentType: AnyType?
    let unresolvedComponents: [NodeID<NameExpr>]

    switch resolution {
    case .failed:
      return facts.assignErrorType(to: subject)

    case .inexecutable(let suffix):
      if case .expr(let domainExpr) = program.ast[subject].domain {
        lastVisitedComponentType = infer(
          typeOf: domainExpr, inScope: scope, expecting: nil, updating: &facts)
      } else {
        fatalError("not implemented")
      }
      unresolvedComponents = suffix

    case .done(let prefix, let suffix):
      assert(!prefix.isEmpty, "at least one name component should have been resolved")
      for p in prefix {
        lastVisitedComponentType = bind(p.component, to: p.candidates, updating: &facts)
      }

      unresolvedComponents = suffix
    }

    // Create the necessary constraints to let the solver resolve the remaining components.
    for component in unresolvedComponents {
      let memberType = AnyType(TypeVariable(node: AnyNodeID(component)))
      facts.append(
        MemberConstraint(
          lastVisitedComponentType!, hasMemberReferredToBy: component, ofType: memberType,
          in: program.ast,
          because: ConstraintCause(.member, at: program.ast[component].origin)))
      lastVisitedComponentType = facts.constrain(
        component, in: program.ast, toHaveType: memberType)
    }

    return lastVisitedComponentType!
  }

  private mutating func infer(
    typeOfSequenceExpr subject: NodeID<SequenceExpr>,
    inScope scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    // Fold the sequence and visit its sub-expressions.
    let foldedSequence = fold(sequenceExpr: subject, inScope: scope)
    foldedSequenceExprs[subject] = foldedSequence

    // Generate constraints from the folded sequence.
    let rootType = infer(
      typeOfSequenceExpr: foldedSequence, inScope: scope,
      expecting: expectedType, updating: &facts)
    return facts.constrain(subject, in: program.ast, toHaveType: rootType)
  }

  private mutating func infer(
    typeOfSequenceExpr subject: FoldedSequenceExpr,
    inScope scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    switch subject {
    case .infix(let callee, let lhs, let rhs):
      // Infer the types of the operands.
      let lhsType = infer(
        typeOfSequenceExpr: lhs, inScope: scope, expecting: nil, updating: &facts)
      let rhsType = infer(
        typeOfSequenceExpr: rhs, inScope: scope, expecting: nil, updating: &facts)

      if lhsType.isError || rhsType.isError {
        return .error
      }

      // Infer the type of the callee.
      let parameterType = ^TypeVariable()
      facts.append(
        ParameterConstraint(
          rhsType, parameterType,
          because: ConstraintCause(.argument, at: program.ast.origin(of: rhs))))

      let outputType = ^TypeVariable()
      let calleeType = LambdaType(
        receiverEffect: nil,
        environment: ^TupleType(labelsAndTypes: [("self", ^RemoteType(.let, lhsType))]),
        inputs: [CallableTypeParameter(type: parameterType)],
        output: outputType)
      facts.assign(^calleeType, to: callee.expr)

      // Create a member constraint for the operator.
      facts.append(
        MemberConstraint(
          lhsType, hasMemberReferredToBy: callee.expr, ofType: ^calleeType,
          in: program.ast,
          because: ConstraintCause(.member, at: program.ast[callee.expr].origin)))

      return outputType

    case .leaf(let expr):
      return infer(typeOf: expr, inScope: scope, expecting: expectedType, updating: &facts)
    }
  }

  private mutating func infer(
    typeOfSubscriptCallExpr subject: NodeID<SubscriptCallExpr>,
    inScope scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    let syntax = program.ast[subject]

    // Infer the type of the callee.
    let calleeType = infer(typeOf: syntax.callee, inScope: scope, expecting: nil, updating: &facts)

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
    if facts.inferredTypes[syntax.callee]!.isError {
      return facts.assignErrorType(to: subject)
    }

    // Case 2
    if calleeType.base is TypeVariable {
      let parameters = parametersMatching(
        arguments: syntax.arguments, inScope: scope, updating: &facts)
      let returnType = expectedType ?? ^TypeVariable(node: AnyNodeID(subject))
      let assumedCalleeType = SubscriptImplType(
        isProperty: false,
        receiverEffect: nil,
        environment: ^TypeVariable(),
        inputs: parameters,
        output: returnType)

      facts.append(
        EqualityConstraint(
          calleeType, ^assumedCalleeType,
          because: ConstraintCause(.callee, at: program.ast[syntax.callee].origin)))

      return facts.constrain(subject, in: program.ast, toHaveType: returnType)
    }

    // Case 3a
    if let callable = SubscriptType(facts.inferredTypes[syntax.callee]!) {
      if parametersMatching(
        arguments: syntax.arguments, of: syntax.callee, inScope: scope,
        expecting: callable.inputs, updating: &facts)
      {
        return facts.constrain(subject, in: program.ast, toHaveType: callable.output)
      } else {
        return facts.assignErrorType(to: subject)
      }
    }

    // Case 3b
    if
      let c = NodeID<NameExpr>(syntax.callee),
      let d = referredDecls[c]?.decl,
      isNominalTypeDecl(d)
    {
      assert(calleeType.base is MetatypeType)

      // Buffer type expressions shall have exactly one argument.
      if syntax.arguments.count != 1 {
        addDiagnostic(.diagnose(invalidBufferTypeExprArgumentCount: subject, in: program.ast))
        return facts.assignErrorType(to: subject)
      }

      // Note: We'll need some form of compile-time evaluation here.
      fatalError("not implemented")
    }

    // Case 3c
    let candidates = lookup("[]", memberOf: facts.inferredTypes[syntax.callee]!, inScope: scope)
    switch candidates.count {
    case 0:
      addDiagnostic(
        .diagnose(
          noUnnamedSubscriptsIn: facts.inferredTypes[syntax.callee]!,
          at: program.ast[syntax.callee].origin))
      return facts.assignErrorType(to: subject)

    case 1:
      // If there's a single candidate, we're looking at case 3a.
      let decl = candidates.first!
      let declType = realize(decl: decl)
      assert(decl.kind == SubscriptDecl.self)

      // Bail out if we can't get the type of the referred declaration.
      if declType.isError {
        return facts.assignErrorType(to: subject)
      }

      // Contextualize the type of the referred declaration.
      let instantiatedType = instantiate(
        declType,
        inScope: scope,
        cause: ConstraintCause(
          .callee, at: program.ast[syntax.callee].origin))

      // Visit the arguments.
      let calleeType = SubscriptType(instantiatedType.shape)!
      if parametersMatching(
        arguments: syntax.arguments, of: syntax.callee, inScope: scope,
        expecting: calleeType.inputs, updating: &facts)
      {
        // Register the callee's constraints.
        facts.append(instantiatedType.constraints)

        // Update the referred declaration map if necessary.
        if let c = NodeID<NameExpr>(syntax.callee) {
          referredDecls[c] = .member(decl)
        }

        return facts.constrain(subject, in: program.ast, toHaveType: calleeType.output)
      } else {
        return facts.assignErrorType(to: subject)
      }

    default:
      // Note: Create an overload constraint.
      fatalError("not implemented")
    }
  }

  private mutating func infer(
    typeOfTupleExpr subject: NodeID<TupleExpr>,
    inScope scope: AnyScopeID,
    expecting expectedType: AnyType?,
    updating facts: inout InferenceFacts
  ) -> AnyType {
    let elements = program.ast[subject].elements
    var elementTypes: [TupleType.Element] = []

    // If the expected type is a tuple compatible with the shape of the expression, propagate that
    // information down the expression tree. Otherwise, infer the type of the expression from the
    // leaves and use type constraints to detect potential mismatch.
    if
      let type = TupleType(expectedType),
      type.elements.elementsEqual(elements, by: { (a, b) in a.label == b.label?.value })
    {
      for i in 0 ..< elements.count {
        let elementType = infer(
          typeOf: elements[i].value, inScope: scope,
          expecting: type.elements[i].type, updating: &facts)
        elementTypes.append(.init(label: elements[i].label?.value, type: elementType))
      }
    } else {
      for i in 0 ..< elements.count {
        let elementType = infer(
          typeOf: elements[i].value, inScope: scope,
          expecting: nil, updating: &facts)
        elementTypes.append(.init(label: elements[i].label?.value, type: elementType))
      }
    }

    return facts.constrain(subject, in: program.ast, toHaveType: TupleType(elementTypes))
  }

  /// If the labels of `arguments` matches those of `parameters`, visit the arguments' expressions
  /// to generate their type constraints assuming they have the corresponding type in `parameters`
  /// and returns `true`. Otherwise, returns `false`.
  private mutating func parametersMatching(
    arguments: [LabeledArgument],
    of callee: AnyExprID,
    inScope scope: AnyScopeID,
    expecting parameters: [CallableTypeParameter],
    updating facts: inout InferenceFacts
  ) -> Bool {
    // Collect the argument and parameter labels.
    let argumentLabels = arguments.map({ $0.label?.value })
    let parameterLabels = parameters.map({ $0.label })

    // Check that the labels inferred from the callee are consistent with that of the call.
    if argumentLabels != parameterLabels {
      addDiagnostic(
        .diagnose(
          labels: argumentLabels,
          incompatibleWith: parameterLabels,
          at: program.ast[callee].origin))
      return false
    }

    // Create type constraints on arguments and parameters.
    for i in 0 ..< arguments.count {
      let argumentExpr = arguments[i].value

      // Infer the type of the argument, expecting it's the same as the parameter's bare type.
      let parameterType = ParameterType(parameters[i].type) ?? fatalError("invalid callee type")
      let argumentType = infer(
        typeOf: argumentExpr, inScope: scope, expecting: parameterType.bareType, updating: &facts)

      // Nothing to constrain if the parameter's type is equal to the argument's type.
      if areEquivalent(parameterType.bareType, argumentType) { continue }

      facts.append(
        ParameterConstraint(
          argumentType, ^parameterType,
          because: ConstraintCause(.argument, at: program.ast[argumentExpr].origin)))
    }

    return true
  }

  /// Visit `arguments` to generate their type constraints and returns a matching parameter list.
  private mutating func parametersMatching(
    arguments: [LabeledArgument],
    inScope scope: AnyScopeID,
    updating facts: inout InferenceFacts
  ) -> [CallableTypeParameter] {
    var parameters: [CallableTypeParameter] = []
    parameters.reserveCapacity(arguments.count)

    for i in 0 ..< arguments.count {
      let argumentExpr = arguments[i].value
      let parameterType = ^TypeVariable()

      // Infer the type of the argument bottom-up.
      let argumentType = infer(
        typeOf: argumentExpr, inScope: scope,
        expecting: ^TypeVariable(node: AnyNodeID(argumentExpr)), updating: &facts)

      facts.append(
        ParameterConstraint(
          argumentType, parameterType,
          because: ConstraintCause(.argument, at: program.ast[argumentExpr].origin)))

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
    updating facts: inout InferenceFacts
  ) -> AnyType {
    precondition(!candidates.isEmpty)

    if let candidate = candidates.uniqueElement {
      // Bind the component to the resolved declaration and store its type.
      referredDecls[name] = candidate.reference
      facts.append(candidate.type.constraints)
      return facts.constrain(name, in: program.ast, toHaveType: candidate.type.shape)
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
      facts.append(
        OverloadConstraint(
          name, withType: nameType, refersToOneOf: overloads,
          because: ConstraintCause(.binding, at: program.ast[name].origin)))
      return facts.constrain(name, in: program.ast, toHaveType: nameType)
    }
  }

  /// Folds a sequence of binary expressions.
  private mutating func fold(
    sequenceExpr expr: NodeID<SequenceExpr>,
    inScope scope: AnyScopeID
  ) -> FoldedSequenceExpr {
    let syntax = program.ast[expr]
    return fold(sequenceExprTail: syntax.tail[0...], into: .leaf(syntax.head), inScope: scope)
  }

  /// Folds the remainder of a sequence of binary expressions into `initialResult`.
  private mutating func fold(
    sequenceExprTail tail: ArraySlice<SequenceExpr.TailElement>,
    into initialResult: FoldedSequenceExpr,
    inScope scope: AnyScopeID
  ) -> FoldedSequenceExpr {
    var accumulator = initialResult

    for i in tail.indices {
      // Search for the operator declaration.
      let operatorStem = program.ast[tail[i].operator].name.value.stem
      let candidates = lookup(
        operator: operatorStem,
        notation: .infix,
        inScope: scope)

      switch candidates.count {
      case 0:
        addDiagnostic(
          .diagnose(undefinedOperator: operatorStem, at: program.ast[tail[i].operator].origin))
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
