import Utils
import Core

/// A visitor that generates constraints based on the structure of the AST.
struct ConstraintGenerator {

  /// The types inferred by constraint generation for the visited expressions, along with the
  /// constraints between these types and the diagnostics of the error encountered.
  struct Result {

    /// A map from visited expression to its inferred type.
    let inferredTypes: ExprProperty<AnyType>

    /// The set of type constraints being generated.
    let constraints: [Constraint]

    /// Indicates that the solver encountered one or more errors.
    let didFoundError: Bool

  }

  /// The expression for which constraints are being generated.
  private let subject: AnyExprID

  /// The scope in which the AST is visited.
  private var scope: AnyScopeID

  /// A map from expression to its expected type.
  private var expectedTypes = ExprProperty<AnyType>()

  /// A map from visited expression to its inferred type.
  private var inferredTypes = ExprProperty<AnyType>()

  /// The set of type constraints being generated.
  private var constraints: [Constraint] = []

  /// True iff a constraint could not be solved.
  private var foundConflict = false

  /// Creates an instance that generates type constraints for `expr` in `scope` with the given
  /// inferred and expected types.
  init(
    scope: AnyScopeID,
    expr: AnyExprID,
    fixedType: AnyType?,
    expectedType: AnyType?
  ) {
    self.subject = expr
    self.scope = scope
    inferredTypes[expr] = fixedType
    expectedTypes[expr] = expectedType
  }

  /// Applies `self` to generate constraints using `checker` to resolve names and realize types.
  mutating func apply(using checker: inout TypeChecker) -> Result {
    _ = visit(expr: subject, using: &checker)
    return Result(
      inferredTypes: inferredTypes,
      constraints: constraints,
      didFoundError: foundConflict)
  }

  /// Returns the variable assigned to `expr` in the generated contraints, using `checker` to
  /// resolve names and realize types.
  private mutating func visit(expr: AnyExprID, using checker: inout TypeChecker) -> AnyType {
    defer { assert(inferredTypes[expr] != nil) }

    switch expr.kind {
    case BooleanLiteralExpr.self:
      return visit(booleanLiteral: NodeID(rawValue: expr.rawValue), using: &checker)
    case CastExpr.self:
      return visit(cast: NodeID(rawValue: expr.rawValue), using: &checker)
    case CondExpr.self:
      return visit(cond: NodeID(rawValue: expr.rawValue), using: &checker)
    case FunctionCallExpr.self:
      return visit(functionCall: NodeID(rawValue: expr.rawValue), using: &checker)
    case InoutExpr.self:
      return visit(`inout`: NodeID(rawValue: expr.rawValue), using: &checker)
    case IntegerLiteralExpr.self:
      return visit(integerLiteral: NodeID(rawValue: expr.rawValue), using: &checker)
    case LambdaExpr.self:
      return visit(lambda: NodeID(rawValue: expr.rawValue), using: &checker)
    case NameExpr.self:
      return visit(name: NodeID(rawValue: expr.rawValue), using: &checker)
    case SequenceExpr.self:
      return visit(sequence: NodeID(rawValue: expr.rawValue), using: &checker)
    case SubscriptCallExpr.self:
      return visit(subscriptCall: NodeID(rawValue: expr.rawValue), using: &checker)
    case TupleExpr.self:
      return visit(tuple: NodeID(rawValue: expr.rawValue), using: &checker)
    default:
      unreachable()
    }
  }

  private mutating func visit(
    booleanLiteral id: NodeID<BooleanLiteralExpr>,
    using checker: inout TypeChecker
  ) -> AnyType {
    constrain(
      id, toHaveType: checker.program.ast.coreType(named: "Bool")!,
      at: checker.program.ast[id].origin)
  }

  private mutating func visit(
    cast id: NodeID<CastExpr>,
    using checker: inout TypeChecker
  ) -> AnyType {
    let node = checker.program.ast[id]

    // Realize the type to which the left operand should be converted.
    guard var target = checker.realize(node.right, inScope: scope)?.instance else {
      return assignErrorType(to: id)
    }

    let (ty, cs) = checker.contextualize(
      type: target,
      inScope: scope,
      cause: ConstraintCause(.cast, at: checker.program.ast[id].origin))
    target = ty
    constraints.append(contentsOf: cs)

    let lhs = node.left
    switch node.kind {
    case .down:
      // Note: constraining the type of the left operand to be above the right operand wouldn't
      // contribute any useful information to the constraint system.
      break

    case .up:
      // The type of the left operand must be statically known to subtype of the right operand.
      inferredTypes[lhs] = ^TypeVariable(node: lhs.base)
      constraints.append(
        inferenceConstraint(
          inferredTypes[lhs]!, isSubtypeOf: target,
          because: ConstraintCause(.cast, at: checker.program.ast[id].origin)))

    case .builtinPointerConversion:
      // The type of the left operand must be `Builtin.Pointer`.
      inferredTypes[lhs] = .builtin(.pointer)
    }

    // Visit the left operand.
    _ = visit(expr: lhs, using: &checker)

    // In any case, the expression is assumed to have the type denoted by the right operand.
    return constrain(id, toHaveType: target, at: checker.program.ast[id].origin)
  }

  private mutating func visit(
    cond id: NodeID<CondExpr>,
    using checker: inout TypeChecker
  ) -> AnyType {
    // Visit the condition(s).
    let boolType = checker.program.ast.coreType(named: "Bool")!
    for item in checker.program.ast[id].condition {
      switch item {
      case .expr(let expr):
        // Condition must be Boolean.
        inferredTypes[expr] = ^boolType
        _ = visit(expr: expr, using: &checker)

      case .decl(let binding):
        if !checker.check(binding: binding) { foundConflict = true }
      }
    }

    // Assume the node represents an expression if both branches are single expressions.
    let inferredType: AnyType?

    // Visit the success branch.
    switch checker.program.ast[id].success {
    case .expr(let thenExpr):
      expectedTypes[thenExpr] = expectedTypes[id]
      inferredType = visit(expr: thenExpr, using: &checker)

    case .block(let thenBlock):
      if !checker.check(brace: thenBlock) { foundConflict = true }
      inferredType = nil
    }

    // Visit the failure branch.
    switch checker.program.ast[id].failure {
    case .expr(let elseExpr):
      expectedTypes[elseExpr] = inferredType
      _ = visit(expr: elseExpr, using: &checker)
      return constrain(id, toHaveType: inferredType!, at: checker.program.ast[id].origin)

    case .block(let thenBlock):
      if !checker.check(brace: thenBlock) { foundConflict = true }
      return constrain(id, toHaveType: AnyType.void, at: checker.program.ast[id].origin)

    case nil:
      return constrain(id, toHaveType: AnyType.void, at: checker.program.ast[id].origin)
    }
  }

  private mutating func visit(
    error id: NodeID<ErrorExpr>,
    using checker: inout TypeChecker
  ) -> AnyType {
    assignErrorType(to: id)
  }

  private mutating func visit(
    functionCall id: NodeID<FunctionCallExpr>,
    using checker: inout TypeChecker
  ) -> AnyType {
    // Infer the type of the callee.
    let callee = checker.program.ast[id].callee
    let calleeType = visit(expr: callee, using: &checker)

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
      return assignErrorType(to: id)
    }

    // Case 2
    if calleeType.base is TypeVariable {
      let parameters = visit(arguments: checker.program.ast[id].arguments, using: &checker)
      let returnType = expectedTypes[id] ?? ^TypeVariable(node: AnyNodeID(id))

      constraints.append(
        FunctionCallConstraint(
          calleeType, takes: parameters, andReturns: returnType,
          because: ConstraintCause(.callee, at: checker.program.ast[callee].origin)))

      return constrain(id, toHaveType: returnType, at: checker.program.ast[id].origin)
    }

    // Case 3a
    if let callable = calleeType.base as? CallableType {
      if visit(
        arguments: checker.program.ast[id].arguments,
        of: checker.program.ast[id].callee,
        expecting: callable.inputs,
        using: &checker)
      {
        return constrain(id, toHaveType: callable.output, at: checker.program.ast[id].origin)
      } else {
        return assignErrorType(to: id)
      }
    }

    // Case 3b
    if
      let c = NodeID<NameExpr>(callee),
      let d = checker.referredDecls[c]?.decl,
      checker.isNominalTypeDecl(d)
    {
      let instanceType = MetatypeType(calleeType)!.instance
      let initName = SourceRepresentable(
        value: Name(
          stem: "init",
          labels: ["self"] + checker.program.ast[id].arguments.map({ $0.label?.value })),
        range: checker.program.ast[c].name.origin)
      let initCandidates = checker.resolve(initName, memberOf: instanceType, from: scope)

      // We're done if we couldn't find any initializer.
      if initCandidates.isEmpty {
        checker.addDiagnostic(.diagnose(undefinedName: initName.value, at: initName.origin))
        return assignErrorType(to: callee)
      }

      if let pick = initCandidates.uniqueElement {
        // Rebind the callee.
        checker.referredDecls[c] = pick.reference

        // Constrain the callee's type.
        let (initType, initConstraints) = checker.contextualize(
          type: pick.type,
          inScope: checker.program.declToScope[d]!,
          cause: .init(.callee, at: nil))
        constraints.append(contentsOf: initConstraints)

        let ctorType = LambdaType(initType)!.ctor()!
        inferredTypes[callee] = ^ctorType

        // Visit the arguments.
        if visit(
          arguments: checker.program.ast[id].arguments,
          of: checker.program.ast[id].callee,
          expecting: ctorType.inputs,
          using: &checker)
        {
          return constrain(id, toHaveType: ctorType.output, at: checker.program.ast[id].origin)
        } else {
          return assignErrorType(to: id)
        }
      } else {
        fatalError("not implemented")
      }
    }

    // Case 3c
    checker.addDiagnostic(
      .diagnose(
        nonCallableType: inferredTypes[callee]!,
        at: checker.program.ast[checker.program.ast[id].callee].origin))
    return assignErrorType(to: id)
  }

  private mutating func visit(
    `inout` id: NodeID<InoutExpr>,
    using checker: inout TypeChecker
  ) -> AnyType {
    constrain(
      id, toHaveType: visit(expr: checker.program.ast[id].subject, using: &checker),
      at: checker.program.ast[id].origin)
  }

  private mutating func visit(
    integerLiteral id: NodeID<IntegerLiteralExpr>,
    using checker: inout TypeChecker
  ) -> AnyType {
    let trait = checker.program.ast.coreTrait(named: "ExpressibleByIntegerLiteral")!
    let cause = ConstraintCause(.literal, at: checker.program.ast[id].origin)

    // Constrain the type of the literal to conform to `ExpressibleByIntegerLiteral` or,
    // unless it's been already inferred from context, to be equal to `Int`.
    let expectedType = expectedTypes[id] ?? ^TypeVariable(node: AnyNodeID(id))
    if expectedType.base is TypeVariable {
      constraints.append(
        expressibleByLiteralConstraint(
          expectedType, trait: trait, defaultType: ^checker.program.ast.coreType(named: "Int")!,
          because: cause))
      return constrain(id, toHaveType: expectedType, at: checker.program.ast[id].origin)
    } else {
      constraints.append(ConformanceConstraint(expectedType, conformsTo: [trait], because: cause))
      return constrain(id, toHaveType: expectedType, at: checker.program.ast[id].origin)
    }
  }

  private mutating func visit(
    lambda id: NodeID<LambdaExpr>,
    using checker: inout TypeChecker
  ) -> AnyType {
    // Realize the type of the underlying declaration.
    guard let declType = LambdaType(checker.realize(underlyingDeclOf: id)) else {
      return assignErrorType(to: id)
    }

    // Schedule the underlying declaration to be type-checked.
    checker.deferTypeChecking(id)

    if let expectedType = LambdaType(expectedTypes[id]!) {
      // Check that the declaration defines the expected number of parameters.
      if declType.inputs.count != expectedType.inputs.count {
        checker.addDiagnostic(
          .diagnose(
            expectedLambdaParameterCount: expectedType.inputs.count,
            found: declType.inputs.count,
            at: checker.program.ast[id].origin))
        return assignErrorType(to: id)
      }

      // Check that the declaration defines the expected argument labels.
      if !declType.inputs.elementsEqual(expectedType.inputs, by: { $0.label == $1.label }) {
        checker.addDiagnostic(
          .diagnose(
            labels: declType.inputs.map(\.label),
            incompatibleWith: expectedType.inputs.map(\.label),
            at: checker.program.ast[id].origin))
        return assignErrorType(to: id)
      }
    } else if declType.output.base is TypeVariable {
      if case .expr(let body) = checker.program.ast[checker.program.ast[id].decl].body {
        // Infer the return type of the lambda from its body.
        inferredTypes[body] = declType.output
        expectedTypes[body] = declType.output

        let currentScope = scope
        scope = AnyScopeID(checker.program.ast[id].decl)
        _ = visit(expr: body, using: &checker)
        scope = currentScope
      } else {
        // The system is underspecified.
        let origin = checker.program.ast[checker.program.ast[id].decl].introducerRange
        checker.addDiagnostic(.diagnose(cannotInferComplexReturnTypeAt: origin))
        return assignErrorType(to: id)
      }
    }

    return constrain(id, toHaveType: declType, at: checker.program.ast[id].origin)
  }

  private mutating func visit(
    name id: NodeID<NameExpr>,
    using checker: inout TypeChecker
  ) -> AnyType {
    // Resolve the nominal prefix of the expression.
    let resolution = checker.resolve(nominalPrefixOf: id, from: scope)
    return visit(name: id, withNameResolutionResult: resolution, using: &checker)
  }

  private mutating func visit(
    name id: NodeID<NameExpr>,
    withNameResolutionResult resolution: TypeChecker.NameResolutionResult,
    using checker: inout TypeChecker
  ) -> AnyType {
    var parentType: AnyType?
    let unresolvedComponents: [NodeID<NameExpr>]

    switch resolution {
    case .failed:
      return assignErrorType(to: id)

    case .inexecutable(let suffix):
      if case .expr(let domainExpr) = checker.program.ast[id].domain {
        parentType = visit(expr: domainExpr, using: &checker)
      } else {
        fatalError("not implemented")
      }
      unresolvedComponents = suffix

    case .done(let prefix, let suffix):
      assert(!prefix.isEmpty)
      for p in prefix {
        parentType = constrain(p.component, to: p.candidates, using: &checker)
      }

      unresolvedComponents = suffix
    }

    // Create the necessary constraints to let the solver resolve the remaining components.
    for component in unresolvedComponents {
      let componentOrigin = checker.program.ast[component].origin
      let memberType = expectedTypes[component] ?? ^TypeVariable(node: AnyNodeID(component))
      constraints.append(
        MemberConstraint(
          parentType!, hasMemberReferredToBy: component, ofType: memberType,
          in: checker.program.ast,
          because: ConstraintCause(.member, at: componentOrigin)))
      parentType = constrain(component, toHaveType: memberType, at: componentOrigin)
    }

    return parentType!
  }

  private mutating func visit(
    sequence id: NodeID<SequenceExpr>,
    using checker: inout TypeChecker
  ) -> AnyType {
    // Fold the sequence and visit its sub-expressions.
    let foldedSequence = fold(sequenceExpr: id, using: &checker)
    checker.foldedSequenceExprs[id] = foldedSequence

    // Generate constraints from the folded sequence.
    let inferredRootType = visit(
      foldedSequence: foldedSequence,
      expectingRootType: expectedTypes[id],
      using: &checker)
    return constrain(id, toHaveType: inferredRootType, at: checker.program.ast[id].origin)
  }

  private mutating func visit(
    foldedSequence root: FoldedSequenceExpr,
    expectingRootType expectedRootType: AnyType?,
    using checker: inout TypeChecker
  ) -> AnyType {
    switch root {
    case .infix(let callee, let lhs, let rhs):
      // Infer the types of the operands.
      let lhsType = visit(foldedSequence: lhs, expectingRootType: nil, using: &checker)
      if lhsType.isError {
        return .error
      }

      let rhsType = visit(foldedSequence: rhs, expectingRootType: nil, using: &checker)
      if rhsType.isError {
        return .error
      }

      // Infer the type of the callee.
      let parameterType = ^TypeVariable()
      constraints.append(
        ParameterConstraint(
          rhsType, parameterType,
          because: ConstraintCause(.argument, at: checker.program.ast.origin(of: rhs))))

      let outputType = ^TypeVariable()
      let calleeType = LambdaType(
        receiverEffect: nil,
        environment: ^TupleType(labelsAndTypes: [("self", ^RemoteType(.let, lhsType))]),
        inputs: [CallableTypeParameter(type: parameterType)],
        output: outputType)
      inferredTypes[callee.expr] = ^calleeType

      // Create a member constraint for the operator.
      constraints.append(
        MemberConstraint(
          lhsType, hasMemberReferredToBy: callee.expr, ofType: ^calleeType,
          in: checker.program.ast,
          because: ConstraintCause(.member, at: checker.program.ast[callee.expr].origin)))

      return outputType

    case .leaf(let expr):
      expectedTypes[expr] = expectedRootType
      return visit(expr: expr, using: &checker)
    }
  }

  private mutating func visit(
    subscriptCall id: NodeID<SubscriptCallExpr>,
    using checker: inout TypeChecker
  ) -> AnyType {
    // Infer the type of the callee.
    let callee = checker.program.ast[id].callee
    let calleeType = visit(expr: callee, using: &checker)

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
    if inferredTypes[callee]!.isError {
      return assignErrorType(to: id)
    }

    // Case 2
    if calleeType.base is TypeVariable {
      let parameters = visit(arguments: checker.program.ast[id].arguments, using: &checker)
      let returnType = expectedTypes[id] ?? ^TypeVariable(node: AnyNodeID(id))
      let assumedCalleeType = SubscriptImplType(
        isProperty: false,
        receiverEffect: nil,
        environment: ^TypeVariable(),
        inputs: parameters,
        output: returnType)

      constraints.append(
        EqualityConstraint(
          calleeType, ^assumedCalleeType,
          because: ConstraintCause(.callee, at: checker.program.ast[callee].origin)))

      return constrain(id, toHaveType: returnType, at: checker.program.ast[id].origin)
    }

    // Case 3a
    if let callable = SubscriptType(inferredTypes[callee]!) {
      if visit(
        arguments: checker.program.ast[id].arguments,
        of: checker.program.ast[id].callee,
        expecting: callable.inputs,
        using: &checker)
      {
        return constrain(id, toHaveType: callable.output, at: checker.program.ast[id].origin)
      } else {
        return assignErrorType(to: id)
      }
    }

    // Case 3b
    if
      let c = NodeID<NameExpr>(callee),
      let d = checker.referredDecls[c]?.decl,
      checker.isNominalTypeDecl(d)
    {
      assert(calleeType.base is MetatypeType)

      // Buffer type expressions shall have exactly one argument.
      if checker.program.ast[id].arguments.count != 1 {
        checker.addDiagnostic(
          .diagnose(invalidBufferTypeExprArgumentCount: id, in: checker.program.ast))
        return assignErrorType(to: id)
      }

      // Note: We'll need some form of compile-time evaluation here.
      fatalError("not implemented")
    }

    // Case 3c
    let candidates = checker.lookup("[]", memberOf: inferredTypes[callee]!, inScope: scope)
    switch candidates.count {
    case 0:
      checker.addDiagnostic(
        .diagnose(
          noUnnamedSubscriptsIn: inferredTypes[callee]!,
          at: checker.program.ast[checker.program.ast[id].callee].origin))
      return assignErrorType(to: id)

    case 1:
      // If there's a single candidate, we're looking at case 3a.
      let decl = candidates.first!
      let declType = checker.realize(decl: decl)
      assert(decl.kind == SubscriptDecl.self)

      // Bail out if we can't get the type of the referred declaration.
      if declType.isError {
        return assignErrorType(to: id)
      }

      // Contextualize the type of the referred declaration.
      let (contextualizedDeclType, declConstraints) = checker.contextualize(
        type: declType,
        inScope: scope,
        cause: ConstraintCause(
          .callee, at: checker.program.ast[checker.program.ast[id].callee].origin))

      // Visit the arguments.
      let calleeType = SubscriptType(contextualizedDeclType)!
      if visit(
        arguments: checker.program.ast[id].arguments,
        of: checker.program.ast[id].callee,
        expecting: calleeType.inputs,
        using: &checker)
      {
        // Register the callee's constraints.
        constraints.append(contentsOf: declConstraints)

        // Update the referred declaration map if necessary.
        if let c = NodeID<NameExpr>(callee) {
          checker.referredDecls[c] = .member(decl)
        }

        return constrain(id, toHaveType: calleeType.output, at: checker.program.ast[id].origin)
      } else {
        return assignErrorType(to: id)
      }

    default:
      // Note: Create an overload constraint.
      fatalError("not implemented")
    }
  }

  private mutating func visit(
    tuple id: NodeID<TupleExpr>,
    using checker: inout TypeChecker
  ) -> AnyType {
    let tupleExpr = checker.program.ast[id].elements
    var tupleTypeElements: [TupleType.Element] = []

    // If the expected type is a tuple compatible with the shape of the expression, propagate that
    // information down the expression tree. Otherwise, infer the type of the expression from the
    // leaves and use type constraints to detect potential mismatch.
    if let type = TupleType(expectedTypes[id]),
      type.elements.elementsEqual(tupleExpr, by: { (a, b) in a.label == b.label?.value })
    {
      for i in 0 ..< tupleExpr.count {
        expectedTypes[tupleExpr[i].value] = type.elements[i].type
        let elementType = visit(expr: tupleExpr[i].value, using: &checker)
        tupleTypeElements.append(.init(label: tupleExpr[i].label?.value, type: elementType))
      }
    } else {
      for i in 0 ..< tupleExpr.count {
        let elementType = visit(expr: tupleExpr[i].value, using: &checker)
        tupleTypeElements.append(.init(label: tupleExpr[i].label?.value, type: elementType))
      }
    }

    return constrain(
      id, toHaveType: TupleType(tupleTypeElements), at: checker.program.ast[id].origin)
  }

  /// If the labels of `arguments` matches those of `parameters`, visit the arguments' expressions
  /// to generate their type constraints assuming they have the corresponding type in `parameters`
  /// and returns `true`. Otherwise, returns `false`.
  private mutating func visit(
    arguments: [LabeledArgument],
    of callee: AnyExprID,
    expecting parameters: [CallableTypeParameter],
    using checker: inout TypeChecker
  ) -> Bool {
    // Collect the argument and parameter labels.
    let argumentLabels = arguments.map({ $0.label?.value })
    let parameterLabels = parameters.map({ $0.label })

    // Check that the labels inferred from the callee are consistent with that of the call.
    if argumentLabels != parameterLabels {
      checker.addDiagnostic(
        .diagnose(
          labels: argumentLabels,
          incompatibleWith: parameterLabels,
          at: checker.program.ast[callee].origin))
      return false
    }

    // Propagate type information down.
    for i in 0 ..< arguments.count {
      let argumentExpr = arguments[i].value
      let parameterType = parameters[i].type

      // Infer the type of the argument, expecting it's the same as the parameter's bare type.
      if let type = ParameterType(parameterType) {
        expectedTypes[argumentExpr] = type.bareType
      }
      let argumentType = visit(expr: argumentExpr, using: &checker)

      constraints.append(
        ParameterConstraint(
          argumentType, parameterType,
          because: ConstraintCause(.argument, at: checker.program.ast[argumentExpr].origin)))
    }

    return true
  }

  /// Visit `arguments` to generate their type constraints and returns a matching parameter list.
  private mutating func visit(
    arguments: [LabeledArgument],
    using checker: inout TypeChecker
  ) -> [CallableTypeParameter] {
    var parameters: [CallableTypeParameter] = []
    parameters.reserveCapacity(arguments.count)

    for i in 0 ..< arguments.count {
      let argumentExpr = arguments[i].value
      let parameterType = ^TypeVariable()

      // Infer the type of the argument bottom-up.
      let argumentType = visit(expr: argumentExpr, using: &checker)

      constraints.append(
        ParameterConstraint(
          argumentType, parameterType,
          because: ConstraintCause(.argument, at: checker.program.ast[argumentExpr].origin)))

      let argumentLabel = arguments[i].label?.value
      parameters.append(CallableTypeParameter(label: argumentLabel, type: parameterType))
    }

    return parameters
  }

  /// Folds a sequence of binary expressions.
  private mutating func fold(
    sequenceExpr expr: NodeID<SequenceExpr>,
    using checker: inout TypeChecker
  ) -> FoldedSequenceExpr {
    let node = checker.program.ast[expr]
    return fold(sequenceExprTail: node.tail[0...], into: .leaf(node.head), using: &checker)
  }

  /// Folds the remainder of a sequence of binary expressions into `initialResult`.
  private mutating func fold(
    sequenceExprTail tail: ArraySlice<SequenceExpr.TailElement>,
    into initialResult: FoldedSequenceExpr,
    using checker: inout TypeChecker
  ) -> FoldedSequenceExpr {
    var accumulator = initialResult

    for i in tail.indices {
      // Search for the operator declaration.
      let operatorStem = checker.program.ast[tail[i].operator].name.value.stem
      let candidates = checker.lookup(
        operator: operatorStem,
        notation: .infix,
        inScope: scope)

      switch candidates.count {
      case 0:
        checker.diagnostics.insert(
          .diagnose(
            undefinedOperator: operatorStem,
            at: checker.program.ast[tail[i].operator].origin))
        accumulator.append(
          operator: (expr: tail[i].operator, precedence: nil),
          right: tail[i].operand)

      case 1:
        let precedence = checker.program.ast[candidates[0]].precedenceGroup?.value
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

  /// Assigns the error type to `subject` in the AST and returns the error type.
  private mutating func assignErrorType<ID: ExprID>(to subject: ID) -> AnyType {
    foundConflict = true
    let ty = AnyType.error
    inferredTypes[subject] = ty
    return ty
  }

  /// Constrains `subject` to have type `inferredType` and returns either `inferredType` or the
  /// type currently assigned to `subject` in the AST.
  private mutating func constrain<ID: ExprID, T: TypeProtocol>(
    _ subject: ID,
    toHaveType inferredType: T,
    at range: SourceRange?
  ) -> AnyType {
    if let ty = inferredTypes[subject] {
      if ty != inferredType {
        constraints.append(
          EqualityConstraint(^inferredType, ty, because: ConstraintCause(.structural, at: range)))
      }
      return ty
    } else {
      let ty = ^inferredType
      inferredTypes[subject] = ty
      return ty
    }
  }

  /// Constrains `name` to be a reference to either of the declarations in `candidates`.
  ///
  /// - Requires: `candidates` is not empty
  private mutating func constrain(
    _ name: NodeID<NameExpr>,
    to candidates: [(reference: DeclRef, type: AnyType)],
    using checker: inout TypeChecker
  ) -> AnyType {
    precondition(!candidates.isEmpty)
    let constrainOrigin = checker.program.ast[name].origin

    if let pick = candidates.uniqueElement {
      // Contextualize the candidate's type.
      let declScope = checker.program.declToScope[pick.reference.decl, default: scope]
      let (nameType, nameConstraints) = checker.contextualize(
        type: pick.type,
        inScope: declScope,
        cause: ConstraintCause(.binding, at: constrainOrigin))

      // Bind the component to the resolved declaration and store its type.
      checker.referredDecls[name] = pick.reference
      constraints.append(contentsOf: nameConstraints)
      return constrain(name, toHaveType: nameType, at: constrainOrigin)
    } else {
      // Create an overload set.
      let overloads: [OverloadConstraint.Candidate] = candidates.map({ (candidate) in
        // Contextualize the candidate's type.
        let (pickType, pickConstraints) = checker.contextualize(
          type: candidate.type,
          inScope: checker.program.declToScope[candidate.reference.decl]!,
          cause: ConstraintCause(.binding, at: constrainOrigin))

        return .init(
          reference: candidate.reference,
          type: pickType,
          constraints: pickConstraints,
          penalties: 0)
      })

      // Constrain the name to refer to one of the overloads.
      let nameType = expectedTypes[name] ?? ^TypeVariable(node: AnyNodeID(name))
      constraints.append(
        OverloadConstraint(name, withType: nameType,
          refersToOneOf: overloads,
          because: ConstraintCause(.binding, at: constrainOrigin)))
      return constrain(name, toHaveType: nameType, at: constrainOrigin)
    }
  }

}
