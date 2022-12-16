import Utils

/// A visitor that generates constraints based on the structure of the AST.
struct ConstraintGenerator {

  /// The types inferred by constraint generation for the visited expressions, along with the
  /// constraints between these types and the diagnostics of the error encountered.
  struct Result {

    /// A map from visited expression to its inferred type.
    let inferredTypes: ExprProperty<AnyType>

    /// The set of type constraints being generated.
    let constraints: [Constraint]

    /// The diagnostics of the errors the generator encountered.
    let diagnostics: [Diagnostic]

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

  /// The diagnostics of the errors the generator encountered.
  private var diagnostics: [Diagnostic] = []

  /// Creates an instance that generates type constraints for `expr` in `scope` with the given
  /// inferred and expected types.
  init(
    scope: AnyScopeID,
    expr: AnyExprID,
    inferredType: AnyType?,
    expectedType: AnyType?
  ) {
    self.subject = expr
    self.scope = scope
    inferredTypes[expr] = inferredType
    expectedTypes[expr] = expectedType
  }

  /// Applies `self` to generate constraints using `checker` to resolve names and realize types.
  mutating func apply(using checker: inout TypeChecker) -> Result {
    visit(expr: subject, using: &checker)
    return Result(
      inferredTypes: inferredTypes,
      constraints: constraints,
      diagnostics: diagnostics)
  }

  private mutating func visit(expr: AnyExprID, using checker: inout TypeChecker) {
    switch expr.kind {
    case BooleanLiteralExpr.self:
      return visit(booleanLiteral: NodeID(rawValue: expr.rawValue), using: &checker)
    case CastExpr.self:
      return visit(cast: NodeID(rawValue: expr.rawValue), using: &checker)
    case CondExpr.self:
      return visit(cond: NodeID(rawValue: expr.rawValue), using: &checker)
    case FloatLiteralExpr.self:
      return visit(floatLiteral: NodeID(rawValue: expr.rawValue), using: &checker)
    case FunctionCallExpr.self:
      return visit(functionCall: NodeID(rawValue: expr.rawValue), using: &checker)
    case InoutExpr.self:
      return visit(`inout`: NodeID(rawValue: expr.rawValue), using: &checker)
    case IntegerLiteralExpr.self:
      return visit(integerLiteral: NodeID(rawValue: expr.rawValue), using: &checker)
    case LambdaExpr.self:
      return visit(lambda: NodeID(rawValue: expr.rawValue), using: &checker)
    case MapLiteralExpr.self:
      return visit(mapLiteral: NodeID(rawValue: expr.rawValue), using: &checker)
    case MatchExpr.self:
      return visit(match: NodeID(rawValue: expr.rawValue), using: &checker)
    case NameExpr.self:
      return visit(name: NodeID(rawValue: expr.rawValue), using: &checker)
    case NilLiteralExpr.self:
      return visit(nil: NodeID(rawValue: expr.rawValue), using: &checker)
    case SequenceExpr.self:
      return visit(sequence: NodeID(rawValue: expr.rawValue), using: &checker)
    case SpawnExpr.self:
      return visit(spawn: NodeID(rawValue: expr.rawValue), using: &checker)
    case StringLiteralExpr.self:
      return visit(stringLiteral: NodeID(rawValue: expr.rawValue), using: &checker)
    case SubscriptCallExpr.self:
      return visit(subscriptCall: NodeID(rawValue: expr.rawValue), using: &checker)
    case TupleExpr.self:
      return visit(tuple: NodeID(rawValue: expr.rawValue), using: &checker)
    case TupleMemberExpr.self:
      return visit(tupleMember: NodeID(rawValue: expr.rawValue), using: &checker)
    case UnicodeScalarLiteralExpr.self:
      return visit(unicodeScalarLiteral: NodeID(rawValue: expr.rawValue), using: &checker)
    default:
      unreachable()
    }
  }

  private mutating func visit(
    booleanLiteral id: NodeID<BooleanLiteralExpr>,
    using checker: inout TypeChecker
  ) {
    let boolType = checker.program.ast.coreType(named: "Bool")!
    assume(typeOf: id, equals: boolType, at: checker.program.ast[id].origin)
  }

  private mutating func visit(
    bufferLiteral id: NodeID<BufferLiteralExpr>,
    using checker: inout TypeChecker
  ) {
    fatalError("not implemented")
  }

  private mutating func visit(
    cast id: NodeID<CastExpr>,
    using checker: inout TypeChecker
  ) {
    let node = checker.program.ast[id]

    // Realize the type to which the left operand should be converted.
    guard var target = checker.realize(node.right, inScope: scope)?.instance else {
      assignToError(id)
      return
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
        equalityOrSubtypingConstraint(
          inferredTypes[lhs]!, target,
          because: ConstraintCause(.cast, at: checker.program.ast[id].origin)))

    case .builtinPointerConversion:
      // The type of the left operand must be `Builtin.Pointer`.
      inferredTypes[lhs] = .builtin(.pointer)
    }

    // Visit the left operand.
    visit(expr: lhs, using: &checker)

    // In any case, the expression is assumed to have the type denoted by the right operand.
    assume(typeOf: id, equals: target, at: checker.program.ast[id].origin)
  }

  private mutating func visit(
    cond id: NodeID<CondExpr>,
    using checker: inout TypeChecker
  ) {
    defer { assert(inferredTypes[id] != nil) }

    // Visit the condition(s).
    let boolType = checker.program.ast.coreType(named: "Bool")!
    for item in checker.program.ast[id].condition {
      switch item {
      case .expr(let expr):
        // Condition must be Boolean.
        inferredTypes[expr] = ^boolType
        visit(expr: expr, using: &checker)

      case .decl(let binding):
        _ = checker.check(binding: binding)
      }
    }

    // Assume the node represents an expression if both branches are single expressions.
    let inferredType: AnyType?

    // Visit the success branch.
    switch checker.program.ast[id].success {
    case .expr(let thenExpr):
      expectedTypes[thenExpr] = expectedTypes[id]
      visit(expr: thenExpr, using: &checker)
      inferredType = inferredTypes[thenExpr]

    case .block(let thenBlock):
      _ = checker.check(brace: thenBlock)
      inferredType = nil
    }

    // Visit the failure branch.
    switch checker.program.ast[id].failure {
    case .expr(let elseExpr):
      assume(typeOf: id, equals: inferredType!, at: checker.program.ast[id].origin)
      expectedTypes[elseExpr] = inferredType
      visit(expr: elseExpr, using: &checker)

    case .block(let thenBlock):
      assume(typeOf: id, equals: AnyType.void, at: checker.program.ast[id].origin)
      _ = checker.check(brace: thenBlock)

    case nil:
      assume(typeOf: id, equals: AnyType.void, at: checker.program.ast[id].origin)
    }
  }

  private mutating func visit(
    error id: NodeID<ErrorExpr>,
    using checker: inout TypeChecker
  ) {
    // Nothing to do here.
  }

  private mutating func visit(
    floatLiteral id: NodeID<FloatLiteralExpr>,
    using checker: inout TypeChecker
  ) {
    fatalError("not implemented")
  }

  private mutating func visit(
    functionCall id: NodeID<FunctionCallExpr>,
    using checker: inout TypeChecker
  ) {
    defer { assert(inferredTypes[id] != nil) }

    // Infer the type of the callee.
    let callee = checker.program.ast[id].callee
    visit(expr: callee, using: &checker)

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
    if inferredTypes[callee]!.isError {
      assignToError(id)
      return
    }

    // Case 2
    if let calleeType = TypeVariable(inferredTypes[callee]!) {
      let parameters = visit(arguments: checker.program.ast[id].arguments, using: &checker)
      let returnType = expectedTypes[id] ?? ^TypeVariable(node: AnyNodeID(id))
      let assumedCalleeType = LambdaType(
        environment: ^TypeVariable(), inputs: parameters, output: returnType)

      constraints.append(
        EqualityConstraint(
          ^calleeType, ^assumedCalleeType,
          because: ConstraintCause(.callee, at: checker.program.ast[callee].origin)))

      assume(typeOf: id, equals: returnType, at: checker.program.ast[id].origin)
      return
    }

    // Case 3a
    if let calleeType = inferredTypes[callee]!.base as? CallableType {
      if visit(
        arguments: checker.program.ast[id].arguments,
        of: checker.program.ast[id].callee,
        expecting: calleeType.inputs,
        using: &checker)
      {
        assume(typeOf: id, equals: calleeType.output, at: checker.program.ast[id].origin)
      } else {
        assignToError(id)
      }
      return
    }

    // Case 3b
    if
      let c = NodeID<NameExpr>(callee),
      let d = checker.referredDecls[c]?.decl,
      checker.isNominalTypeDecl(d)
    {
      let instanceType = MetatypeType(inferredTypes[c]!)!.instance
      let initName = SourceRepresentable(
        value: Name(
          stem: "init",
          labels: ["self"] + checker.program.ast[id].arguments.map({ $0.label?.value })),
        range: checker.program.ast[c].name.origin)
      let initCandidates = checker.resolve2(initName, memberOf: instanceType, from: scope)

      // We're done if we couldn't find any initializer.
      if initCandidates.isEmpty {
        diagnostics.append(.diagnose(undefinedName: initName.value, at: initName.origin))
        assignToError(callee)
        return
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

        let calleeType = LambdaType(initType)!.ctor()!
        inferredTypes[c] = ^calleeType

        // Visit the arguments.
        if visit(
          arguments: checker.program.ast[id].arguments,
          of: checker.program.ast[id].callee,
          expecting: calleeType.inputs,
          using: &checker)
        {
          assume(typeOf: id, equals: calleeType.output, at: checker.program.ast[id].origin)
        } else {
          assignToError(id)
        }
      } else {
        fatalError("not implemented")
      }

      return
    }

    // Case 3c
    diagnostics.append(
      .diagnose(
        nonCallableType: inferredTypes[callee]!,
        at: checker.program.ast[checker.program.ast[id].callee].origin))
    assignToError(id)
  }

  private mutating func visit(
    `inout` id: NodeID<InoutExpr>,
    using checker: inout TypeChecker
  ) {
    let subexpr = checker.program.ast[id].subject
    expectedTypes[subexpr] = expectedTypes[id]
    visit(expr: subexpr, using: &checker)
    assume(typeOf: id, equals: inferredTypes[subexpr]!, at: checker.program.ast[id].origin)
  }

  private mutating func visit(
    integerLiteral id: NodeID<IntegerLiteralExpr>,
    using checker: inout TypeChecker
  ) {
    defer { assert(inferredTypes[id] != nil) }

    let trait = checker.program.ast.coreTrait(named: "ExpressibleByIntegerLiteral")!
    let cause = ConstraintCause(.literal, at: checker.program.ast[id].origin)

    switch expectedTypes[id]?.base {
    case let tau as TypeVariable:
      // The type of the expression is a variable, possibly constrained elsewhere; constrain it to
      // either be `Int` or conform to `ExpressibleByIntegerLiteral`.
      constraints.append(
        expressibleByLiteralConstraint(
          ^tau, trait: trait, defaultType: ^checker.program.ast.coreType(named: "Int")!,
           because: cause))
      assume(typeOf: id, equals: tau, at: checker.program.ast[id].origin)

    case .some(let expectedType):
      // The type of has been fixed; constrain it to conform to `ExpressibleByIntegerLiteral`.
      constraints.append(ConformanceConstraint(^expectedType, conformsTo: [trait], because: cause))
      assume(typeOf: id, equals: expectedType, at: checker.program.ast[id].origin)

    case nil:
      // Without contextual information, infer the type of the literal as `Val.Int`.
      let intType = checker.program.ast.coreType(named: "Int")!
      assume(typeOf: id, equals: intType, at: checker.program.ast[id].origin)
    }
  }

  private mutating func visit(
    lambda id: NodeID<LambdaExpr>,
    using checker: inout TypeChecker
  ) {
    defer { assert(inferredTypes[id] != nil) }

    // Realize the type of the underlying declaration.
    guard let declType = LambdaType(checker.realize(underlyingDeclOf: id)) else {
      assignToError(id)
      return
    }

    // Schedule the underlying declaration to be type-checked.
    checker.deferTypeChecking(id)

    if let expectedType = LambdaType(expectedTypes[id]!) {
      // Check that the declaration defines the expected number of parameters.
      if declType.inputs.count != expectedType.inputs.count {
        diagnostics.append(
          .diagnose(
            expectedLambdaParameterCount: expectedType.inputs.count,
            found: declType.inputs.count,
            at: checker.program.ast[id].origin))
        assignToError(id)
        return
      }

      // Check that the declaration defines the expected argument labels.
      if !declType.labels.elementsEqual(expectedType.labels) {
        diagnostics.append(
          .diagnose(
            labels: Array(declType.labels),
            incompatibleWith: Array(expectedType.labels),
            at: checker.program.ast[id].origin))
        assignToError(id)
        return
      }
    } else if declType.output.base is TypeVariable {
      if case .expr(let body) = checker.program.ast[checker.program.ast[id].decl].body {
        // Infer the return type of the lambda from its body.
        inferredTypes[body] = declType.output
        expectedTypes[body] = declType.output

        let currentScope = scope
        scope = AnyScopeID(checker.program.ast[id].decl)
        visit(expr: body, using: &checker)
        scope = currentScope
      } else {
        // The system is underspecified.
        let origin = checker.program.ast[checker.program.ast[id].decl].introducerRange
        diagnostics.append(.diagnose(cannotInferComplexReturnTypeAt: origin))
        assignToError(id)
        return
      }
    }

    assume(typeOf: id, equals: declType, at: checker.program.ast[id].origin)
  }

  private mutating func visit(
    mapLiteral i: NodeID<MapLiteralExpr>,
    using checker: inout TypeChecker
  ) {
    fatalError("not implemented")
  }

  private mutating func visit(
    match i: NodeID<MatchExpr>,
    using checker: inout TypeChecker
  ) {
    fatalError("not implemented")
  }

  private mutating func visit(
    name id: NodeID<NameExpr>,
    using checker: inout TypeChecker
  ) {
    defer { assert(inferredTypes[id] != nil) }

    // Resolve the nominal prefix of the expression.
    let resolution = checker.resolve(nominalPrefixOf: id, from: scope)
    visit(name: id, withNameResolutionResult: resolution, using: &checker)
  }

  private mutating func visit(
    name id: NodeID<NameExpr>,
    withNameResolutionResult resolution: TypeChecker.NameResolutionResult,
    using checker: inout TypeChecker
  ) {
    var parentType: AnyType?
    let unresolvedComponents: [NodeID<NameExpr>]

    switch resolution {
    case .failed(let undefinedComponent, let parentType):
      let name = checker.program.ast[undefinedComponent].name
      diagnostics.append(.diagnose(undefinedName: name.value, in: parentType, at: name.origin))
      assignToError(id)
      return

    case .inexecutable(let suffix):
      if case .expr(let domainExpr) = checker.program.ast[id].domain {
        visit(expr: domainExpr, using: &checker)
        parentType = inferredTypes[domainExpr]!
      } else {
        fatalError("not implemented")
      }
      unresolvedComponents = suffix

    case .done(let prefix, let suffix):
      for p in prefix {
        constrain(p.component, to: p.candidates, using: &checker)
        parentType = inferredTypes[p.component]
      }

      unresolvedComponents = suffix
    }

    // Create the necessary constraints to let the solver resolve the remaining components.
    for component in unresolvedComponents {
      let componentOrigin = checker.program.ast[component].origin
      let memberType = expectedTypes[component] ?? ^TypeVariable(node: AnyNodeID(component))
      constraints.append(
        MemberConstraint(
          parentType!, hasMemberExpressedBy: component, ofType: memberType,
          in: checker.program.ast,
          cause: ConstraintCause(.member, at: componentOrigin)))
      assume(typeOf: component, equals: memberType, at: componentOrigin)
      parentType = memberType
    }
  }

  private mutating func visit(
    nil i: NodeID<NilLiteralExpr>,
    using checker: inout TypeChecker
  ) {
    fatalError("not implemented")
  }

  private mutating func visit(
    sequence id: NodeID<SequenceExpr>,
    using checker: inout TypeChecker
  ) {
    defer { assert(inferredTypes[id] != nil) }

    // Fold the sequence and visit its sub-expressions.
    let foldedSequence = fold(sequenceExpr: id, using: &checker)
    checker.foldedSequenceExprs[id] = foldedSequence

    // Generate constraints from the folded sequence.
    let inferredRootType = visit(
      foldedSequence: foldedSequence,
      expectingRootType: expectedTypes[id],
      using: &checker)
    assume(typeOf: id, equals: inferredRootType, at: checker.program.ast[id].origin)
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
          lhsType, hasMemberExpressedBy: callee.expr, ofType: ^calleeType, in: checker.program.ast,
          cause: ConstraintCause(.member, at: checker.program.ast[callee.expr].origin)))

      return outputType

    case .leaf(let expr):
      expectedTypes[expr] = expectedRootType
      visit(expr: expr, using: &checker)
      return inferredTypes[expr]!
    }
  }

  private mutating func visit(
    spawn id: NodeID<SpawnExpr>,
    using checker: inout TypeChecker
  ) {
    fatalError("not implemented")
  }

  private mutating func visit(
    stringLiteral i: NodeID<StringLiteralExpr>,
    using checker: inout TypeChecker
  ) {
    fatalError("not implemented")
  }

  private mutating func visit(
    subscriptCall id: NodeID<SubscriptCallExpr>,
    using checker: inout TypeChecker
  ) {
    defer { assert(inferredTypes[id] != nil) }

    // Infer the type of the callee.
    let callee = checker.program.ast[id].callee
    visit(expr: callee, using: &checker)

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
      assignToError(id)
      return
    }

    // Case 2
    if let calleeType = TypeVariable(inferredTypes[callee]!) {
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
          ^calleeType, ^assumedCalleeType,
          because: ConstraintCause(.callee, at: checker.program.ast[callee].origin)))

      assume(typeOf: id, equals: returnType, at: checker.program.ast[id].origin)
      return
    }

    // Case 3a
    if let calleeType = SubscriptType(inferredTypes[callee]!) {
      if visit(
        arguments: checker.program.ast[id].arguments,
        of: checker.program.ast[id].callee,
        expecting: calleeType.inputs,
        using: &checker)
      {
        assume(typeOf: id, equals: calleeType.output, at: checker.program.ast[id].origin)
      } else {
        assignToError(id)
      }
      return
    }

    // Case 3b
    if let c = NodeID<NameExpr>(callee),
      let d = checker.referredDecls[c]?.decl,
      checker.isNominalTypeDecl(d)
    {
      assert(inferredTypes[callee]?.base is MetatypeType)

      // Buffer type expressions shall have exactly one argument.
      if checker.program.ast[id].arguments.count != 1 {
        diagnostics.append(
          .diagnose(invalidBufferTypeExprArgumentCount: id, in: checker.program.ast))
        assignToError(id)
        return
      }

      // Note: We'll need some form of compile-time evaluation here.
      fatalError("not implemented")
    }

    // Case 3c
    let candidates = checker.lookup("[]", memberOf: inferredTypes[callee]!, inScope: scope)
    switch candidates.count {
    case 0:
      diagnostics.append(
        .diagnose(
          noUnnamedSubscriptsIn: inferredTypes[callee]!,
          at: checker.program.ast[checker.program.ast[id].callee].origin))
      assignToError(id)

    case 1:
      // If there's a single candidate, we're looking at case 3a.
      let decl = candidates.first!
      let declType = checker.realize(decl: decl)
      assert(decl.kind == SubscriptDecl.self)

      // Bail out if we can't get the type of the referred declaration.
      if declType.isError {
        assignToError(id)
        return
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
        assume(typeOf: id, equals: calleeType.output, at: checker.program.ast[id].origin)
      } else {
        assignToError(id)
        return
      }

      // Register the callee's constraints.
      constraints.append(contentsOf: declConstraints)

      // Update the referred declaration map if necessary.
      if let c = NodeID<NameExpr>(callee) {
        checker.referredDecls[c] = .member(decl)
      }

    default:
      // Note: Create an overload constraint.
      fatalError("not implemented")
    }
  }

  private mutating func visit(
    tuple id: NodeID<TupleExpr>,
    using checker: inout TypeChecker
  ) {
    defer { assert(inferredTypes[id] != nil) }

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
        visit(expr: tupleExpr[i].value, using: &checker)
        tupleTypeElements.append(
          TupleType.Element(
            label: tupleExpr[i].label?.value,
            type: inferredTypes[tupleExpr[i].value]!))
      }
    } else {
      for i in 0 ..< tupleExpr.count {
        visit(expr: tupleExpr[i].value, using: &checker)
        tupleTypeElements.append(
          TupleType.Element(
            label: tupleExpr[i].label?.value,
            type: inferredTypes[tupleExpr[i].value]!))
      }
    }

    assume(typeOf: id, equals: TupleType(tupleTypeElements), at: checker.program.ast[id].origin)
  }

  private mutating func visit(
    tupleMember id: NodeID<TupleMemberExpr>,
    using checker: inout TypeChecker
  ) {
    fatalError("not implemented")
  }

  private mutating func visit(
    unicodeScalarLiteral id: NodeID<UnicodeScalarLiteralExpr>,
    using checker: inout TypeChecker
  ) {
    fatalError("not implemented")
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
      diagnostics.append(
        .diagnose(
          labels: argumentLabels,
          incompatibleWith: parameterLabels,
          at: checker.program.ast[callee].origin))
      return false
    }

    // Propagate type information down.
    for i in 0 ..< arguments.count {
      let argumentExpr = arguments[i].value
      let argumentType = ^TypeVariable(node: argumentExpr.base)
      let parameterType = parameters[i].type

      inferredTypes[argumentExpr] = argumentType
      constraints.append(
        ParameterConstraint(
          argumentType, parameterType,
          because: ConstraintCause(.argument, at: checker.program.ast[argumentExpr].origin)))

      if let type = ParameterType(parameterType) {
        expectedTypes[argumentExpr] = type.bareType
      }
      visit(expr: argumentExpr, using: &checker)
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
      // Infer the type of the argument bottom-up.
      visit(expr: arguments[i].value, using: &checker)

      let argument = arguments[i].value
      let parameterType = ^TypeVariable()
      constraints.append(
        ParameterConstraint(
          inferredTypes[argument]!, parameterType,
          because: ConstraintCause(.argument, at: checker.program.ast[argument].origin)))

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

  private mutating func assume<ID: ExprID, T: TypeProtocol>(
    typeOf id: ID,
    equals inferredType: T,
    at range: SourceRange?
  ) {
    if let ty = inferredTypes[id] {
      if ty != inferredType {
        constraints.append(
          EqualityConstraint(^inferredType, ty, because: ConstraintCause(.structural, at: range)))
      }
    } else {
      inferredTypes[id] = ^inferredType
    }
  }

  private mutating func assignToError<ID: ExprID>(_ id: ID) {
    inferredTypes[id] = .error
  }

  /// - Requires: `candidates` is not empty
  private mutating func constrain(
    _ name: NodeID<NameExpr>,
    to candidates: [(reference: DeclRef, type: AnyType)],
    using checker: inout TypeChecker
  ) {
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
      assume(typeOf: name, equals: nameType, at: constrainOrigin)
      constraints.append(contentsOf: nameConstraints)
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
      assume(typeOf: name, equals: nameType, at: constrainOrigin)
    }
  }

}
