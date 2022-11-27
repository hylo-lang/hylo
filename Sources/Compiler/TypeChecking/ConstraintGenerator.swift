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
    case AssignExpr.self:
      return visit(assign: NodeID(rawValue: expr.rawValue), using: &checker)
    case AsyncExpr.self:
      return visit(async: NodeID(rawValue: expr.rawValue), using: &checker)
    case AwaitExpr.self:
      return visit(await: NodeID(rawValue: expr.rawValue), using: &checker)
    case BooleanLiteralExpr.self:
      return visit(booleanLiteral: NodeID(rawValue: expr.rawValue), using: &checker)
    case CastExpr.self:
      return visit(cast: NodeID(rawValue: expr.rawValue), using: &checker)
    case CondExpr.self:
      return visit(cond: NodeID(rawValue: expr.rawValue), using: &checker)
    case FloatLiteralExpr.self:
      return visit(floatLiteral: NodeID(rawValue: expr.rawValue), using: &checker)
    case FunCallExpr.self:
      return visit(funCall: NodeID(rawValue: expr.rawValue), using: &checker)
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
    case NilExpr.self:
      return visit(nil: NodeID(rawValue: expr.rawValue), using: &checker)
    case SequenceExpr.self:
      return visit(sequence: NodeID(rawValue: expr.rawValue), using: &checker)
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
    assign id: NodeID<AssignExpr>,
    using checker: inout TypeChecker
  ) {
    // Infer the type on the left.
    let lhs = checker.program.ast[id].left
    visit(expr: lhs, using: &checker)

    // Constrain the right to be subtype of the left.
    let rhs = checker.program.ast[id].right
    inferredTypes[rhs] = ^TypeVariable(node: rhs.base)
    constraints.append(
      equalityOrSubtypingConstraint(
        inferredTypes[rhs]!,
        inferredTypes[lhs]!,
        because: ConstraintCause(.initializationOrAssignment, at: checker.program.ast[id].origin)))

    // Infer the type on the right.
    expectedTypes[rhs] = inferredTypes[lhs]
    visit(expr: rhs, using: &checker)

    // Assignments have the void type.
    assume(typeOf: id, equals: AnyType.void, at: checker.program.ast[id].origin)
  }

  private mutating func visit(
    async id: NodeID<AsyncExpr>,
    using checker: inout TypeChecker
  ) {
    fatalError("not implemented")
  }

  private mutating func visit(
    await id: NodeID<AwaitExpr>,
    using checker: inout TypeChecker
  ) {
    fatalError("not implemented")
  }

  private mutating func visit(
    booleanLiteral id: NodeID<BooleanLiteralExpr>,
    using checker: inout TypeChecker
  ) {
    let boolType = checker.program.ast.coreType(named: "Bool")!
    assume(typeOf: id, equals: boolType, at: checker.program.ast[id].origin)
  }

  private mutating func visit(
    bufferLiteral id : NodeID<BufferLiteralExpr>,
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
          inferredTypes[lhs]!,
          target,
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
    funCall id: NodeID<FunCallExpr>,
    using checker: inout TypeChecker
  ) {
    defer { assert(inferredTypes[id] != nil) }

    // Infer the type of the callee.
    let callee = checker.program.ast[id].callee
    visit(expr: callee, using: &checker)

    // There are four cases to consider:
    // 1. We failed to infer the type of the callee. In that case there's nothing more we can do.
    // 2. We determined that the callee refers to a nominal type declaration. In that case, we
    //    desugar a constructor call.
    // 3. We determined the exact type of the callee, and its a callable. In that case, we may
    //    propagate that information top-down to refine the inference of the arguments' types.
    // 4. We couldn't infer the exact type of the callee and must rely on bottom-up inference to
    //    further refine type inference.

    // 1st case
    if inferredTypes[callee]!.isError {
      assignToError(id)
      return
    }

    // 2nd case
    if let c = NodeID<NameExpr>(callee),
       let d = checker.referredDecls[c]?.decl,
       checker.doesSupportInitSugar(d)
    {
      switch d.kind {
      case ProductTypeDecl.self:
        let initializers = checker.resolve(
          Name(stem: "init"),
          introducedInDeclSpaceOf: AnyScopeID(d)!,
          inScope: scope)

        // Select suitable candidates based on argument labels.
        let labels = checker.program.ast[id].arguments.map({ $0.label?.value })
        var candidates: [OverloadConstraint.Candidate] = []
        for initializer in initializers {
          // Remove the receiver from the parameter list.
          let ctor = (initializer.type.base as! LambdaType).ctor()!

          if labels.elementsEqual(ctor.labels) {
            let (ty, cs) = checker.open(type: ^ctor)
            candidates.append(OverloadConstraint.Candidate(
              reference: .direct(initializer.decl),
              type: ty,
              constraints: cs,
              penalties: 0))
          }
        }

        switch candidates.count {
        case 0:
          let name = Name(stem: "init", labels: labels)
          diagnostics.append(
            .diagnose(undefinedName: "\(name)", at: checker.program.ast[c].name.origin))
          assignToError(id)
          return

        case 1:
          // Reassign the referred declaration and type of the name expression.
          checker.referredDecls[c] = candidates[0].reference
          inferredTypes[c] = candidates[0].type

          // Propagate the type of the constructor down.
          let outputType = propagateDown(
            callee: callee,
            calleeType: candidates[0].type.base as! LambdaType,
            calleeTypeConstraints: candidates[0].constraints,
            arguments: checker.program.ast[id].arguments,
            using: &checker)
          assume(typeOf: id, equals: outputType, at: checker.program.ast[id].origin)

        default:
          // TODO: Handle specializations
          fatalError("not implemented")
        }

      case TraitDecl.self:
        let trait = TraitType(NodeID(rawValue: d.rawValue), ast: checker.program.ast)
        diagnostics.append(
          .diagnose(cannotConstructTrait: trait, at: checker.program.ast[callee].origin))
        assignToError(id)

      case TypeAliasDecl.self:
        fatalError("not implemented")

      default:
        unreachable("unexpected declaration")
      }

      return
    }

    // 3rd case
    if let calleeType = inferredTypes[callee]!.base as? LambdaType {
      let outputType = propagateDown(
        callee: callee,
        calleeType: calleeType,
        calleeTypeConstraints: [],
        arguments: checker.program.ast[id].arguments,
        using: &checker)
      assume(typeOf: id, equals: outputType, at: checker.program.ast[id].origin)
      return
    }

    // 4th case
    let output = expectedTypes[id] ?? ^TypeVariable(node: AnyNodeID(id))
    assume(typeOf: id, equals: output, at: checker.program.ast[id].origin)

    var inputs: [CallableTypeParameter] = []
    for i in 0 ..< checker.program.ast[id].arguments.count {
      // Infer the type of the argument bottom-up.
      visit(expr: checker.program.ast[id].arguments[i].value, using: &checker)

      let argument = checker.program.ast[id].arguments[i].value
      let parameterType = ^TypeVariable()
      constraints.append(
        ParameterConstraint(
          inferredTypes[argument]!,
          parameterType,
          because: ConstraintCause(.argument, at: checker.program.ast[argument].origin)))

      let argumentLabel = checker.program.ast[id].arguments[i].label?.value
      inputs.append(CallableTypeParameter(label: argumentLabel, type: parameterType))
    }

    // Constrain the type of the callee.
    let calleeType = LambdaType(
      environment: ^TypeVariable(),
      inputs: inputs,
      output: output)
    constraints.append(
      EqualityConstraint(
        inferredTypes[callee]!,
        ^calleeType,
        because: ConstraintCause(.callee, at: checker.program.ast[callee].origin)))
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
      let intType = checker.program.ast.coreType(named: "Int")!

      constraints.append(
        DisjunctionConstraint(
          choices: [
            .init(
              constraints: [
                EqualityConstraint(^tau, ^intType, because: cause)
              ],
              penalties: 0),
            .init(
              constraints: [
                ConformanceConstraint(^tau, traits: [trait], because: cause)
              ],
              penalties: 1),
            ],
          because: cause))
      assume(typeOf: id, equals: tau, at: checker.program.ast[id].origin)

    case .some(let expectedType):
      // The type of has been fixed; constrain it to conform to `ExpressibleByIntegerLiteral`.
      constraints.append(
        ConformanceConstraint(
          ^expectedType,
          traits: [trait],
          because: cause))
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
    guard let declType = checker.realize(underlyingDeclOf: id)?.base as? LambdaType else {
      assignToError(id)
      return
    }

    // Schedule the underlying declaration to be type-checked.
    checker.deferTypeChecking(id)

    if let expectedType = expectedTypes[id]!.base as? LambdaType {
      // Check that the declaration defines the expected number of parameters.
      if declType.inputs.count != expectedType.inputs.count {
        diagnostics.append(.diagnose(
          expectedLambdaParameterCount: expectedType.inputs.count,
          found: declType.inputs.count,
          at: checker.program.ast[id].origin))
        assignToError(id)
        return
      }

      // Check that the declaration defines the expected argument labels.
      if !declType.labels.elementsEqual(expectedType.labels) {
        diagnostics.append(.diagnose(
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
    // Resolves the name.
    switch checker.program.ast[id].domain {
    case .none:
      let expr = checker.program.ast[id]
      if checker.isBuiltinModuleVisible && (expr.name.value.stem == "Builtin") {
        assume(typeOf: id, equals: BuiltinType.module, at: checker.program.ast[id].origin)
        checker.referredDecls[id] = .direct(AnyDeclID(checker.program.ast.builtinDecl))
        return
      }

      let candidates = checker.resolve(expr.name.value, inScope: scope)
      if candidates.isEmpty {
        diagnostics.append(
          .diagnose(undefinedName: expr.name.value.description, at: expr.name.origin))
        assignToError(id)
        return
      }

      let inferredType: AnyType
      if candidates.count == 1 {
        // Contextualize the match.
        let context = checker.program.declToScope[candidates[0].decl]!
        let (ty, cs) = checker.contextualize(
          type: candidates[0].type,
          inScope: context,
          cause: ConstraintCause(.binding, at: checker.program.ast[id].origin))
        inferredType = ty

        // Register associated constraints.
        constraints.append(contentsOf: cs)

        // Bind the name expression to the referred declaration.
        if checker.program.isNonStaticMember(candidates[0].decl) {
          checker.referredDecls[id] = .member(candidates[0].decl)
        } else {
          checker.referredDecls[id] = .direct(candidates[0].decl)
        }
      } else {
        // TODO: Create an overload constraint
        fatalError("not implemented")
      }

      assume(typeOf: id, equals: inferredType, at: checker.program.ast[id].origin)

    case .expr(let domain):
      // Infer the type of the domain.
      visit(expr: domain, using: &checker)
      let domainType = inferredTypes[domain]!

      // If we failed to infer the type of the domain, there's nothing more we can do.
      if case .error = domainType {
        assignToError(id)
        return
      }

      // Handle references to built-in symbols.
      if domainType == .builtin(.module) {
        let symbolName = checker.program.ast[id].name.value.stem

        if let type = BuiltinSymbols[symbolName] {
          assume(typeOf: id, equals: type, at: checker.program.ast[id].origin)
          checker.referredDecls[id] = .direct(AnyDeclID(checker.program.ast.builtinDecl))
        } else if let type = BuiltinType(symbolName) {
          assume(typeOf: id, equals: type, at: checker.program.ast[id].origin)
          checker.referredDecls[id] = .direct(AnyDeclID(checker.program.ast.builtinDecl))
        } else {
          diagnostics.append(
            .diagnose(undefinedName: symbolName, at: checker.program.ast[id].name.origin))
          assignToError(id)
        }

        return
      }

      // Map the expression to a fresh variable unless we have top-down type information.
      let inferredType = expectedTypes[id] ?? ^TypeVariable(node: AnyNodeID(id))
      assume(typeOf: id, equals: inferredType, at: checker.program.ast[id].origin)

      // If we determined that the domain refers to a nominal type declaration, create a static
      // member constraint. Otherwise, create a non-static member constraint.
      let cause = ConstraintCause(.member, at: checker.program.ast[id].origin)

      if let base = NodeID<NameExpr>(domain),
         let decl = checker.referredDecls[base]?.decl,
         checker.doesSupportInitSugar(decl)
      {
        constraints.append(UnboundMemberConstraint(
          type: domainType,
          hasMemberExpressedBy: id,
          in: checker.program.ast,
          ofType: inferredType,
          because: cause))
      } else {
        // FIXME: We can't assume the domain is an instance if types are first-class.
        constraints.append(BoundMemberConstraint(
          type: domainType,
          hasMemberExpressedBy: id,
          in: checker.program.ast,
          ofType: inferredType,
          because: cause))
      }

    case .type:
      fatalError("not implemented")

    case .implicit:
      fatalError("not implemented")
    }
  }

  private mutating func visit(
    nil i: NodeID<NilExpr>,
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
          rhsType,
          parameterType,
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
        BoundMemberConstraint(
          type: lhsType,
          hasMemberExpressedBy: callee.expr,
          in: checker.program.ast,
          ofType: ^calleeType,
          because: ConstraintCause(.member, at: checker.program.ast[callee.expr].origin)))

      return outputType

    case .leaf(let expr):
      expectedTypes[expr] = expectedRootType
      visit(expr: expr, using: &checker)
      return inferredTypes[expr]!
    }
  }

  private mutating func visit(
    stringLiteral i: NodeID<StringLiteralExpr>,
    using checker: inout TypeChecker
  ) {
    fatalError("not implemented")
  }

  private mutating func visit(
    subscriptCall i: NodeID<SubscriptCallExpr>,
    using checker: inout TypeChecker
  ) {
    fatalError("not implemented")
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
    if let type = expectedTypes[id]?.base as? TupleType,
       type.elements.elementsEqual(tupleExpr, by: { (a, b) in a.label == b.label?.value })
    {
      for i in 0 ..< tupleExpr.count {
        expectedTypes[tupleExpr[i].value] = type.elements[i].type
        visit(expr: tupleExpr[i].value, using: &checker)
        tupleTypeElements.append(TupleType.Element(
          label: tupleExpr[i].label?.value,
          type: inferredTypes[tupleExpr[i].value]!))
      }
    } else {
      for i in 0 ..< tupleExpr.count {
        visit(expr: tupleExpr[i].value, using: &checker)
        tupleTypeElements.append(TupleType.Element(
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

  private mutating func propagateDown(
    callee: AnyExprID,
    calleeType: LambdaType,
    calleeTypeConstraints: ConstraintSet,
    arguments: [CallArgument],
    using checker: inout TypeChecker
  ) -> AnyType {
    // Collect the argument labels.
    let argumentLabels = arguments.map({ $0.label?.value })

    // Check that the labels inferred from the callee are consistent with that of the call.
    let calleeLabels = calleeType.inputs.map({ $0.label })
    if calleeLabels != argumentLabels {
      diagnostics.append(.diagnose(
        labels: argumentLabels,
        incompatibleWith: calleeLabels,
        at: checker.program.ast[callee].origin))
      return .error
    }

    // Gather the callee's constraints.
    for c in calleeTypeConstraints {
      var newConstraint = c
      newConstraint.cause = ConstraintCause(.callee, at: checker.program.ast[callee].origin)
      constraints.append(newConstraint)
    }

    // Propagate type information to the arguments.
    for i in 0 ..< arguments.count {
      let argumentExpr = arguments[i].value
      let argumentType = ^TypeVariable(node: argumentExpr.base)
      let parameterType = calleeType.inputs[i].type

      inferredTypes[argumentExpr] = argumentType
      constraints.append(
        ParameterConstraint(
          argumentType,
          parameterType,
          because: ConstraintCause(.argument, at: checker.program.ast[argumentExpr].origin)))

      if let type = parameterType.base as? ParameterType {
        expectedTypes[argumentExpr] = type.bareType
      }
      visit(expr: argumentExpr, using: &checker)
    }

    // Constrain the type of the call.
    return calleeType.output
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
        checker.diagnostics.insert(.diagnose(
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

}
