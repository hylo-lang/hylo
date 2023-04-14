import BigInt
import Core
import FrontEnd
import Utils

/// The conversion of typed Val module AST into corresponding C++ AST.
public struct CXXTranspiler {

  public static let coreLibModuleName = "ValCore"

  /// The Val typed nodes.
  ///
  /// This property is used when we need access to the contents of a node from its ID.
  let wholeValProgram: TypedProgram

  /// Creates an instance.
  public init(_ wholeValProgram: TypedProgram) {
    self.wholeValProgram = wholeValProgram
  }

  /// Returns a C++ AST implementing the semantics of `source`.
  public func cxx(_ source: ModuleDecl.Typed) -> CXXModule {
    let isCoreLibrary = source.id == wholeValProgram.coreLibrary?.id
    return CXXModule(
      name: isCoreLibrary ? Self.coreLibModuleName : source.baseName,
      isCoreLibrary: isCoreLibrary,
      topLevelDecls: source.topLevelDecls.map({ cxx(topLevel: $0) }),
      entryPointBody: entryPointBody(module: source))
  }

  // MARK: Declarations

  /// Returns a transpilation of `source`.
  func cxx(topLevel source: AnyDeclID.TypedNode) -> CXXTopLevelDecl {
    switch source.kind {
    case FunctionDecl.self:
      return cxx(function: FunctionDecl.Typed(source)!)
    case ProductTypeDecl.self:
      return cxx(type: ProductTypeDecl.Typed(source)!)
    case OperatorDecl.self:
      return CXXComment(comment: "operator decl")
    case ConformanceDecl.self:
      return CXXComment(comment: "conformance decl")
    case TraitDecl.self:
      return CXXComment(comment: "trait decl")
    case TypeAliasDecl.self:
      return CXXComment(comment: "type alias decl")
    default:
      unexpected(source)
    }
  }

  /// Returns a transpilation of `source`.
  func cxx(function source: FunctionDecl.Typed) -> CXXFunctionDecl {
    assert(wholeValProgram.isGlobal(source.id))

    let functionName = source.identifier?.value ?? ""

    return CXXFunctionDecl(
      identifier: CXXIdentifier(functionName),
      output: cxxFunctionReturnType(source, with: functionName),
      parameters: source.parameters.map {
        CXXFunctionDecl.Parameter(CXXIdentifier($0.baseName), cxx(typeExpr: $0.type))
      },
      body: source.body != nil ? cxx(funBody: source.body!) : nil)
  }

  /// Returns a transpilation of the return type `source`.
  private func cxxFunctionReturnType(_ source: FunctionDecl.Typed, with name: String)
    -> CXXTypeExpr  // TODO: this will be refactored by follow-up patches.
  {
    switch source.type.base {
    case let valDeclType as LambdaType:
      return cxx(typeExpr: valDeclType.output, asReturnType: true)

    case is MethodType:
      fatalError("not implemented")

    default:
      unreachable("unexpected type")
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(funBody body: FunctionDecl.Typed.Body) -> CXXScopedBlock {
    switch body {
    case .block(let bodyDetails):
      return cxx(brace: bodyDetails)

    case .expr(let bodyDetails):
      return CXXScopedBlock(stmts: [CXXReturnStmt(expr: cxx(expr: bodyDetails))])
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(type source: ProductTypeDecl.Typed) -> CXXClassDecl {
    assert(wholeValProgram.isGlobal(source.id))
    return CXXClassDecl(
      name: CXXIdentifier(source.identifier.value),
      members: source.members.flatMap({ cxx(member: $0) }))
  }

  /// Returns a transpilation of `source`.
  private func cxx(member source: AnyDeclID.TypedNode) -> [CXXClassDecl.ClassMember] {
    switch source.kind {
    case BindingDecl.self:
      // One binding can expand into multiple class attributes
      return cxx(productTypeBinding: BindingDecl.Typed(source)!)

    case InitializerDecl.self:
      return [cxx(initializer: InitializerDecl.Typed(source)!)]

    case MethodDecl.self, FunctionDecl.self:
      return [.method]

    default:
      unexpected(source)
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(productTypeBinding source: BindingDecl.Typed) -> [CXXClassDecl.ClassMember] {
    return source.pattern.subpattern.names.map({
      .attribute(
        CXXClassAttribute(
          type: cxx(typeExpr: $0.pattern.decl.type),
          name: CXXIdentifier($0.pattern.decl.baseName),
          initializer: nil,  // TODO
          isStatic: source.isStatic))
    })
  }
  /// Returns a transpilation of `source`.
  private func cxx(initializer source: InitializerDecl.Typed) -> CXXClassDecl.ClassMember {
    let parentType = enclosingProductType(source)
    switch source.introducer.value {
    case .`init`:
      return .constructor(
        CXXConstructor(
          name: CXXIdentifier(parentType.baseName),
          parameters: source.parameters.map {
            CXXConstructor.Parameter(name: CXXIdentifier($0.baseName), type: cxx(typeExpr: $0.type))
          },
          initializers: [],
          body: source.body != nil ? cxx(brace: source.body!) : nil))
    case .memberwiseInit:
      let attributes = nonStaticAttributes(parentType)
      return .constructor(
        CXXConstructor(
          name: CXXIdentifier(parentType.baseName),
          parameters: attributes.map {
            return (name: CXXIdentifier($0.name), type: cxx(typeExpr: $0.type))
          },
          initializers: attributes.map {
            return (name: CXXIdentifier($0.name), value: CXXIdentifier($0.name))
          },
          body: nil))
    }
  }

  /// An attribute of a product type.
  typealias Attribute = (name: String, type: AnyType)

  /// Returns the list of non-statuc attributes for `source`.
  private func nonStaticAttributes(_ source: ProductTypeDecl.Typed) -> [Attribute] {
    return source.decls.lazy.flatMap({ (d) -> [Attribute] in
      // First check for binding declarations inside the product type
      guard let binding = BindingDecl.Typed(d) else {
        return []
      }
      // Then, for each binding declaration iterate over the subpatterns to get the attributes.
      let r = binding.pattern.subpattern.names.map({
        Attribute(name: $0.pattern.decl.baseName, type: $0.pattern.decl.type)
      })
      return r
    })

  }

  /// Returns a transpilation of `source`.
  ///
  /// Returned statement can be a `CXXLocalVarDecl` or a `CXXScopedBlock` containing `CXXLocalVarDecl` objects.
  private func cxx(localBinding source: BindingDecl.Typed) -> CXXStmt {
    let cxxInitializer = source.initializer != nil ? cxx(expr: source.initializer!) : nil
    let numNames = source.pattern.subpattern.names.count

    assert(numNames > 0 || cxxInitializer != nil)

    if numNames > 1 {
      let variables = source.pattern.subpattern.names.map({
        cxx(localNamePattern: $0.pattern, with: cxxInitializer)
      })
      // TODO: scoped block is not good here, because of lifetime issues
      return CXXScopedBlock(stmts: variables)
    } else if numNames == 1 {
      return cxx(localNamePattern: source.pattern.subpattern.names[0].pattern, with: cxxInitializer)
    } else if numNames == 0 {
      // No pattern found; just call the initializer, dropping the result.
      return CXXExprStmt(expr: CXXVoidCast(baseExpr: cxxInitializer!))
    } else {
      unreachable()
    }
  }
  /// Returns a transpilation of `source`.
  private func cxx(localNamePattern source: NamePattern.Typed, with cxxInitializer: CXXExpr?)
    -> CXXLocalVarDecl
  {
    CXXLocalVarDecl(
      type: cxx(typeExpr: source.decl.type),
      name: CXXIdentifier(source.decl.identifier.value),
      initializer: cxxInitializer)
  }

  // MARK: Statements

  /// Returns a transpilation of `source`.
  private func cxx<ID: StmtID>(stmt source: ID.TypedNode) -> CXXStmt {
    switch source.kind {
    case BraceStmt.self:
      return cxx(brace: BraceStmt.Typed(source)!)
    case DeclStmt.self:
      return cxx(declStmt: DeclStmt.Typed(source)!)
    case ExprStmt.self:
      return cxx(exprStmt: ExprStmt.Typed(source)!)
    case AssignStmt.self:
      return cxx(assignStmt: AssignStmt.Typed(source)!)
    case ReturnStmt.self:
      return cxx(returnStmt: ReturnStmt.Typed(source)!)
    case ConditionalStmt.self:
      return cxx(conditionalStmt: ConditionalStmt.Typed(source)!)
    case WhileStmt.self:
      return cxx(whileStmt: WhileStmt.Typed(source)!)
    case DoWhileStmt.self:
      return cxx(doWhileStmt: DoWhileStmt.Typed(source)!)
    case ForStmt.self:
      return cxx(forStmt: ForStmt.Typed(source)!)
    case BreakStmt.self:
      return cxx(breakStmt: BreakStmt.Typed(source)!)
    case ContinueStmt.self:
      return cxx(continueStmt: ContinueStmt.Typed(source)!)
    case YieldStmt.self:
      return cxx(yieldStmt: YieldStmt.Typed(source)!)
    default:
      unexpected(source)
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(brace source: BraceStmt.Typed) -> CXXScopedBlock {
    return CXXScopedBlock(stmts: Array(source.stmts.map({ cxx(stmt: $0) })))
  }

  /// Returns a transpilation of `source`.
  private func cxx(declStmt source: DeclStmt.Typed) -> CXXStmt {
    switch source.decl.kind {
    case BindingDecl.self:
      return cxx(localBinding: BindingDecl.Typed(source.decl)!)
    default:
      unexpected(source)
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(exprStmt source: ExprStmt.Typed) -> CXXStmt {
    return CXXExprStmt(expr: cxx(expr: source.expr))
  }

  /// Returns a transpilation of `source`.
  private func cxx(assignStmt source: AssignStmt.Typed) -> CXXExprStmt {
    return CXXExprStmt(
      expr: CXXInfixExpr(
        oper: .assignment,
        lhs: cxx(expr: source.left),
        rhs: cxx(expr: source.right)))
  }

  /// Returns a transpilation of `source`.
  private func cxx(returnStmt source: ReturnStmt.Typed) -> CXXReturnStmt {
    return CXXReturnStmt(expr: source.value != nil ? cxx(expr: source.value!) : nil)
  }

  /// Returns a transpilation of `source`.
  private func cxx(conditionalStmt source: ConditionalStmt.Typed) -> CXXIfStmt {
    CXXIfStmt(
      condition: cxx(condition: source.condition),
      trueStmt: cxx(brace: source.success),
      falseStmt: source.failure.map({ cxx(stmt: $0) }))
  }

  /// Returns a transpilation of `source`.
  private func cxx(whileStmt source: WhileStmt.Typed) -> CXXWhileStmt {
    CXXWhileStmt(
      condition: cxx(condition: source.condition),
      body: cxx(stmt: source.body))
  }
  /// Returns a transpilation of `source`.
  private func cxx(doWhileStmt source: DoWhileStmt.Typed) -> CXXDoWhileStmt {
    return CXXDoWhileStmt(
      body: cxx(stmt: source.body),
      condition: cxx(expr: source.condition))
  }
  /// Returns a transpilation of `source`.
  private func cxx(forStmt source: ForStmt.Typed) -> CXXStmt {
    return CXXComment(comment: "ForStmt")
  }
  /// Returns a transpilation of `source`.
  private func cxx(breakStmt source: BreakStmt.Typed) -> CXXBreakStmt {
    return CXXBreakStmt()
  }
  /// Returns a transpilation of `source`.
  private func cxx(continueStmt source: ContinueStmt.Typed) -> CXXContinueStmt {
    return CXXContinueStmt()
  }
  /// Returns a transpilation of `source`.
  private func cxx(yieldStmt source: YieldStmt.Typed) -> CXXStmt {
    return CXXComment(comment: "YieldStmt")
  }

  /// Returns a transpilation of `condition`.
  private func cxx(condition: [ConditionItem]) -> CXXExpr {
    // TODO: multiple conditions
    // TODO: bindings in conditions
    if let c = condition.uniqueElement {
      switch c {
      case .expr(let e):
        return cxx(expr: wholeValProgram[e])
      case .decl(_):
        return CXXComment(comment: "binding condition")
      }
    } else {
      fatalError("not implemented")
    }
  }

  // MARK: Expressions

  /// Returns a transpilation of `source`.
  private func cxx(expr source: AnyExprID.TypedNode) -> CXXExpr {
    switch source.kind {
    case BooleanLiteralExpr.self:
      return cxx(booleanLiteral: BooleanLiteralExpr.Typed(source)!)
    case IntegerLiteralExpr.self:
      return cxx(integerLiteral: IntegerLiteralExpr.Typed(source)!)
    case NameExpr.self:
      return cxx(name: NameExpr.Typed(source)!)
    case SequenceExpr.self:
      return cxx(sequence: SequenceExpr.Typed(source)!)
    case FunctionCallExpr.self:
      return cxx(functionCall: FunctionCallExpr.Typed(source)!)
    case ConditionalExpr.self:
      return cxx(cond: ConditionalExpr.Typed(source)!)
    case InoutExpr.self:
      return cxx(`inout`: InoutExpr.Typed(source)!)
    default:
      unexpected(source)
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(booleanLiteral source: BooleanLiteralExpr.Typed) -> CXXBooleanLiteralExpr {
    return CXXBooleanLiteralExpr(value: source.value)
  }

  /// Returns a transpilation of `source`.
  private func cxx(integerLiteral source: IntegerLiteralExpr.Typed) -> CXXExpr {
    return CXXIntegerLiteralExpr(value: source.value)
  }

  /// Returns a transpilation of `source`.
  private func cxx(sequence source: SequenceExpr.Typed) -> CXXExpr {
    return cxx(foldedSequence: source.foldedSequenceExprs!)
  }

  /// Returns a transpilation of `source`.
  private func cxx(foldedSequence source: FoldedSequenceExpr) -> CXXExpr {
    switch source {
    case .infix(let (callee, _), let lhs, let rhs):
      return cxx(
        infix: wholeValProgram[callee],
        lhs: cxx(foldedSequence: lhs),
        rhs: cxx(foldedSequence: rhs))

    case .leaf(let leafNode):
      return cxx(expr: wholeValProgram[leafNode])
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(
    infix source: NameExpr.Typed,
    lhs: CXXExpr,
    rhs: CXXExpr
  ) -> CXXExpr {
    switch nameOfDecl(source.decl.decl!) {
    case "<<": return CXXInfixExpr(oper: .leftShift, lhs: lhs, rhs: rhs)
    case ">>": return CXXInfixExpr(oper: .rightShift, lhs: lhs, rhs: rhs)
    case "*": return CXXInfixExpr(oper: .multiplication, lhs: lhs, rhs: rhs)
    case "/": return CXXInfixExpr(oper: .division, lhs: lhs, rhs: rhs)
    case "%": return CXXInfixExpr(oper: .remainder, lhs: lhs, rhs: rhs)
    case "+": return CXXInfixExpr(oper: .addition, lhs: lhs, rhs: rhs)
    case "-": return CXXInfixExpr(oper: .subtraction, lhs: lhs, rhs: rhs)
    case "==": return CXXInfixExpr(oper: .equality, lhs: lhs, rhs: rhs)
    case "!=": return CXXInfixExpr(oper: .inequality, lhs: lhs, rhs: rhs)
    case "<": return CXXInfixExpr(oper: .lessThan, lhs: lhs, rhs: rhs)
    case "<=": return CXXInfixExpr(oper: .lessEqual, lhs: lhs, rhs: rhs)
    case ">=": return CXXInfixExpr(oper: .greaterEqual, lhs: lhs, rhs: rhs)
    case ">": return CXXInfixExpr(oper: .greaterThan, lhs: lhs, rhs: rhs)
    case "^": return CXXInfixExpr(oper: .bitwiseXor, lhs: lhs, rhs: rhs)
    case "&": return CXXInfixExpr(oper: .bitwiseAnd, lhs: lhs, rhs: rhs)
    case "&&": return CXXInfixExpr(oper: .logicalAnd, lhs: lhs, rhs: rhs)
    case "|": return CXXInfixExpr(oper: .bitwiseOr, lhs: lhs, rhs: rhs)
    case "||": return CXXInfixExpr(oper: .logicalOr, lhs: lhs, rhs: rhs)
    case "<<=": return CXXInfixExpr(oper: .shiftLeftAssignment, lhs: lhs, rhs: rhs)
    case ">>=": return CXXInfixExpr(oper: .shiftRightAssignment, lhs: lhs, rhs: rhs)
    case "*=": return CXXInfixExpr(oper: .mulAssignment, lhs: lhs, rhs: rhs)
    case "/=": return CXXInfixExpr(oper: .divAssignment, lhs: lhs, rhs: rhs)
    case "%=": return CXXInfixExpr(oper: .remAssignment, lhs: lhs, rhs: rhs)
    case "+=": return CXXInfixExpr(oper: .addAssignment, lhs: lhs, rhs: rhs)
    case "-=": return CXXInfixExpr(oper: .divAssignment, lhs: lhs, rhs: rhs)
    case "&=": return CXXInfixExpr(oper: .bitwiseAndAssignment, lhs: lhs, rhs: rhs)

    default:
      // Expand this as a regular function call.
      return CXXFunctionCallExpr(callee: cxx(name: source), arguments: [lhs, rhs])
    }
  }

  /// Returns a transpilation of `source`.
  private func cxx(functionCall source: FunctionCallExpr.Typed) -> CXXExpr {
    // Transpile the arguments
    // TODO: passing conventions
    let arguments = Array(source.arguments.map({ cxx(expr: wholeValProgram[$0.value]) }))

    // Transpile the calee expression
    let callee: CXXExpr
    if let calleeNameExpr = NameExpr.Typed(source.callee) {
      // If the callee is a name expression, we might need the callee type when expanding the name.
      callee = cxx(name: calleeNameExpr)

      // The name expresssion might fully represent an prefix/postfix operator call;
      // in this case, we don't need to wrap this into a function call
      if callee is CXXPrefixExpr || callee is CXXPostfixExpr {
        assert(arguments.isEmpty)
        return callee
      }
    } else {
      callee = cxx(expr: source.callee)
    }

    return CXXFunctionCallExpr(callee: callee, arguments: arguments)
  }

  /// Returns a transpilation of `source`.
  ///
  /// A conditional expression is transpiled using a ternary operator (see `CXXConditionalExpr`)
  /// whenever possible. Otherwise, it is transpiled as a local variable initialized in a an `if`
  /// statement (see `CXXIfStmt`).
  private func cxx(cond source: ConditionalExpr.Typed) -> CXXExpr {
    CXXConditionalExpr(
      condition: cxx(condition: source.condition),
      trueExpr: cxx(expr: source.success),
      falseExpr: cxx(expr: source.failure))
  }

  /// Returns a transpilation of `source`.
  private func cxx(`inout` source: InoutExpr.Typed) -> CXXExpr {
    return cxx(expr: source.subject)
  }

  // MARK: names

  /// Returns a transpilation of `source`.
  ///
  /// This usually result into a C++ identifier, but it can also result in pre-/post-fix operator calls.
  private func cxx(name source: NameExpr.Typed) -> CXXExpr {
    switch source.decl {
    case .direct(let callee) where callee.kind == FunctionDecl.self:
      return cxx(
        nameOfFunction: FunctionDecl.Typed(callee)!, withDomainExpr: source.domainExpr)

    case .direct(let callee) where callee.kind == InitializerDecl.self:
      return cxx(nameOfInitializer: InitializerDecl.Typed(callee)!)

    case .direct(let callee) where callee.kind == ParameterDecl.self:
      let name = nameOfDecl(callee)
      if name == "self" {
        // `self` -> `*this`
        return CXXPrefixExpr(oper: .dereference, base: CXXReceiverExpr())
      } else {
        return CXXIdentifier(nameOfDecl(callee))
      }

    case .direct(let callee):
      // For variables, and other declarations, just use the name of the declaration
      return CXXIdentifier(nameOfDecl(callee))

    case .member(let callee) where callee.kind == FunctionDecl.self:
      return cxx(nameOfMemberFunction: FunctionDecl.Typed(callee)!, withDomain: source.domain)

    case .member(let callee):
      // TODO: revisit this
      return CXXIdentifier(nameOfDecl(callee))

    case .builtinFunction(let f):
      // Decorate the function so that we can uniquely identify it.
      // Example: `native_zeroinitializer_double`.
      let cxxType = cxx(typeExpr: f.type.output)
      return CXXIdentifier("native_\(f.llvmInstruction)_\(cxxType.text)")

    case .builtinType:
      unreachable()
    }
  }

  /// Returns a transpilation of `source`.
  ///
  /// Usually this translates to an identifier, but it can translate to pre-/post-fix operator calls.
  private func cxx(
    nameOfFunction source: FunctionDecl.Typed, withDomainExpr domainExpr: AnyExprID.TypedNode?
  ) -> CXXExpr {
    // Check for prefix && postfix operator calls
    if source.notation != nil && source.notation!.value == .prefix {
      let prefixMapping: [String: CXXPrefixExpr.Operator] = [
        "++": .prefixIncrement,
        "--": .prefixDecrement,
        "+": .unaryPlus,
        "-": .unaryMinus,
        "!": .logicalNot,
      ]
      if let cxxPrefixOperator = prefixMapping[nameOfDecl(source)] {
        return CXXPrefixExpr(
          oper: cxxPrefixOperator, base: cxx(expr: domainExpr!))
      }
    } else if source.notation != nil && source.notation!.value == .postfix {
      let postfixMapping: [String: CXXPostfixExpr.Operator] = [
        "++": .suffixIncrement,
        "--": .suffixDecrement,
      ]
      if let cxxPostfixOperator = postfixMapping[nameOfDecl(source)] {
        return CXXPostfixExpr(
          oper: cxxPostfixOperator, base: cxx(expr: domainExpr!))
      }
    }

    // TODO: handle captures
    return CXXIdentifier(nameOfDecl(source))
  }
  /// Returns a transpilation of `source`.
  private func cxx(nameOfInitializer source: InitializerDecl.Typed) -> CXXExpr {
    return CXXIdentifier(nameOfDecl(enclosingProductType(source)))
  }
  /// Returns a transpilation of `source`.
  private func cxx(
    nameOfMemberFunction source: FunctionDecl.Typed, withDomain domain: NameExpr.Typed.Domain
  ) -> CXXExpr {

    // TODO: revisit this whole function; simplify it, and check for correctness

    var receiver: CXXExpr
    switch domain {
    case .none:
      receiver = CXXReceiverExpr()
    case .expr(let domainDetails):
      receiver = cxx(expr: domainDetails)
    case .implicit:
      fatalError("not implemented")
    }

    // Emit the function reference.
    return CXXInfixExpr(oper: .dotAccess, lhs: receiver, rhs: CXXIdentifier(nameOfDecl(source)))
  }

  // MARK: miscelaneous

  /// Returns a transpilation of `source`.
  private func cxx(
    typeExpr source: AnyType, asReturnType isReturnType: Bool = false
  ) -> CXXTypeExpr {
    switch wholeValProgram.relations.canonical(source).base {
    case .void:
      return CXXTypeExpr(isReturnType ? "void" : "std::monostate")

    case let type as ProductType:
      return CXXTypeExpr(type.name.value)

    case let type as ParameterType:
      // TODO: convention
      return cxx(typeExpr: type.bareType)

    case let type as BuiltinType:
      return cxx(builtinType: type)

    case let type as SumType:
      if type.elements.isEmpty {
        return CXXTypeExpr(isReturnType ? "void" : "std::monostate")
      } else {
        // TODO
        fatalError("not implemented")
      }

    default:
      fatalError("not implemented")
    }
  }
  /// Returns a transpilation of `source`.
  private func cxx(builtinType source: BuiltinType) -> CXXTypeExpr {
    switch source {
    case .i(let bitWidth) where bitWidth == 1:
      return CXXTypeExpr(native: "bool")
    case .i(let bitWidth) where bitWidth == 8:
      return CXXTypeExpr(native: "int8_t")
    case .i(let bitWidth) where bitWidth == 16:
      return CXXTypeExpr(native: "int16_t")
    case .i(let bitWidth) where bitWidth == 32:
      return CXXTypeExpr(native: "int32_t")
    case .i(let bitWidth) where bitWidth == 64:
      return CXXTypeExpr(native: "int64_t")
    case .half:
      fatalError("half floats are not supported in C++")
    case .float:
      return CXXTypeExpr(native: "float")
    case .double:
      return CXXTypeExpr(native: "double")
    case .fp128:
      return CXXTypeExpr(native: "long double")
    case .ptr:
      return CXXTypeExpr(native: "intptr_t")
    default:
      fatalError("builtin type not implemented")
    }
  }

  /// Returns of `source`, without manging and labels.
  private func nameOfDecl<T: DeclID>(_ source: TypedNode<T>) -> String {
    switch source.kind {
    case ConformanceDecl.self, ExtensionDecl.self:
      fatalError("not implemented")

    case FunctionDecl.self:
      return FunctionDecl.Typed(source)!.identifier!.value

    case InitializerDecl.self:
      return "init"

    case MethodDecl.self:
      return MethodDecl.Typed(source)!.identifier.value

    case MethodImpl.self:
      return String(describing: MethodImpl.Typed(source)!.introducer.value)

    case ProductTypeDecl.self:
      return ProductTypeDecl.Typed(source)!.baseName

    case ParameterDecl.self:
      return ParameterDecl.Typed(source)!.identifier.value
    case VarDecl.self:
      return VarDecl.Typed(source)!.identifier.value

    default:
      fatalError("not implemented")
    }

  }

  // MARK: entry point function body

  /// Returns the transpiled body of the entry point function in `source`.
  func entryPointBody(module source: ModuleDecl.Typed) -> CXXScopedBlock? {
    return source.topLevelDecls.first(transformedBy: { (d) in
      if FunctionDecl.Typed(d)?.identifier?.value == "main" {
        return entryPointBody(mainFunction: FunctionDecl.Typed(d)!, in: source)
      } else {
        return nil
      }
    })
  }
  /// Returns the transpiled body of the entry point function calling `source`.
  private func entryPointBody(mainFunction source: FunctionDecl.Typed, in module: ModuleDecl.Typed)
    -> CXXScopedBlock
  {
    // The expression that makes a call to the module `main` function.
    let callToMain = CXXFunctionCallExpr(
      callee: CXXInfixExpr(
        oper: .scopeResolution,
        lhs: CXXIdentifier(module.baseName),
        rhs: CXXIdentifier("main")),
      arguments: [])

    // Foward function result if the result type is Int32.
    let resultType = (source.type.base as! LambdaType).output
    let forwardReturn = wholeValProgram.relations.areEquivalent(
      resultType, ^wholeValProgram.ast.coreType(named: "Int32")!)

    // Build the body of the CXX entry-point function.
    var bodyContent: [CXXStmt] = []
    if forwardReturn {
      // Forward the result of the function as the exit code.
      bodyContent = [
        CXXReturnStmt(
          expr: CXXInfixExpr(oper: .dotAccess, lhs: callToMain, rhs: CXXIdentifier("value")))
      ]
    } else {
      // Make a plain function call, discarding the result
      bodyContent = [CXXExprStmt(expr: CXXVoidCast(baseExpr: callToMain))]
    }
    return CXXScopedBlock(stmts: bodyContent)
  }

  // MARK: misc

  /// Returns the enclosing product type of `decl`.
  ///
  /// Fails if the source is nested by a `TypeScope` that is not a product type (trait or type alias).
  private func enclosingProductType<ID: DeclID>(_ decl: TypedNode<ID>) -> ProductTypeDecl.Typed {
    let parentScope = wholeValProgram.innermostType(containing: decl.id)!
    return wholeValProgram[ProductTypeDecl.ID(parentScope)!]
  }

}
