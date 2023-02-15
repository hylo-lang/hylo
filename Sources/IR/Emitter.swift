import BigInt
import Core
import FrontEnd
import Utils

/// Val's IR emitter.
///
/// The emitter transforms well-formed, typed ASTs to a representation suitable for flow-sensitive
/// analysis and code generation.
public struct Emitter {

  /// The program being lowered.
  public let program: TypedProgram

  /// The basic block in which new instructions are currently inserted.
  private var insertionBlock: Block.ID?

  /// A stack of frames describing the variables and allocations of each traversed lexical scope.
  private var frames = Stack()

  /// The receiver of the function or subscript currently being lowered, if any.
  private var receiver: ParameterDecl.Typed?

  /// The diagnostics of lowering errors.
  private var diagnostics: DiagnosticSet = []

  /// Creates an emitter with a well-typed AST.
  public init(program: TypedProgram) {
    self.program = program
  }

  /// Reports the given diagnostic.
  private mutating func report(_ d: Diagnostic) {
    diagnostics.insert(d)
  }

  // MARK: Declarations

  /// Inserts the IR for the top-level declaration `d` into `module`, reporting errors and warnings
  /// to `diagnostics`.
  ///
  /// - Requires: `d` is at module scope.
  mutating func emit(
    topLevel d: AnyDeclID.TypedNode,
    into module: inout Module,
    diagnostics: inout DiagnosticSet
  ) {
    precondition(d.scope.kind == TranslationUnit.self)

    swap(&self.diagnostics, &diagnostics)
    defer { swap(&self.diagnostics, &diagnostics) }

    switch d.kind {
    case FunctionDecl.self:
      emit(functionDecl: FunctionDecl.Typed(d)!, into: &module)
    case OperatorDecl.self:
      break
    case ProductTypeDecl.self:
      emit(productDecl: ProductTypeDecl.Typed(d)!, into: &module)
    case TraitDecl.self:
      break
    default:
      unexpected(d)
    }
  }

  /// Inserts the IR for `decl` into `module`.
  private mutating func emit(functionDecl decl: FunctionDecl.Typed, into module: inout Module) {
    // Declare the function in the module if necessary.
    let functionID = module.getOrCreateFunction(correspondingTo: decl, program: program)

    // Nothing else to do if the function has no body.
    guard let body = decl.body else { return }

    // Create the function entry.
    assert(module.functions[functionID].blocks.isEmpty)
    let entryID = module.createBasicBlock(
      accepting: module.functions[functionID].inputs.map(\.type),
      atEndOf: functionID)
    insertionBlock = entryID

    // Configure the locals.
    var locals = TypedDeclProperty<Operand>()

    let explicitCaptures = decl.explicitCaptures
    for (i, capture) in explicitCaptures.enumerated() {
      locals[capture] = .parameter(block: entryID, index: i)
    }

    for (i, capture) in decl.implicitCaptures!.enumerated() {
      locals[program[capture.decl]] = .parameter(block: entryID, index: i + explicitCaptures.count)
    }

    var implicitParameterCount = explicitCaptures.count + decl.implicitCaptures!.count
    if let receiver = decl.receiver {
      locals[receiver] = .parameter(block: entryID, index: implicitParameterCount)
      implicitParameterCount += 1
    }

    for (i, parameter) in decl.parameters.enumerated() {
      locals[parameter] = .parameter(block: entryID, index: i + implicitParameterCount)
    }

    // Emit the body.
    frames.push(Frame(locals: locals))
    var receiverDecl = decl.receiver
    swap(&receiverDecl, &self.receiver)

    switch body {
    case .block(let stmt):
      // Emit the statements of the function.
      emit(stmt: stmt, into: &module)

    case .expr(let expr):
      // Emit the body of the function.
      let value = emitRValue(expr, into: &module)

      // Emit stack deallocation.
      emitStackDeallocs(in: &module, site: expr.site)

      // Emit the implicit return statement.
      if expr.type != .never {
        module.append(module.makeReturn(value, anchoredAt: expr.site), to: insertionBlock!)
      }
    }

    swap(&receiverDecl, &self.receiver)
    frames.pop()
    assert(frames.isEmpty)
  }

  /// Inserts the IR for `decl` into `module`.
  private mutating func emit(subscriptDecl decl: SubscriptDecl.Typed, into module: inout Module) {
    fatalError("not implemented")
  }

  /// Inserts the IR for `decl` into `module`.
  private mutating func emit(productDecl decl: ProductTypeDecl.Typed, into module: inout Module) {
    for member in decl.members {
      // Emit the member functions and subscripts of the type declaration.
      switch member.kind {
      case FunctionDecl.self:
        emit(functionDecl: FunctionDecl.Typed(member)!, into: &module)

      case InitializerDecl.self:
        if InitializerDecl.Typed(member)!.introducer.value == .memberwiseInit { continue }
        fatalError("not implemented")

      case SubscriptDecl.self:
        emit(subscriptDecl: SubscriptDecl.Typed(member)!, into: &module)

      default:
        continue
      }
    }
  }

  /// Inserts the IR for the local binding `decl` into `module`.
  ///
  /// - Requires: `decl` is a local binding.
  private mutating func emit(localBindingDecl decl: BindingDecl.Typed, into module: inout Module) {
    switch decl.pattern.introducer.value {
    case .var, .sinklet:
      emit(storedLocalBindingDecl: decl, into: &module)
    case .let:
      emit(localBindingDecl: decl, borrowing: .let, into: &module)
    case .inout:
      emit(localBindingDecl: decl, borrowing: .inout, into: &module)
    }
  }

  /// Inserts the IR for the local binding `decl` into `module`.
  ///
  /// - Requires: `decl` is a local local `var` or `sink let` binding.
  private mutating func emit(
    storedLocalBindingDecl decl: BindingDecl.Typed,
    into module: inout Module
  ) {
    precondition(program.isLocal(decl.id))
    precondition(reading(decl.pattern.introducer.value, { ($0 == .var) || ($0 == .sinklet) }))

    /// A map from object path to its corresponding (sub-)object during destruction.
    var objects: [[Int]: Operand] = [:]

    // Emit the initializer, if any.
    if let initializer = decl.initializer {
      objects[[]] = emitRValue(initializer, into: &module)
    }

    // Allocate storage for each name introduced by the declaration.
    for (path, name) in decl.pattern.subpattern.names {
      let storage = module.append(
        module.makeAllocStack(name.decl.type, for: name.decl.id, anchoredAt: name.site),
        to: insertionBlock!)[0]
      frames.top.allocs.append(storage)
      frames[name.decl] = storage

      if let initializer = decl.initializer {
        // Determine the object corresponding to the current name.
        var rhsType = initializer.type
        for i in 0 ..< path.count {
          // Make sure the initializer has been destructured deeply enough.
          let subpath = Array(path[0 ..< i])
          if objects[subpath] != nil { continue }

          let layout = AbstractTypeLayout(of: rhsType, definedIn: program)
          rhsType = layout[i].type

          let wholePath = Array(path[0 ..< (i - 1)])
          let whole = objects[wholePath]!
          let parts = module.append(
            module.makeDestructure(whole, anchoredAt: initializer.site),
            to: insertionBlock!)

          for j in 0 ..< parts.count {
            objects[wholePath + [j]] = parts[j]
          }
        }

        // Borrow the storage for initialization corresponding to the current name.
        let target = module.append(
          module.makeBorrow(.set, from: storage, anchoredAt: name.site),
          to: insertionBlock!)[0]

        // Store the corresponding (part of) the initializer.
        module.append(
          module.makeStore(objects[path]!, at: target, anchoredAt: name.site),
          to: insertionBlock!)
      }
    }
  }

  /// Inserts the IR for the local binding `decl` into `module`.
  ///
  /// - Requires: `decl` is a local local `let`, `inout`, or `set` binding.
  private mutating func emit(
    localBindingDecl decl: BindingDecl.Typed,
    borrowing capability: AccessEffect,
    into module: inout Module
  ) {
    precondition(program.isLocal(decl.id))
    precondition(reading(decl.pattern.introducer.value, { ($0 != .var) && ($0 != .sinklet) }))

    /// The pattern of the binding being emitted.
    let pattern = decl.pattern

    // There's nothing to do if there's no initializer.
    if let initializer = decl.initializer {
      let source: Operand
      if (initializer.kind == NameExpr.self) || (initializer.kind == SubscriptCallExpr.self) {
        // Emit the initializer as a l-value.
        source = emitLValue(initializer, meantFor: capability, into: &module)
      } else {
        // emit a r-value and store it into local storage.
        let value = emitRValue(initializer, into: &module)

        let exprType = initializer.type
        let storage = module.append(
          module.makeAllocStack(exprType, anchoredAt: pattern.site),
          to: insertionBlock!)[0]
        frames.top.allocs.append(storage)
        source = storage

        let target = module.append(
          module.makeBorrow(.set, from: storage, anchoredAt: pattern.site),
          to: insertionBlock!)[0]
        module.append(
          module.makeStore(value, at: target, anchoredAt: pattern.site),
          to: insertionBlock!)
      }

      for (path, name) in pattern.subpattern.names {
        let s =
          module.append(
            module.makeElementAddr(source, at: path, anchoredAt: name.decl.site),
            to: insertionBlock!)[0]
        frames[name.decl] =
          module.append(
            module.makeBorrow(capability, from: s, anchoredAt: name.decl.site),
            to: insertionBlock!)[0]
      }
    }
  }

  // MARK: Statements

  /// Inserts the IR for `stmt` into `module`.
  private mutating func emit<ID: StmtID>(stmt: ID.TypedNode, into module: inout Module) {
    switch stmt.kind {
    case AssignStmt.self:
      emit(assignStmt: AssignStmt.Typed(stmt)!, into: &module)
    case BraceStmt.self:
      emit(braceStmt: BraceStmt.Typed(stmt)!, into: &module)
    case DeclStmt.self:
      emit(declStmt: DeclStmt.Typed(stmt)!, into: &module)
    case DoWhileStmt.self:
      emit(doWhileStmt: DoWhileStmt.Typed(stmt)!, into: &module)
    case ExprStmt.self:
      emit(exprStmt: ExprStmt.Typed(stmt)!, into: &module)
    case ReturnStmt.self:
      emit(returnStmt: ReturnStmt.Typed(stmt)!, into: &module)
    case WhileStmt.self:
      emit(whileStmt: WhileStmt.Typed(stmt)!, into: &module)
    default:
      unexpected(stmt)
    }
  }

  private mutating func emit(assignStmt stmt: AssignStmt.Typed, into module: inout Module) {
    guard stmt.left.kind != InoutExpr.self else {
      report(.error(assignmentLHSMustBeMarkedForMutationAt: .empty(at: stmt.left.site.first())))
      return
    }

    let rhs = emitRValue(stmt.right, into: &module)
    // FIXME: Should request the capability 'set or inout'.
    let lhs = emitLValue(stmt.left, meantFor: .set, into: &module)
    module.append(module.makeStore(rhs, at: lhs, anchoredAt: stmt.site), to: insertionBlock!)
  }

  private mutating func emit(braceStmt stmt: BraceStmt.Typed, into module: inout Module) {
    frames.push()
    for s in stmt.stmts {
      emit(stmt: s, into: &module)
    }
    emitStackDeallocs(in: &module, site: stmt.site)
    frames.pop()
  }

  private mutating func emit(declStmt stmt: DeclStmt.Typed, into module: inout Module) {
    switch stmt.decl.kind {
    case BindingDecl.self:
      emit(localBindingDecl: BindingDecl.Typed(stmt.decl)!, into: &module)
    default:
      unexpected(stmt.decl)
    }
  }

  private mutating func emit(doWhileStmt stmt: DoWhileStmt.Typed, into module: inout Module) {
    let loopBody = module.createBasicBlock(atEndOf: insertionBlock!.function)
    let loopTail = module.createBasicBlock(atEndOf: insertionBlock!.function)
    module.append(
      module.makeBranch(to: loopBody, anchoredAt: .empty(at: stmt.site.first())),
      to: insertionBlock!)
    insertionBlock = loopBody

    // Note: we're not using `emit(braceStmt:into:)` because we need to evaluate the loop
    // condition before exiting the scope.
    frames.push()
    for s in stmt.body.stmts {
      emit(stmt: s, into: &module)
    }

    let c = emitBranchCondition(stmt.condition, into: &module)
    emitStackDeallocs(in: &module, site: stmt.site)
    frames.pop()
    module.append(
      module.makeCondBranch(
        if: c, then: loopBody, else: loopTail, anchoredAt: stmt.condition.site),
      to: insertionBlock!)

    insertionBlock = loopTail
  }

  private mutating func emit(exprStmt stmt: ExprStmt.Typed, into module: inout Module) {
    _ = emitRValue(stmt.expr, into: &module)
  }

  private mutating func emit(returnStmt stmt: ReturnStmt.Typed, into module: inout Module) {
    let value: Operand
    if let expr = stmt.value {
      value = emitRValue(expr, into: &module)
    } else {
      value = .constant(.void)
    }

    emitStackDeallocs(in: &module, site: stmt.site)
    module.append(module.makeReturn(value, anchoredAt: stmt.site), to: insertionBlock!)
  }

  private mutating func emit(whileStmt stmt: WhileStmt.Typed, into module: inout Module) {
    let loopHead = module.createBasicBlock(atEndOf: insertionBlock!.function)
    let loopTail = module.createBasicBlock(atEndOf: insertionBlock!.function)

    // Emit the condition(s).
    module.append(
      module.makeBranch(to: loopHead, anchoredAt: .empty(at: stmt.site.first())),
      to: insertionBlock!)
    insertionBlock = loopHead

    for item in stmt.condition {
      let next = module.createBasicBlock(atEndOf: insertionBlock!.function)

      frames.push()
      defer { frames.pop() }

      switch item {
      case .expr(let itemExpr):
        let e = program[itemExpr]
        let c = emitBranchCondition(e, into: &module)
        emitStackDeallocs(in: &module, site: e.site)
        module.append(
          module.makeCondBranch(if: c, then: next, else: loopTail, anchoredAt: e.site),
          to: insertionBlock!)
        insertionBlock = next

      case .decl:
        fatalError("not implemented")
      }
    }

    emit(braceStmt: stmt.body, into: &module)
    module.append(
      module.makeBranch(to: loopHead, anchoredAt: .empty(at: stmt.site.first())),
      to: insertionBlock!)
    insertionBlock = loopTail
  }

  // MARK: r-values

  /// Inserts the IR for the rvalue `expr` into `module` at the end of the current insertion block.
  private mutating func emitRValue<ID: ExprID>(
    _ expr: ID.TypedNode,
    into module: inout Module
  ) -> Operand {
    defer {
      // Mark the execution path unreachable if the computed value has type `Never`.
      if program.relations.areEquivalent(expr.type, .never) {
        emitStackDeallocs(in: &module, site: expr.site)
        module.append(module.makeUnreachable(anchoredAt: expr.site), to: insertionBlock!)
      }
    }

    switch expr.kind {
    case BooleanLiteralExpr.self:
      return emitRValue(booleanLiteral: BooleanLiteralExpr.Typed(expr)!, into: &module)
    case CondExpr.self:
      return emitRValue(conditional: CondExpr.Typed(expr)!, into: &module)
    case FloatLiteralExpr.self:
      return emitRValue(floatLiteral: FloatLiteralExpr.Typed(expr)!, into: &module)
    case FunctionCallExpr.self:
      return emitRValue(functionCall: FunctionCallExpr.Typed(expr)!, into: &module)
    case IntegerLiteralExpr.self:
      return emitRValue(integerLiteral: IntegerLiteralExpr.Typed(expr)!, into: &module)
    case NameExpr.self:
      return emitRValue(name: NameExpr.Typed(expr)!, into: &module)
    case SequenceExpr.self:
      return emitRValue(sequence: SequenceExpr.Typed(expr)!, into: &module)
    case TupleExpr.self:
      return emitRValue(tuple: TupleExpr.Typed(expr)!, into: &module)
    default:
      unexpected(expr)
    }
  }

  private mutating func emitRValue(
    booleanLiteral expr: BooleanLiteralExpr.Typed,
    into module: inout Module
  ) -> Operand {
    let value = Operand.constant(
      .integer(IntegerConstant(expr.value ? 1 : 0, bitWidth: 1)))

    let boolType = program.ast.coreType(named: "Bool")!
    return module.append(
      module.makeRecord(boolType, aggregating: [value], anchoredAt: expr.site),
      to: insertionBlock!)[0]
  }

  private mutating func emitRValue(
    conditional expr: CondExpr.Typed,
    into module: inout Module
  ) -> Operand {
    let functionID = insertionBlock!.function

    // If the expression is supposed to return a value, allocate storage for it.
    var resultStorage: Operand?
    if expr.type != .void {
      resultStorage =
        module.append(
          module.makeAllocStack(expr.type, anchoredAt: expr.site),
          to: insertionBlock!)[0]
      frames.top.allocs.append(resultStorage!)
    }

    // Emit the condition(s).
    var alt: Block.ID?

    for item in expr.condition {
      let success = module.createBasicBlock(atEndOf: functionID)
      let failure = module.createBasicBlock(atEndOf: functionID)
      alt = failure

      switch item {
      case .expr(let itemExpr):
        // Evaluate the condition in the current block.
        let c = emitBranchCondition(program[itemExpr], into: &module)
        module.append(
          module.makeCondBranch(if: c, then: success, else: failure, anchoredAt: expr.site),
          to: insertionBlock!)
        insertionBlock = success

      case .decl:
        fatalError("not implemented")
      }
    }

    let continuation = module.createBasicBlock(atEndOf: functionID)

    // Emit the success branch.
    // Note: the insertion pointer is already set in the corresponding block.
    switch expr.success {
    case .expr(let thenExpr):
      frames.push()
      let value = emitRValue(program[thenExpr], into: &module)
      if let target = resultStorage {
        let target = module.append(
          module.makeBorrow(.set, from: target, anchoredAt: program[thenExpr].site),
          to: insertionBlock!)[0]
        module.append(
          module.makeStore(value, at: target, anchoredAt: program[thenExpr].site),
          to: insertionBlock!)
      }
      emitStackDeallocs(in: &module, site: expr.site)
      frames.pop()

    case .block:
      fatalError("not implemented")
    }
    module.append(module.makeBranch(to: continuation, anchoredAt: expr.site), to: insertionBlock!)

    // Emit the failure branch.
    insertionBlock = alt
    switch expr.failure {
    case .expr(let elseExpr):
      frames.push()
      let value = emitRValue(program[elseExpr], into: &module)
      if let target = resultStorage {
        let target = module.append(
          module.makeBorrow(.set, from: target, anchoredAt: program[elseExpr].site),
          to: insertionBlock!)[0]
        module.append(
          module.makeStore(value, at: target, anchoredAt: program[elseExpr].site),
          to: insertionBlock!)
      }
      emitStackDeallocs(in: &module, site: expr.site)
      frames.pop()

    case .block:
      fatalError("not implemented")

    case nil:
      break
    }
    module.append(module.makeBranch(to: continuation, anchoredAt: expr.site), to: insertionBlock!)

    // Emit the value of the expression.
    insertionBlock = continuation
    if let s = resultStorage {
      return module.append(module.makeLoad(s, anchoredAt: expr.site), to: insertionBlock!)[0]
    } else {
      return .constant(.void)
    }
  }

  private mutating func emitRValue(
    floatLiteral expr: FloatLiteralExpr.Typed,
    into module: inout Module
  ) -> Operand {
    emitNumericLiteral(
      expr.value, withType: program.relations.canonical(expr.type),
      anchoredAt: expr.site,
      into: &module)
  }

  private mutating func emitRValue(
    functionCall expr: FunctionCallExpr.Typed,
    into module: inout Module
  ) -> Operand {
    if case .builtinFunction(let f) = NameExpr.Typed(expr.callee)?.decl {
      return emit(builtinFunctionCallTo: f, with: expr.arguments, at: expr.site, into: &module)
    }

    // Callee must have a lambda type.
    let calleeType = LambdaType(expr.callee.type)!

    // Arguments are evaluated first, from left to right.
    let arguments: [Operand] = zip(calleeType.inputs, expr.arguments).map { (p, a) in
      emit(argument: program[a.value], to: ParameterType(p.type)!, into: &module)
    }
    let (callee, liftedArguments) = emitCallee(expr.callee, into: &module)

    return module.append(
      module.makeCall(applying: callee, to: liftedArguments + arguments, anchoredAt: expr.site),
      to: insertionBlock!)[0]
  }

  /// Emits the IR of a call to `f` with given `arguments` at `site` into `module`, inserting
  /// instructions at the end of `self.insertionBlock`.
  private mutating func emit(
    builtinFunctionCallTo f: BuiltinFunction,
    with arguments: [LabeledArgument],
    at site: SourceRange,
    into module: inout Module
  ) -> Operand {
    return module.append(
      module.makeLLVM(
        applying: f,
        to: arguments.map({ (a) in emitRValue(program[a.value], into: &module) }),
        anchoredAt: site),
      to: insertionBlock!)[0]
  }

  private mutating func emitRValue(
    integerLiteral expr: IntegerLiteralExpr.Typed,
    into module: inout Module
  ) -> Operand {
    emitNumericLiteral(
      expr.value, withType: program.relations.canonical(expr.type),
      anchoredAt: expr.site,
      into: &module)
  }

  private mutating func emitRValue(
    name expr: NameExpr.Typed,
    into module: inout Module
  ) -> Operand {
    switch expr.decl {
    case .direct(let declID):
      // Lookup for a local symbol.
      if let s = frames[declID] {
        return module.append(module.makeLoad(s, anchoredAt: expr.site), to: insertionBlock!)[0]
      }

      fatalError("not implemented")

    case .member:
      fatalError("not implemented")

    case .builtinFunction:
      fatalError("not implemented")

    case .builtinType:
      fatalError("not implemented")
    }
  }

  private mutating func emitRValue(
    sequence expr: SequenceExpr.Typed,
    into module: inout Module
  ) -> Operand {
    emit(.sink, foldedSequenceExpr: expr.foldedSequenceExprs!, into: &module)
  }

  private mutating func emit(
    _ convention: AccessEffect,
    foldedSequenceExpr expr: FoldedSequenceExpr,
    into module: inout Module
  ) -> Operand {
    switch expr {
    case .infix(let callee, let lhs, let rhs):
      let calleeType = LambdaType(program.relations.canonical(program.exprTypes[callee.expr]!))!
        .lifted

      // Emit the operands, starting with RHS.
      let r = emit(
        ParameterType(calleeType.inputs[1].type)!.access, foldedSequenceExpr: rhs, into: &module)
      let l = emit(
        ParameterType(calleeType.inputs[0].type)!.access, foldedSequenceExpr: lhs, into: &module)

      // Emit the callee.
      guard case .member(let calleeDecl) = program.referredDecls[callee.expr] else {
        unreachable()
      }
      assert(calleeDecl.kind == FunctionDecl.self)
      let c = Operand.constant(
        .function(
          FunctionRef(
            name: DeclLocator(identifying: calleeDecl, in: program).mangled,
            type: .address(calleeType))))

      // Emit the call.
      return module.append(
        module.makeCall(applying: c, to: [r, l], anchoredAt: program.ast.site(of: expr)),
        to: insertionBlock!)[0]

    case .leaf(let expr):
      return (convention == .sink)
        ? emitRValue(program[expr], into: &module)
        : emitLValue(program[expr], meantFor: convention, into: &module)
    }
  }

  private mutating func emitRValue(
    tuple syntax: TupleExpr.Typed,
    into module: inout Module
  ) -> Operand {
    if syntax.elements.isEmpty { return .constant(.void) }

    var elements: [Operand] = []
    for e in syntax.elements {
      elements.append(emitRValue(program[e.value], into: &module))
    }
    return module.append(
      module.makeRecord(syntax.type, aggregating: elements, anchoredAt: syntax.site),
      to: insertionBlock!)[0]
  }

  /// Inserts the IR for the argument `expr` passed to a parameter of type `parameterType` into
  /// `module` at the end of the current insertion block.
  private mutating func emit(
    argument expr: AnyExprID.TypedNode,
    to parameterType: ParameterType,
    into module: inout Module
  ) -> Operand {
    switch parameterType.access {
    case .let, .inout, .set:
      let s = emitLValue(expr, into: &module)
      return module.append(
        module.makeBorrow(parameterType.access, from: s, anchoredAt: expr.site),
        to: insertionBlock!)[0]

    case .sink:
      return emitRValue(expr, into: &module)

    default:
      fatalError("not implemented")
    }
  }

  /// Inserts the IR for given `callee` into `module` at the end of the current insertion block and
  /// returns `(c, a)`, where `c` is the callee's value and `a` are arguments to lifted parameters.
  ///
  /// Lifted arguments are produced if `callee` is a reference to a function with captures, such as
  /// a bound member function or a local function declaration with a non-empty environment.
  ///
  /// - Requires: `callee` has a lambda type.
  private mutating func emitCallee(
    _ callee: AnyExprID.TypedNode,
    into module: inout Module
  ) -> (callee: Operand, liftedArguments: [Operand]) {
    if let e = NameExpr.Typed(callee) {
      return emitNamedCallee(e, into: &module)
    } else {
      return (emitRValue(callee, into: &module), [])
    }
  }

  /// Inserts the IR for given `callee` into `module` at the end of the current insertion block and
  /// returns `(c, a)`, where `c` is the callee's value and `a` are arguments to lifted parameters.
  ///
  /// - Requires: `callee` has a lambda type.
  private mutating func emitNamedCallee(
    _ callee: NameExpr.Typed,
    into module: inout Module
  ) -> (callee: Operand, liftedArguments: [Operand]) {
    let calleeType = LambdaType(program.relations.canonical(callee.type))!

    switch callee.decl {
    case .direct(let d) where d.kind == FunctionDecl.self:
      // Callee is a direct reference to a function declaration.
      guard calleeType.environment == .void else {
        fatalError("not implemented")
      }

      let c = Operand.constant(
        .function(
          FunctionRef(
            name: DeclLocator(identifying: d.id, in: program).mangled,
            type: .address(calleeType))))
      return (c, [])

    case .direct(let d) where d.kind == InitializerDecl.self:
      // Callee is a direct reference to an initializer declaration.
      fatalError("not implemented")

    case .member(let d) where d.kind == FunctionDecl.self:
      // Callee is a member reference to a function or method.
      let c = Operand.constant(
        .function(
          FunctionRef(
            name: DeclLocator(identifying: d.id, in: program).mangled,
            type: .address(calleeType.lifted))))

      // Emit the location of the receiver.
      let receiver: Operand
      switch callee.domain {
      case .none:
        receiver = frames[self.receiver!]!
      case .expr(let e):
        receiver = emitLValue(e, into: &module)
      case .implicit:
        unreachable()
      }

      // Load or borrow the receiver.
      if let t = RemoteType(calleeType.captures[0].type) {
        let i = module.append(
          module.makeBorrow(t.access, from: receiver, anchoredAt: callee.site),
          to: insertionBlock!)
        return (c, i)
      } else {
        let i = module.append(
          module.makeLoad(receiver, anchoredAt: callee.site),
          to: insertionBlock!)
        return (c, i)
      }

    case .builtinFunction, .builtinType:
      // Calls to built-ins should have been handled already.
      unreachable()

    default:
      // Callee is a lambda.
      return (emitRValue(callee, into: &module), [])
    }
  }

  /// Inserts the IR for branch condition `expr` into `module` at the end of the current insertion
  /// block.
  ///
  /// - Requires: `expr.type` is `Val.Bool`
  private mutating func emitBranchCondition(
    _ expr: AnyExprID.TypedNode,
    into module: inout Module
  ) -> Operand {
    precondition(program.relations.canonical(expr.type) == program.ast.coreType(named: "Bool")!)
    let b = emitRValue(expr, into: &module)
    return module.append(
      module.makeDestructure(b, anchoredAt: expr.site),
      to: insertionBlock!)[0]
  }

  /// Inserts the IR for numeric literal `s` with type `literalType` into `module` at the end of
  /// the current insertion, anchoring instructions at `anchor`.
  ///
  /// - Requires `literalType` must be one of the core numeric types.
  private mutating func emitNumericLiteral(
    _ s: String,
    withType literalType: AnyType,
    anchoredAt anchor: SourceRange,
    into module: inout Module
  ) -> Operand {
    switch literalType {
    case program.ast.coreType(named: "Double")!:
      let v = Constant.floatingPoint(.double(s))
      return module.append(
        module.makeRecord(literalType, aggregating: [.constant(v)], anchoredAt: anchor),
        to: insertionBlock!)[0]

    case program.ast.coreType(named: "Float")!:
      let v = Constant.floatingPoint(.float(s))
      return module.append(
        module.makeRecord(literalType, aggregating: [.constant(v)], anchoredAt: anchor),
        to: insertionBlock!)[0]

    default:
      return emitIntegerLiteral(s, withType: literalType, anchoredAt: anchor, into: &module)
    }
  }

  /// Inserts the IR for numeric literal `s` with type `literalType` into `module` at the end of
  /// the current insertion, anchoring instructions at `anchor`.
  ///
  /// - Requires `literalType` must be one of the core integer types.
  private mutating func emitIntegerLiteral(
    _ s: String,
    withType literalType: AnyType,
    anchoredAt anchor: SourceRange,
    into module: inout Module
  ) -> Operand {
    let bits: WideUInt?
    switch literalType {
    case program.ast.coreType(named: "Int")!:
      bits = .init(valLiteral: s, signed: true, bitWidth: 64)
    case program.ast.coreType(named: "Int32")!:
      bits = .init(valLiteral: s, signed: true, bitWidth: 32)
    case program.ast.coreType(named: "Int8")!:
      bits = .init(valLiteral: s, signed: true, bitWidth: 8)
    default:
      unreachable("unexpected numeric type")
    }

    guard let b = bits else {
      diagnostics.insert(
        .error(integerLiterl: s, overflowsWhenStoredInto: literalType, at: anchor))
      return .constant(.poison(PoisonConstant(type: .object(literalType))))
    }

    return module.append(
      module.makeRecord(
        literalType, aggregating: [.constant(.integer(IntegerConstant(b)))], anchoredAt: anchor),
      to: insertionBlock!)[0]
  }

  // MARK: l-values

  /// Inserts the IR for the lvalue `expr` meant for `capability` into `module` at the end of the
  /// current insertion block.
  private mutating func emitLValue(
    _ syntax: AnyExprID.TypedNode,
    meantFor capability: AccessEffect,
    into module: inout Module
  ) -> Operand {
    let s = emitLValue(syntax, into: &module)
    return module.append(
      module.makeBorrow(capability, from: s, anchoredAt: syntax.site),
      to: insertionBlock!)[0]
  }

  /// Inserts the IR for the lvalue `syntax` into `module` at the end of the current insertion
  /// block.
  private mutating func emitLValue(
    _ syntax: AnyExprID.TypedNode,
    into module: inout Module
  ) -> Operand {
    switch syntax.kind {
    case InoutExpr.self:
      return emitLValue(inoutExpr: InoutExpr.Typed(syntax)!, into: &module)
    case NameExpr.self:
      return emitLValue(name: NameExpr.Typed(syntax)!, into: &module)
    default:
      return emitLValue(convertingRValue: syntax, into: &module)
    }
  }

  /// Inserts the IR for the rvalue `syntax` converted as a lvalue into `module` at the end of the
  /// current insertion block.
  private mutating func emitLValue(
    convertingRValue syntax: AnyExprID.TypedNode,
    into module: inout Module
  ) -> Operand {
    let rvalueType = program.relations.canonical(syntax.type)

    let value = emitRValue(syntax, into: &module)
    let storage = module.append(
      module.makeAllocStack(rvalueType, anchoredAt: syntax.site),
      to: insertionBlock!)[0]
    frames.top.allocs.append(storage)

    let target = module.append(
      module.makeBorrow(.set, from: storage, anchoredAt: syntax.site),
      to: insertionBlock!)[0]
    module.append(
      module.makeStore(value, at: target, anchoredAt: syntax.site),
      to: insertionBlock!)

    return storage
  }

  private mutating func emitLValue(
    inoutExpr syntax: InoutExpr.Typed,
    into module: inout Module
  ) -> Operand {
    return emitLValue(syntax.subject, into: &module)
  }

  private mutating func emitLValue(
    name syntax: NameExpr.Typed,
    into module: inout Module
  ) -> Operand {
    switch syntax.decl {
    case .direct(let d):
      if let s = frames[d] {
        return s
      } else {
        fatalError("not implemented")
      }

    case .member(let d):
      // Emit the receiver.
      let receiverAddress: Operand
      switch syntax.domain {
      case .none:
        receiverAddress = frames[receiver!]!
      case .implicit:
        fatalError("not implemented")
      case .expr(let e):
        receiverAddress = emitLValue(e, into: &module)
      }

      return addressOfMember(
        boundTo: receiverAddress, declaredBy: d, into: &module, at: syntax.site)

    case .builtinFunction, .builtinType:
      // Built-in functions and types are never used as l-value.
      unreachable()
    }

    fatalError()
  }

  /// Returns the address of the member declared by `decl` and bound to `receiverAddress`,
  /// inserting IR anchored at `anchor` into `module`.
  private mutating func addressOfMember(
    boundTo receiverAddress: Operand,
    declaredBy decl: AnyDeclID.TypedNode,
    into module: inout Module,
    at anchor: SourceRange
  ) -> Operand {
    switch decl.kind {
    case VarDecl.self:
      let receiverLayout = AbstractTypeLayout(
        of: module.type(of: receiverAddress).astType, definedIn: program)

      let i = receiverLayout.offset(of: VarDecl.Typed(decl)!.baseName)!
      return module.append(
        module.makeElementAddr(receiverAddress, at: [i], anchoredAt: anchor),
        to: insertionBlock!)[0]

    default:
      fatalError("not implemented")
    }
  }

  // MARK: Helpers

  /// Emits a deallocation instruction for each allocation in the top frame of `self.frames`.
  private mutating func emitStackDeallocs(in module: inout Module, site: SourceRange) {
    while let a = frames.top.allocs.popLast() {
      module.append(
        module.makeDeallocStack(for: a, anchoredAt: site),
        to: insertionBlock!)
    }
  }

}

extension Emitter {

  /// The local variables and allocations of a lexical scope.
  fileprivate struct Frame {

    /// A map from declaration of a local variable to its corresponding IR in the frame.
    var locals = TypedDeclProperty<Operand>()

    /// The allocations in the frame, in FILO order.
    var allocs: [Operand] = []

  }

  /// A stack of frames.
  fileprivate struct Stack {

    /// The frames in the stack, ordered from bottom to top.
    private var frames: [Frame] = []

    /// True iff the stack is empty.
    var isEmpty: Bool { frames.isEmpty }

    /// Accesses the top frame.
    ///
    /// - Requires: The stack is not empty.
    var top: Frame {
      get { frames[frames.count - 1] }
      _modify { yield &frames[frames.count - 1] }
    }

    /// Accesses the IR corresponding to `d`.
    ///
    /// - Requires: The stack is not empty.
    /// - Complexity: O(*n*) for read access where *n* is the number of frames in the stack. O(1)
    ///   for write access.
    subscript<ID: DeclID>(d: ID.TypedNode) -> Operand? {
      get {
        for frame in frames.reversed() {
          if let operand = frame.locals[d] { return operand }
        }
        return nil
      }
      set { top.locals[d] = newValue }
    }

    /// Pushes `newFrame` on the stack.
    mutating func push(_ newFrame: Frame = Frame()) {
      frames.append(newFrame)
    }

    /// Pops the top frame.
    ///
    /// - Requires: The stack is not empty.
    @discardableResult
    mutating func pop() -> Frame {
      precondition(top.allocs.isEmpty, "stack leak")
      return frames.removeLast()
    }

  }

}

extension Diagnostic {

  static func error(assignmentLHSMustBeMarkedForMutationAt site: SourceRange) -> Diagnostic {
    .error("left-hand side of assignment must be marked for mutation", at: site)
  }

  static func error(
    integerLiterl s: String, overflowsWhenStoredInto t: AnyType,
    at site: SourceRange
  ) -> Diagnostic {
    .error("integer literal '\(s)' overflows when stored into '\(t)'", at: site)
  }

}
