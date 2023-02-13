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
        module.append(
          ReturnInstruction(value: value, site: expr.site),
          to: insertionBlock!)
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
        AllocStackInstruction(name.decl.type, site: name.site),
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
            DestructureInstruction(
              whole, as: layout.properties.map({ .object($0.type) }),
              site: initializer.site),
            to: insertionBlock!)

          for j in 0 ..< parts.count {
            objects[wholePath + [j]] = parts[j]
          }
        }

        // Borrow the storage for initialization corresponding to the current name.
        let target = module.append(
          BorrowInstruction(.set, .address(name.decl.type), from: storage, site: name.site),
          to: insertionBlock!)[0]

        // Store the corresponding (part of) the initializer.
        module.append(
          StoreInstruction(objects[path]!, to: target, site: name.site),
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
          AllocStackInstruction(exprType, site: pattern.site),
          to: insertionBlock!)[0]
        frames.top.allocs.append(storage)
        source = storage

        let target = module.append(
          BorrowInstruction(.set, .address(exprType), from: storage, site: pattern.site),
          to: insertionBlock!)[0]
        module.append(
          StoreInstruction(value, to: target, site: pattern.site),
          to: insertionBlock!)
      }

      for (path, name) in pattern.subpattern.names {
        frames[name.decl] =
          module.append(
            BorrowInstruction(
              capability, .address(name.decl.type), from: source, at: path, binding: name.decl,
              site: name.decl.site),
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
    _ = module.append(StoreInstruction(rhs, to: lhs, site: stmt.site), to: insertionBlock!)
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
      BranchInstruction(target: loopBody, site: .empty(at: stmt.site.first())),
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
      CondBranchInstruction(
        condition: c, targetIfTrue: loopBody, targetIfFalse: loopTail,
        site: stmt.condition.site),
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
    module.append(ReturnInstruction(value: value, site: stmt.site), to: insertionBlock!)
  }

  private mutating func emit(whileStmt stmt: WhileStmt.Typed, into module: inout Module) {
    let loopHead = module.createBasicBlock(atEndOf: insertionBlock!.function)
    let loopTail = module.createBasicBlock(atEndOf: insertionBlock!.function)

    // Emit the condition(s).
    module.append(
      BranchInstruction(target: loopHead, site: .empty(at: stmt.site.first())),
      to: insertionBlock!)
    insertionBlock = loopHead

    for item in stmt.condition {
      let b = module.createBasicBlock(atEndOf: insertionBlock!.function)

      frames.push()
      defer { frames.pop() }

      switch item {
      case .expr(let itemExpr):
        let e = program[itemExpr]
        let c = emitBranchCondition(e, into: &module)
        emitStackDeallocs(in: &module, site: e.site)
        module.append(
          CondBranchInstruction(
            condition: c, targetIfTrue: b, targetIfFalse: loopTail,
            site: e.site),
          to: insertionBlock!)
        insertionBlock = b

      case .decl:
        fatalError("not implemented")
      }
    }

    emit(braceStmt: stmt.body, into: &module)
    module.append(
      BranchInstruction(target: loopHead, site: .empty(at: stmt.site.first())),
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
        module.append(UnrechableInstruction(site: expr.site), to: insertionBlock!)
      }
    }

    switch expr.kind {
    case BooleanLiteralExpr.self:
      return emitRValue(booleanLiteral: BooleanLiteralExpr.Typed(expr)!, into: &module)
    case CondExpr.self:
      return emitRValue(conditional: CondExpr.Typed(expr)!, into: &module)
    case FunctionCallExpr.self:
      return emitRValue(functionCall: FunctionCallExpr.Typed(expr)!, into: &module)
    case IntegerLiteralExpr.self:
      return emitRValue(integerLiteral: IntegerLiteralExpr.Typed(expr)!, into: &module)
    case NameExpr.self:
      return emitRValue(name: NameExpr.Typed(expr)!, into: &module)
    case SequenceExpr.self:
      return emitRValue(sequence: SequenceExpr.Typed(expr)!, into: &module)
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
      RecordInstruction(objectType: .object(boolType), operands: [value], site: expr.site),
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
          AllocStackInstruction(expr.type, site: expr.site),
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
          CondBranchInstruction(
            condition: c,
            targetIfTrue: success,
            targetIfFalse: failure,
            site: expr.site),
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
          BorrowInstruction(
            .set, .address(expr.type), from: target,
            site: program[thenExpr].site),
          to: insertionBlock!)[0]
        module.append(
          StoreInstruction(value, to: target, site: program[thenExpr].site),
          to: insertionBlock!)
      }
      emitStackDeallocs(in: &module, site: expr.site)
      frames.pop()

    case .block:
      fatalError("not implemented")
    }
    module.append(BranchInstruction(target: continuation, site: expr.site), to: insertionBlock!)

    // Emit the failure branch.
    insertionBlock = alt
    switch expr.failure {
    case .expr(let elseExpr):
      frames.push()
      let value = emitRValue(program[elseExpr], into: &module)
      if let target = resultStorage {
        let target = module.append(
          BorrowInstruction(
            .set, .address(expr.type), from: target,
            site: program[elseExpr].site),
          to: insertionBlock!)[0]
        module.append(
          StoreInstruction(value, to: target, site: program[elseExpr].site),
          to: insertionBlock!)
      }
      emitStackDeallocs(in: &module, site: expr.site)
      frames.pop()

    case .block:
      fatalError("not implemented")

    case nil:
      break
    }
    module.append(BranchInstruction(target: continuation, site: expr.site), to: insertionBlock!)

    // Emit the value of the expression.
    insertionBlock = continuation
    if let source = resultStorage {
      return module.append(
        LoadInstruction(LoweredType(lowering: expr.type), from: source, site: expr.site),
        to: insertionBlock!)[0]
    } else {
      return .constant(.void)
    }
  }

  private mutating func emitRValue(
    functionCall expr: FunctionCallExpr.Typed,
    into module: inout Module
  ) -> Operand {
    if let n = NameExpr.Typed(expr.callee),
      case .builtinFunction(let f) = n.decl
    {
      return emit(builtinFunctionCallTo: f, with: expr.arguments, at: expr.site, into: &module)
    }

    // Determine the callee's convention.
    let calleeType = LambdaType(expr.callee.type)!

    // Arguments are evaluated first, from left to right.
    var argumentConventions: [AccessEffect] = []
    var arguments: [Operand] = []

    for (parameter, argument) in zip(calleeType.inputs, expr.arguments) {
      let parameterType = parameter.type.base as! ParameterType
      argumentConventions.append(parameterType.access)
      arguments.append(emit(argument: program[argument.value], to: parameterType, into: &module))
    }

    // If the callee is a name expression referring to the declaration of a function capture-less
    // function, it is interpreted as a direct function reference. Otherwise, it is evaluated as a
    // function object the arguments.
    let callee: Operand

    if let calleeNameExpr = NameExpr.Typed(expr.callee) {
      switch calleeNameExpr.decl {
      case .direct(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
        // Callee is a direct reference to a function or initializer declaration.
        // TODO: handle captures
        callee = .constant(
          .function(
            FunctionRef(
              name: DeclLocator(identifying: calleeDecl.id, in: program).mangled,
              type: .address(calleeType))))

      case .direct(let calleeDecl) where calleeDecl.kind == InitializerDecl.self:
        switch InitializerDecl.Typed(calleeDecl)!.introducer.value {
        case .`init`:
          // TODO: The function is a custom initializer.
          fatalError("not implemented")

        case .memberwiseInit:
          // The function is a memberwise initializer. In that case, the whole call expression is
          // lowered as a `record` instruction.
          return module.append(
            RecordInstruction(
              objectType: .object(expr.type), operands: arguments,
              site: expr.site),
            to: insertionBlock!)[0]
        }

      case .member(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
        // Callee is a member reference to a function or method.
        let receiverType = calleeType.captures[0].type

        // Add the receiver to the arguments.
        if let type = RemoteType(receiverType) {
          // The receiver as a borrowing convention.
          argumentConventions.insert(type.access, at: 0)

          switch calleeNameExpr.domain {
          case .none:
            let receiver = module.append(
              BorrowInstruction(
                type.access, .address(type.bareType), from: frames[receiver!]!,
                site: expr.site),
              to: insertionBlock!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            let receiver = emitLValue(receiverID, meantFor: type.access, into: &module)
            arguments.insert(receiver, at: 0)

          case .implicit:
            unreachable()
          }
        } else {
          // The receiver is consumed.
          argumentConventions.insert(.sink, at: 0)

          switch calleeNameExpr.domain {
          case .none:
            let receiver = module.append(
              LoadInstruction(
                .object(receiverType), from: frames[receiver!]!, site: expr.site),
              to: insertionBlock!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            arguments.insert(emitRValue(receiverID, into: &module), at: 0)

          case .implicit:
            unreachable()
          }
        }

        // Emit the function reference.
        callee = .constant(
          .function(
            FunctionRef(
              name: DeclLocator(identifying: calleeDecl.id, in: program).mangled,
              type: .address(calleeType))))

      case .builtinFunction:
        // Already handled.
        unreachable()

      case .builtinType:
        // Built-in types are never called.
        unreachable()

      default:
        // Evaluate the callee as a function object.
        callee = emitRValue(expr.callee, into: &module)
      }
    } else {
      // Evaluate the callee as a function object.
      callee = emitRValue(expr.callee, into: &module)
    }

    return module.append(
      CallInstruction(
        returnType: .object(expr.type),
        calleeConvention: calleeType.receiverEffect,
        callee: callee,
        argumentConventions: argumentConventions,
        arguments: arguments,
        site: expr.site),
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
      LLVMInstruction(
        applying: f,
        to: arguments.map({ (a) in emitRValue(program[a.value], into: &module) }),
        at: site),
      to: insertionBlock!)[0]
  }

  private mutating func emitRValue(
    integerLiteral expr: IntegerLiteralExpr.Typed,
    into module: inout Module
  ) -> Operand {
    let type = expr.type.base as! ProductType

    // Determine the bit width of the value.
    let bitWidth: Int
    switch type.name.value {
    case "Int": bitWidth = 64
    case "Int32": bitWidth = 32
    default:
      unreachable("unexpected numeric type")
    }

    // Convert the literal into a bit pattern.
    let bits: BigUInt
    let s = expr.value
    if s.starts(with: "0x") {
      bits = BigUInt(s.dropFirst(2), radix: 16)!
    } else {
      bits = BigUInt(s.dropFirst(2))!
    }

    // Emit the constant integer.
    let value = IntegerConstant(bits, bitWidth: bitWidth)
    return module.append(
      RecordInstruction(
        objectType: .object(type), operands: [.constant(.integer(value))], site: expr.site),
      to: insertionBlock!)[0]
  }

  private mutating func emitRValue(
    name expr: NameExpr.Typed,
    into module: inout Module
  ) -> Operand {
    switch expr.decl {
    case .direct(let declID):
      // Lookup for a local symbol.
      if let source = frames[declID] {
        return module.append(
          LoadInstruction(.object(expr.type), from: source, site: expr.site),
          to: insertionBlock!)[0]
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
      let calleeType = program.exprTypes[callee.expr]!.base as! LambdaType

      // Emit the operands, starting with RHS.
      let rhsType = calleeType.inputs[0].type.base as! ParameterType
      let rhsOperand = emit(rhsType.access, foldedSequenceExpr: rhs, into: &module)

      let lhsConvention: AccessEffect
      let lhsOperand: Operand
      if let lhsType = RemoteType(calleeType.captures[0].type) {
        lhsConvention = lhsType.access
        lhsOperand = emit(lhsConvention, foldedSequenceExpr: lhs, into: &module)
      } else {
        lhsConvention = .sink
        lhsOperand = emit(.sink, foldedSequenceExpr: lhs, into: &module)
      }

      // Create the callee's value.
      let calleeOperand: Operand
      switch program.referredDecls[callee.expr] {
      case .member(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
        calleeOperand = .constant(
          .function(
            FunctionRef(
              name: DeclLocator(identifying: calleeDecl, in: program).mangled,
              type: .address(calleeType))))

      default:
        unreachable()
      }

      // Emit the call.
      return module.append(
        CallInstruction(
          returnType: .object(calleeType.output),
          calleeConvention: .let,
          callee: calleeOperand,
          argumentConventions: [lhsConvention, rhsType.access],
          arguments: [lhsOperand, rhsOperand],
          site: program.ast.site(of: expr)),
        to: insertionBlock!)[0]

    case .leaf(let expr):
      switch convention {
      case .let:
        return emitLValue(program[expr], meantFor: .let, into: &module)
      case .inout:
        return emitLValue(program[expr], meantFor: .inout, into: &module)
      case .set:
        return emitLValue(program[expr], meantFor: .set, into: &module)
      case .sink:
        return emitRValue(program[expr], into: &module)
      case .yielded:
        fatalError("not implemented")
      }
    }
  }

  private mutating func emit(
    argument expr: AnyExprID.TypedNode,
    to parameterType: ParameterType,
    into module: inout Module
  ) -> Operand {
    switch parameterType.access {
    case .let:
      return emitLValue(expr, meantFor: .let, into: &module)
    case .inout:
      return emitLValue(expr, meantFor: .inout, into: &module)
    case .set:
      return emitLValue(expr, meantFor: .set, into: &module)
    case .sink:
      return emitRValue(expr, into: &module)
    case .yielded:
      fatalError("not implemented")
    }
  }

  /// Inserts the IR for the callee `expr` into `module` at the end of the current insertion block,
  /// inserting into `conventions` and `arguments` the passing convention and value of the callee's
  /// receiver if `expr` refers to a bound member function.
  ///
  /// - Requires: `expr` has a lambda type.
  private mutating func emitCallee(
    _ expr: AnyExprID.TypedNode,
    conventions: inout [AccessEffect],
    arguments: inout [Operand],
    into module: inout Module
  ) -> Operand {
    let calleeType = expr.type.base as! LambdaType

    // If the callee is a name expression referring to the declaration of a capture-less function,
    // it is interpreted as a direct function reference.
    if let nameExpr = NameExpr.Typed(expr) {
      switch nameExpr.decl {
      case .direct(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
        // Callee is a direct reference to a function or initializer declaration.
        // TODO: handle captures
        return .constant(
          .function(
            FunctionRef(
              name: DeclLocator(identifying: calleeDecl.id, in: program).mangled,
              type: .address(calleeType))))

      case .direct(let calleeDecl) where calleeDecl.kind == InitializerDecl.self:
        let d = InitializerDecl.Typed(nameExpr)!
        switch d.introducer.value {
        case .`init`:
          // The function is a custom initializer.
          fatalError("not implemented")

        case .memberwiseInit:
          // The function is a memberwise initializer.
          fatalError("not implemented")
        }

      case .member(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
        // Callee is a member reference to a function or method.
        let receiverType = calleeType.captures[0].type

        // Add the receiver to the arguments.
        if let type = RemoteType(receiverType) {
          // The receiver has a borrowing convention.
          conventions.insert(type.access, at: 1)

          switch nameExpr.domain {
          case .none:
            let receiver = module.append(
              BorrowInstruction(
                type.access, .address(type.bareType), from: frames[receiver!]!,
                site: nameExpr.site),
              to: insertionBlock!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            let receiver = emitLValue(receiverID, meantFor: type.access, into: &module)
            arguments.insert(receiver, at: 0)

          case .implicit:
            unreachable()
          }
        } else {
          // The receiver is consumed.
          conventions.insert(.sink, at: 1)

          switch nameExpr.domain {
          case .none:
            let receiver = module.append(
              LoadInstruction(
                .object(receiverType), from: frames[receiver!]!, site: nameExpr.site),
              to: insertionBlock!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            arguments.insert(emitRValue(receiverID, into: &module), at: 0)

          case .implicit:
            unreachable()
          }
        }

        // Emit the function reference.
        return .constant(
          .function(
            FunctionRef(
              name: DeclLocator(identifying: calleeDecl.id, in: program).mangled,
              type: .address(calleeType))))

      case .builtinFunction(let f):
        // Callee refers to a built-in function.
        return .constant(.builtin(f.reference))

      case .builtinType:
        // Built-in types are never called.
        unreachable()

      default:
        // Callee is a lambda.
        break
      }
    }

    // Otherwise, by default, a callee is evaluated as a function object.
    return emitRValue(expr, into: &module)
  }

  /// Inserts the IR for branch condition `expr` into `module` at the end of the current insertion
  /// block.
  ///
  /// - Requires: `expr.type` is `Val.Bool`
  private mutating func emitBranchCondition<ID: ExprID>(
    _ expr: ID.TypedNode,
    into module: inout Module
  ) -> Operand {
    var v = emitLValue(expr, meantFor: .let, into: &module)
    v =
      module.append(
        BorrowInstruction(.let, .address(BuiltinType.i(1)), from: v, at: [0], site: expr.site),
        to: insertionBlock!)[0]
    v =
      module.append(
        CallInstruction(
          returnType: .object(BuiltinType.i(1)),
          calleeConvention: .let,
          callee: .constant(.builtin(BuiltinFunction("copy_i1")!.reference)),
          argumentConventions: [.let],
          arguments: [v],
          site: expr.site),
        to: insertionBlock!)[0]
    return v
  }

  // MARK: l-values

  /// Inserts the IR for the lvalue `expr` meant for `capability` into `module` at the end of the
  /// current insertion block.
  private mutating func emitLValue<ID: ExprID>(
    _ expr: ID.TypedNode,
    meantFor capability: AccessEffect,
    into module: inout Module
  ) -> Operand {
    switch expr.kind {
    case InoutExpr.self:
      return emitLValue(inoutExpr: InoutExpr.Typed(expr)!, meantFor: capability, into: &module)
    case NameExpr.self:
      return emitLValue(name: NameExpr.Typed(expr)!, meantFor: capability, into: &module)
    default:
      return emitLValue(convertingRValue: expr, meantFor: capability, into: &module)
    }
  }

  /// Inserts the IR for the rvalue `expr` converted as a lvalue meant for `capability` into
  /// `module` at the end of the current insertion block.
  private mutating func emitLValue<ID: ExprID>(
    convertingRValue expr: ID.TypedNode,
    meantFor capability: AccessEffect,
    into module: inout Module
  ) -> Operand {
    let value = emitRValue(expr, into: &module)
    let storage = module.append(
      AllocStackInstruction(expr.type, site: expr.site),
      to: insertionBlock!)[0]
    frames.top.allocs.append(storage)

    let target = module.append(
      BorrowInstruction(.set, .address(expr.type), from: storage, site: expr.site),
      to: insertionBlock!)[0]
    module.append(
      StoreInstruction(value, to: target, site: expr.site),
      to: insertionBlock!)

    return module.append(
      BorrowInstruction(capability, .address(expr.type), from: storage, site: expr.site),
      to: insertionBlock!)[0]
  }

  private mutating func emitLValue(
    inoutExpr expr: InoutExpr.Typed,
    meantFor capability: AccessEffect,
    into module: inout Module
  ) -> Operand {
    return emitLValue(expr.subject, meantFor: capability, into: &module)
  }

  private mutating func emitLValue(
    name expr: NameExpr.Typed,
    meantFor capability: AccessEffect,
    into module: inout Module
  ) -> Operand {
    switch expr.decl {
    case .direct(let decl):
      // Lookup for a local symbol.
      if let source = frames[decl] {
        return module.append(
          BorrowInstruction(capability, .address(expr.type), from: source, site: expr.site),
          to: insertionBlock!)[0]
      }

      fatalError("not implemented")

    case .member(let decl):
      // Emit the receiver.
      let r: Operand

      switch expr.domain {
      case .none:
        r = frames[receiver!]!
      case .implicit:
        fatalError("not implemented")
      case .expr(let receiverID):
        r = emitLValue(receiverID, meantFor: capability, into: &module)
      }

      // Emit the bound member.
      switch decl.kind {
      case VarDecl.self:
        let varDecl = VarDecl.Typed(decl)!
        let layout = AbstractTypeLayout(of: module.type(of: r).astType, definedIn: program)
        let memberIndex = layout.offset(of: varDecl.baseName)!

        // If the lowered receiver is a borrow instruction, modify it in place so that it targets
        // the requested stored member. Otherwise, emit a reborrow.
        if let id = r.instruction,
          let receiverInstruction = module[id] as? BorrowInstruction
        {
          module[id] = BorrowInstruction(
            capability, .address(expr.type), from: receiverInstruction.location,
            at: receiverInstruction.path + [memberIndex],
            site: expr.site)
          return r
        } else {
          let member = BorrowInstruction(
            capability, .address(expr.type), from: r,
            at: [memberIndex],
            site: expr.site)
          return module.append(member, to: insertionBlock!)[0]
        }

      default:
        fatalError("not implemented")
      }

    case .builtinFunction, .builtinType:
      // Built-in functions and types are never used as l-value.
      unreachable()
    }

    fatalError()
  }

  // MARK: Helpers

  /// Emits a deallocation instruction for each allocation in the top frame of `self.frames`.
  private mutating func emitStackDeallocs(in module: inout Module, site: SourceRange) {
    while let alloc = frames.top.allocs.popLast() {
      module.append(DeallocStackInstruction(alloc, site: site), to: insertionBlock!)
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

}
