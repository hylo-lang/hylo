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
  private var stack = Stack()

  /// The declaration of the receiver of the function or subscript currently lowered, if any.
  private var receiverDecl: ParameterDecl.Typed?

  /// Creates an emitter with a well-typed AST.
  public init(program: TypedProgram) {
    self.program = program
  }

  // MARK: Declarations

  /// Emits the IR of the top-level declaration `d` into `module`.
  ///
  /// - Requires: `d` is at module scope.
  mutating func emit(topLevel d: AnyDeclID.TypedNode, into module: inout Module) {
    precondition(d.scope.kind == TopLevelDeclSet.self)
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
      unreachable("unexpected declaration")
    }
  }

  /// Emits the IR of `decl` into `module`.
  private mutating func emit(functionDecl decl: FunctionDecl.Typed, into module: inout Module) {
    // Declare the function in the module if necessary.
    let functionID = module.getOrCreateFunction(correspondingTo: decl, program: program)

    // Nothing else to do if the function has no body.
    guard let body = decl.body else { return }

    // Create the function entry.
    assert(module.functions[functionID].blocks.isEmpty)
    let entryID = module.createBasicBlock(
      accepting: module.functions[functionID].inputs.map({ $0.type }),
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
    stack.push(Frame(locals: locals))
    var receiverDecl = decl.receiver
    swap(&receiverDecl, &self.receiverDecl)

    switch body {
    case .block(let stmt):
      // Emit the statements of the function.
      emit(stmt: stmt, into: &module)

    case .expr(let expr):
      // Emit the body of the function.
      let value = emitR(expr: expr, into: &module)

      // Emit stack deallocation.
      emitStackDeallocs(in: &module, site: expr.site)

      // Emit the implicit return statement.
      if expr.type != .never {
        module.append(
          ReturnInstruction(value: value, site: expr.site),
          to: insertionBlock!)
      }
    }

    swap(&receiverDecl, &self.receiverDecl)
    stack.pop()
    assert(stack.isEmpty)
  }

  /// Emits the IR of `decl` into `module`.
  private mutating func emit(subscriptDecl decl: SubscriptDecl.Typed, into module: inout Module) {
    fatalError("not implemented")
  }

  /// Emits the IR of `decl` into `module`.
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

  /// Emits the IR of `decl` into `module`, assuming it's a local binding.
  private mutating func emit(localBindingDecl decl: BindingDecl.Typed, into module: inout Module) {
    switch decl.pattern.introducer.value {
    case .var, .sinklet:
      emit(storedLocalBindingDecl: decl, into: &module)
    case .let:
      emit(borrowedLocalBindingDecl: decl, withCapability: .let, into: &module)
    case .inout:
      emit(borrowedLocalBindingDecl: decl, withCapability: .inout, into: &module)
    }
  }

  /// Emits the IR of `decl` into `module`, assuming it's a local `var` or `sink let` binding.
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
      objects[[]] = emitR(expr: initializer, into: &module)
    }

    // Allocate storage for each name introduced by the declaration.
    for (path, name) in decl.pattern.subpattern.names {
      let storage = module.append(
        AllocStackInstruction(name.decl.type, site: name.site),
        to: insertionBlock!)[0]
      stack.top.allocs.append(storage)
      stack[name.decl] = storage

      if let initializer = decl.initializer {
        // Determine the object corresponding to the current name.
        var rhsType = initializer.type
        for i in 0 ..< path.count {
          // Make sure the initializer has been destructured deeply enough.
          let subpath = Array(path[0 ..< i])
          if objects[subpath] != nil { continue }

          let layout = program.abstractLayout(of: rhsType)
          rhsType = layout.storedPropertiesTypes[i]

          let wholePath = Array(path[0 ..< (i - 1)])
          let whole = objects[wholePath]!
          let parts = module.append(
            DestructureInstruction(
              whole, as: layout.storedPropertiesTypes.map({ .object($0) }),
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

  /// Emits the IR of `decl` into `module`, assuming it's a local `let`, `inout` or `set` binding.
  private mutating func emit(
    borrowedLocalBindingDecl decl: BindingDecl.Typed,
    withCapability capability: AccessEffect,
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
        source = emitL(expr: initializer, withCapability: capability, into: &module)
      } else {
        // emit a r-value and store it into local storage.
        let value = emitR(expr: initializer, into: &module)

        let exprType = initializer.type
        let storage = module.append(
          AllocStackInstruction(exprType, site: pattern.site),
          to: insertionBlock!)[0]
        stack.top.allocs.append(storage)
        source = storage

        let target = module.append(
          BorrowInstruction(.set, .address(exprType), from: storage, site: pattern.site),
          to: insertionBlock!)[0]
        module.append(
          StoreInstruction(value, to: target, site: pattern.site),
          to: insertionBlock!)
      }

      for (path, name) in pattern.subpattern.names {
        stack[name.decl] =
          module.append(
            BorrowInstruction(
              capability, .address(name.decl.type), from: source, at: path, binding: name.decl,
              site: name.decl.site),
            to: insertionBlock!)[0]
      }
    }
  }

  // MARK: Statements

  /// Emits the IR of `stmt` into `module`.
  private mutating func emit<ID: StmtID>(stmt: ID.TypedNode, into module: inout Module) {
    switch stmt.kind {
    case AssignStmt.self:
      emit(assignStmt: AssignStmt.Typed(stmt)!, into: &module)
    case BraceStmt.self:
      emit(braceStmt: BraceStmt.Typed(stmt)!, into: &module)
    case DeclStmt.self:
      emit(declStmt: DeclStmt.Typed(stmt)!, into: &module)
    case ExprStmt.self:
      emit(exprStmt: ExprStmt.Typed(stmt)!, into: &module)
    case ReturnStmt.self:
      emit(returnStmt: ReturnStmt.Typed(stmt)!, into: &module)
    default:
      unreachable("unexpected statement")
    }
  }

  private mutating func emit(assignStmt stmt: AssignStmt.Typed, into module: inout Module) {
    let rhs = emitR(expr: stmt.right, into: &module)
    // FIXME: Should request the capability 'set or inout'.
    let lhs = emitL(expr: stmt.left, withCapability: .set, into: &module)
    _ = module.append(StoreInstruction(rhs, to: lhs, site: stmt.site), to: insertionBlock!)
  }

  private mutating func emit(braceStmt stmt: BraceStmt.Typed, into module: inout Module) {
    stack.push()
    for s in stmt.stmts {
      emit(stmt: s, into: &module)
    }
    emitStackDeallocs(in: &module, site: stmt.site)
    stack.pop()
  }

  private mutating func emit(declStmt stmt: DeclStmt.Typed, into module: inout Module) {
    switch stmt.decl.kind {
    case BindingDecl.self:
      emit(localBindingDecl: BindingDecl.Typed(stmt.decl)!, into: &module)
    default:
      unreachable("unexpected declaration")
    }
  }

  private mutating func emit(exprStmt stmt: ExprStmt.Typed, into module: inout Module) {
    _ = emitR(expr: stmt.expr, into: &module)
  }

  private mutating func emit(returnStmt stmt: ReturnStmt.Typed, into module: inout Module) {
    let value: Operand
    if let expr = stmt.value {
      value = emitR(expr: expr, into: &module)
    } else {
      value = .constant(.void)
    }

    emitStackDeallocs(in: &module, site: stmt.site)
    module.append(ReturnInstruction(value: value, site: stmt.site), to: insertionBlock!)
  }

  // MARK: r-values

  /// Emits the IR of `expr` treated as a r-value into `module`, inserting instructions at the end
  /// of `self.insertionBlock`.
  private mutating func emitR<ID: ExprID>(
    expr: ID.TypedNode,
    into module: inout Module
  ) -> Operand {
    defer {
      // Mark the execution path unreachable if the computed value has type `Never`.
      if expr.type == .never {
        emitStackDeallocs(in: &module, site: expr.site)
        module.append(UnrechableInstruction(site: expr.site), to: insertionBlock!)
      }
    }

    switch expr.kind {
    case BooleanLiteralExpr.self:
      return emitR(booleanLiteralExpr: BooleanLiteralExpr.Typed(expr)!, into: &module)
    case CondExpr.self:
      return emitR(conditionalExpr: CondExpr.Typed(expr)!, into: &module)
    case FunctionCallExpr.self:
      return emitR(functionCallExpr: FunctionCallExpr.Typed(expr)!, into: &module)
    case IntegerLiteralExpr.self:
      return emitR(integerLiteralExpr: IntegerLiteralExpr.Typed(expr)!, into: &module)
    case NameExpr.self:
      return emitR(nameExpr: NameExpr.Typed(expr)!, into: &module)
    case SequenceExpr.self:
      return emitR(sequenceExpr: SequenceExpr.Typed(expr)!, into: &module)
    default:
      unreachable("unexpected expression")
    }
  }

  private mutating func emitR(
    booleanLiteralExpr expr: BooleanLiteralExpr.Typed,
    into module: inout Module
  ) -> Operand {
    let value = Operand.constant(
      .integer(IntegerConstant(expr.value ? 1 : 0, bitWidth: 1)))

    let boolType = program.ast.coreType(named: "Bool")!
    return module.append(
      RecordInstruction(objectType: .object(boolType), operands: [value], site: expr.site),
      to: insertionBlock!)[0]
  }

  private mutating func emitR(
    conditionalExpr expr: CondExpr.Typed,
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
      stack.top.allocs.append(resultStorage!)
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
        var condition = emitL(expr: program[itemExpr], withCapability: .let, into: &module)
        condition =
          module.append(
            BorrowInstruction(
              .let, .address(BuiltinType.i(1)), from: condition, at: [0],
              site: program[itemExpr].site),
            to: insertionBlock!)[0]
        condition =
          module.append(
            CallInstruction(
              returnType: .object(BuiltinType.i(1)),
              calleeConvention: .let,
              callee: .constant(.builtin(BuiltinFunction("copy_i1")!.reference)),
              argumentConventions: [.let],
              arguments: [condition],
              site: program[itemExpr].site),
            to: insertionBlock!)[0]

        module.append(
          CondBranchInstruction(
            condition: condition,
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
      stack.push()
      let value = emitR(expr: program[thenExpr], into: &module)
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
      stack.pop()

    case .block:
      fatalError("not implemented")
    }
    module.append(BranchInstruction(target: continuation, site: expr.site), to: insertionBlock!)

    // Emit the failure branch.
    insertionBlock = alt
    switch expr.failure {
    case .expr(let elseExpr):
      stack.push()
      let value = emitR(expr: program[elseExpr], into: &module)
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
      stack.pop()

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

  private mutating func emitR(
    functionCallExpr expr: FunctionCallExpr.Typed,
    into module: inout Module
  ) -> Operand {
    let calleeType = expr.callee.type.base as! LambdaType

    // Determine the callee's convention.
    let calleeConvention = calleeType.receiverEffect ?? .let

    // Arguments are evaluated first, from left to right.
    var argumentConventions: [AccessEffect] = []
    var arguments: [Operand] = []

    for (parameter, argument) in zip(calleeType.inputs, expr.arguments) {
      let parameterType = parameter.type.base as! ParameterType
      argumentConventions.append(parameterType.convention)
      arguments.append(emit(argument: program[argument.value], to: parameterType, into: &module))
    }

    // If the callee is a name expression referring to the declaration of a function capture-less
    // function, it is interpreted as a direct function reference. Otherwise, it is evaluated as a
    // function object the arguments.
    let callee: Operand

    if let calleeNameExpr = NameExpr.Typed(expr.callee) {
      switch calleeNameExpr.decl {
      case .direct(let calleeDecl) where calleeDecl.kind == BuiltinDecl.self:
        // Callee refers to a built-in function.
        assert(calleeType.environment == .void)
        callee = .constant(.builtin(BuiltinFunction(calleeNameExpr.name.value.stem)!.reference))

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
          argumentConventions.insert(type.capability, at: 0)

          switch calleeNameExpr.domain {
          case .none:
            let receiver = module.append(
              BorrowInstruction(
                type.capability, .address(type.base), from: stack[receiverDecl!]!,
                site: expr.site),
              to: insertionBlock!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            let receiver = emitL(expr: receiverID, withCapability: type.capability, into: &module)
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
              LoadInstruction(.object(receiverType), from: stack[receiverDecl!]!, site: expr.site),
              to: insertionBlock!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            arguments.insert(emitR(expr: receiverID, into: &module), at: 0)

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

      default:
        // Evaluate the callee as a function object.
        callee = emitR(expr: expr.callee, into: &module)
      }
    } else {
      // Evaluate the callee as a function object.
      callee = emitR(expr: expr.callee, into: &module)
    }

    return module.append(
      CallInstruction(
        returnType: .object(expr.type),
        calleeConvention: calleeConvention,
        callee: callee,
        argumentConventions: argumentConventions,
        arguments: arguments,
        site: expr.site),
      to: insertionBlock!)[0]
  }

  private mutating func emitR(
    integerLiteralExpr expr: IntegerLiteralExpr.Typed,
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

  private mutating func emitR(
    nameExpr expr: NameExpr.Typed,
    into module: inout Module
  ) -> Operand {
    switch expr.decl {
    case .direct(let declID):
      // Lookup for a local symbol.
      if let source = stack[declID] {
        return module.append(
          LoadInstruction(.object(expr.type), from: source, site: expr.site),
          to: insertionBlock!)[0]
      }

      fatalError("not implemented")

    case .member:
      fatalError("not implemented")
    }
  }

  private mutating func emitR(
    sequenceExpr expr: SequenceExpr.Typed,
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
      let rhsOperand = emit(rhsType.convention, foldedSequenceExpr: rhs, into: &module)

      let lhsConvention: AccessEffect
      let lhsOperand: Operand
      if let lhsType = RemoteType(calleeType.captures[0].type) {
        lhsConvention = lhsType.capability
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
          argumentConventions: [lhsConvention, rhsType.convention],
          arguments: [lhsOperand, rhsOperand],
          site: program.ast.site(of: expr)),
        to: insertionBlock!)[0]

    case .leaf(let expr):
      switch convention {
      case .let:
        return emitL(expr: program[expr], withCapability: .let, into: &module)
      case .inout:
        return emitL(expr: program[expr], withCapability: .inout, into: &module)
      case .set:
        return emitL(expr: program[expr], withCapability: .set, into: &module)
      case .sink:
        return emitR(expr: program[expr], into: &module)
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
    switch parameterType.convention {
    case .let:
      return emitL(expr: expr, withCapability: .let, into: &module)
    case .inout:
      return emitL(expr: expr, withCapability: .inout, into: &module)
    case .set:
      return emitL(expr: expr, withCapability: .set, into: &module)
    case .sink:
      return emitR(expr: expr, into: &module)
    case .yielded:
      fatalError("not implemented")
    }
  }

  /// Emits the IR of `expr` as an operand suitable to be the callee of a `CallInstruction` into
  /// `module`, updating `conventions` and `arguments` if `expr` refers to a bound member function.
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
      case .direct(let calleeDecl) where calleeDecl.kind == BuiltinDecl.self:
        // Callee refers to a built-in function.
        assert(calleeType.environment == .void)
        return .constant(.builtin(BuiltinFunction(nameExpr.name.value.stem)!.reference))

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
          conventions.insert(type.capability, at: 1)

          switch nameExpr.domain {
          case .none:
            let receiver = module.append(
              BorrowInstruction(
                type.capability, .address(type.base), from: stack[receiverDecl!]!,
                site: nameExpr.site),
              to: insertionBlock!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            let receiver = emitL(expr: receiverID, withCapability: type.capability, into: &module)
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
                .object(receiverType), from: stack[receiverDecl!]!, site: nameExpr.site),
              to: insertionBlock!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            arguments.insert(emitR(expr: receiverID, into: &module), at: 0)

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

      default:
        // Callee is a lambda.
        break
      }
    }

    // Otherwise, by default, a callee is evaluated as a function object.
    return emitR(expr: expr, into: &module)
  }

  // MARK: l-values

  /// Emits the IR of `expr` treated as a l-value with given `capability` into `module`, inserting
  /// instructions at the end of `self.insertionBlock`.
  private mutating func emitL<ID: ExprID>(
    expr: ID.TypedNode,
    withCapability capability: AccessEffect,
    into module: inout Module
  ) -> Operand {
    switch expr.kind {
    case NameExpr.self:
      return emitL(nameExpr: NameExpr.Typed(expr)!, withCapability: capability, into: &module)

    case SubscriptCallExpr.self:
      fatalError("not implemented")

    default:
      let value = emitR(expr: expr, into: &module)
      let storage = module.append(
        AllocStackInstruction(expr.type, site: expr.site),
        to: insertionBlock!)[0]
      stack.top.allocs.append(storage)

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
  }

  private mutating func emitL(
    nameExpr expr: NameExpr.Typed,
    withCapability capability: AccessEffect,
    into module: inout Module
  ) -> Operand {
    switch expr.decl {
    case .direct(let decl):
      // Lookup for a local symbol.
      if let source = stack[decl] {
        return module.append(
          BorrowInstruction(capability, .address(expr.type), from: source, site: expr.site),
          to: insertionBlock!)[0]
      }

      fatalError("not implemented")

    case .member(let decl):
      // Emit the receiver.
      let receiver: Operand

      switch expr.domain {
      case .none:
        receiver = stack[receiverDecl!]!
      case .implicit:
        fatalError("not implemented")
      case .expr(let receiverID):
        receiver = emitL(expr: receiverID, withCapability: capability, into: &module)
      }

      // Emit the bound member.
      switch decl.kind {
      case VarDecl.self:
        let varDecl = VarDecl.Typed(decl)!
        let layout = program.abstractLayout(of: module.type(of: receiver).astType)
        let memberIndex = layout.storedPropertiesIndices[varDecl.baseName]!

        // If the lowered receiver is a borrow instruction, modify it in place so that it targets
        // the requested stored member. Otherwise, emit a reborrow.
        if let id = receiver.instruction,
          let receiverInstruction = module[id] as? BorrowInstruction
        {
          module[id] = BorrowInstruction(
            capability, .address(expr.type), from: receiverInstruction.location,
            at: receiverInstruction.path + [memberIndex],
            site: expr.site)
          return receiver
        } else {
          let member = BorrowInstruction(
            capability, .address(expr.type), from: receiver,
            at: [memberIndex],
            site: expr.site)
          return module.append(member, to: insertionBlock!)[0]
        }

      default:
        fatalError("not implemented")
      }
    }

    fatalError()
  }

  // MARK: Helpers

  /// Pops the top frame of `self.stack` and emits a deallocation instruction for each allocation
  /// in that frame.
  private mutating func emitStackDeallocs(in module: inout Module, site: SourceRange) {
    while let alloc = stack.top.allocs.popLast() {
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

    /// The frames in the stack, in FILO order.
    private var frames: [Frame] = []

    /// True iff the stack is empty.
    var isEmpty: Bool { frames.isEmpty }

    /// Accesses the top frame, assuming the stack is not empty.
    var top: Frame {
      get { frames[frames.count - 1] }
      _modify { yield &frames[frames.count - 1] }
    }

    /// Accesses the IR corresponding to `d`, assuming the stack is not empty.
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

    /// Pops the top frame, assuming the stack is not empty.
    mutating func pop() {
      precondition(top.allocs.isEmpty, "stack leak")
      frames.removeLast()
    }

  }

}
