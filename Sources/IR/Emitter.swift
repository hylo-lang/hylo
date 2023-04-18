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

  /// Inserts the IR for the top-level declarations of `d` into `module`, reporting errors and
  /// warnings to `diagnostics`.
  mutating func emit(
    module d: ModuleDecl.ID,
    into module: inout Module,
    diagnostics: inout DiagnosticSet
  ) {
    swap(&self.diagnostics, &diagnostics)
    defer { swap(&self.diagnostics, &diagnostics) }

    // Lower the top-level declarations.
    for u in program.ast.topLevelDecls(d) {
      emit(topLevel: program[u], into: &module)
    }

    // Lower the synthesized implementations.
    for i in program.synthesizedDecls[d, default: []] {
      emit(synthesizedDecl: i, into: &module)
    }
  }

  /// Inserts the IR for the top-level declaration `d` into `module`.
  ///
  /// - Requires: `d` is at module scope.
  private mutating func emit(
    topLevel d: AnyDeclID.TypedNode,
    into module: inout Module
  ) {
    precondition(d.scope.kind == TranslationUnit.self)

    switch d.kind {
    case ConformanceDecl.self:
      emit(conformanceDecl: ConformanceDecl.Typed(d)!, into: &module)
    case FunctionDecl.self:
      emit(functionDecl: FunctionDecl.Typed(d)!, into: &module)
    case OperatorDecl.self:
      break
    case ProductTypeDecl.self:
      emit(productDecl: ProductTypeDecl.Typed(d)!, into: &module)
    case TraitDecl.self:
      break
    case TypeAliasDecl.self:
      break
    default:
      unexpected(d)
    }
  }

  /// Inserts the IR for `d` into `module`.
  private mutating func emit(conformanceDecl d: ConformanceDecl.Typed, into module: inout Module) {
    for member in d.members {
      switch member.kind {
      case FunctionDecl.self:
        emit(functionDecl: .init(member)!, into: &module)
      case InitializerDecl.self:
        emit(initializerDecl: .init(member)!, into: &module)
      case SubscriptDecl.self:
        emit(subscriptDecl: .init(member)!, into: &module)
      default:
        continue
      }
    }
  }

  /// Inserts the IR for `decl` into `module`, returning the ID of the lowered function.
  @discardableResult
  private mutating func emit(
    functionDecl decl: FunctionDecl.Typed,
    into module: inout Module
  ) -> Function.ID {
    withClearContext({ $0._emit(functionDecl: decl, into: &module) })
  }

  /// Inserts the IR for `decl` into `module`, returning the ID of the lowered function.
  ///
  /// - Precondition: `self` has a clear lowering context.
  private mutating func _emit(
    functionDecl d: FunctionDecl.Typed,
    into module: inout Module
  ) -> Function.ID {
    let f = module.getOrCreateFunction(correspondingTo: d)

    guard let b = d.body else {
      if d.isFFI { emitFFI(d, into: &module) }
      return f
    }

    // Create the function entry.
    assert(module.functions[f]!.blocks.isEmpty)
    let entry = module.appendBlock(
      taking: module.functions[f]!.inputs.map({ .address($0.bareType) }), to: f)

    // Configure the locals.
    var locals = TypedDeclProperty<Operand>()

    for (i, capture) in d.explicitCaptures.enumerated() {
      locals[capture] = .parameter(entry, i)
    }

    for (i, capture) in d.implicitCaptures!.enumerated() {
      locals[program[capture.decl]] = .parameter(entry, i + d.explicitCaptures.count)
    }

    var implicitParameterCount = d.explicitCaptures.count + d.implicitCaptures!.count
    if let receiver = d.receiver {
      locals[receiver] = .parameter(entry, implicitParameterCount)
      implicitParameterCount += 1
    }

    for (i, parameter) in d.parameters.enumerated() {
      locals[parameter] = .parameter(entry, i + implicitParameterCount)
    }

    insertionBlock = entry
    receiver = d.receiver
    frames.push(.init(scope: AnyScopeID(d.id), locals: locals))

    // Emit the body.
    switch b {
    case .block(let s):
      emit(stmt: s, into: &module)

    case .expr(let e):
      let value = emitRValue(e, into: &module)
      emitStackDeallocs(in: &module, site: e.site)
      if e.type != .never {
        module.append(module.makeReturn(value, anchoredAt: e.site), to: insertionBlock!)
      }
    }

    frames.pop()
    assert(frames.isEmpty)
    return f
  }

  /// Inserts the IR for calling `d` into `module`.
  private mutating func emitFFI(_ d: FunctionDecl.Typed, into module: inout Module) {
    let f = module.getOrCreateFunction(correspondingTo: d)

    // Create the function entry.
    assert(module.functions[f]!.blocks.isEmpty)
    let entry = module.appendBlock(
      taking: module.functions[f]!.inputs.map({ .address($0.bareType) }), to: f)
    insertionBlock = entry

    // Emit FFI call.
    var arguments: [Operand] = []
    for i in module[entry].inputs.indices {
      arguments.append(emitFFIArgument(.parameter(entry, i), at: d.site, into: &module))
    }

    let v = module.append(
      module.makeCallFII(
        returning: module.functions[f]!.output,
        applying: d.foreignName!,
        to: arguments, anchoredAt: d.site),
      to: insertionBlock!)[0]
    module.append(module.makeReturn(v, anchoredAt: d.site), to: insertionBlock!)
  }

  /// Appends the IR to convert `o` to a FFI argument into `module` to the current insertion block,
  /// anchoring new instructions at `site`.
  private mutating func emitFFIArgument(
    _ o: Operand,
    at site: SourceRange,
    into module: inout Module
  ) -> Operand {
    let t = module.type(of: o).ast

    let i = program.ast.coreType(named: "Int")!
    let p = program.ast.coreType(named: "RawPointer")!
    if (t == i) || (t == p) {
      let s = module.append(
        module.makeElementAddr(o, at: [0], anchoredAt: site), to: insertionBlock!)[0]
      return module.append(
        module.makeLoad(s, anchoredAt: site), to: insertionBlock!)[0]
    }

    // TODO: Handle other type conversion.
    unreachable("unexpected FFI type \(t)")
  }

  /// Inserts the IR for `d` into `module`.
  private mutating func emit(
    initializerDecl d: InitializerDecl.Typed,
    into module: inout Module
  ) {
    if d.isMemberwise { return }
    let f = module.initializerDeclaration(lowering: d)

    assert(module.functions[f]!.blocks.isEmpty)
    let entry = module.appendBlock(
      taking: module.functions[f]!.inputs.map({ .address($0.bareType) }), to: f)
    insertionBlock = entry

    // Configure the locals.
    var locals = TypedDeclProperty<Operand>()
    locals[d.receiver] = .parameter(entry, 0)
    for (i, parameter) in d.parameters.enumerated() {
      locals[parameter] = .parameter(entry, i + 1)
    }

    // Emit the body.
    frames.push(.init(scope: AnyScopeID(d.id), locals: locals))
    var currentFunctionReceiver: Optional = program[d.receiver]
    swap(&currentFunctionReceiver, &receiver)
    emit(stmt: d.body!, into: &module)
    swap(&currentFunctionReceiver, &self.receiver)
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
      switch member.kind {
      case FunctionDecl.self:
        emit(functionDecl: .init(member)!, into: &module)
      case InitializerDecl.self:
        emit(initializerDecl: .init(member)!, into: &module)
      case SubscriptDecl.self:
        emit(subscriptDecl: .init(member)!, into: &module)
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
    precondition(read(decl.pattern.introducer.value, { ($0 == .var) || ($0 == .sinklet) }))

    /// A map from object path to its corresponding (sub-)object during destructuring.
    var objects: [PartPath: Operand] = [:]
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
        // Initialize (sub-)object corresponding to the current name.
        for i in 0 ..< path.count {
          // Make sure the initializer has been destructured deeply enough.
          if objects[PartPath(path[...i])] != nil { continue }

          // Destructure the (sub-)object.
          let subobject = PartPath(path[..<i])
          let subobjectParts = module.append(
            module.makeDestructure(objects[subobject]!, anchoredAt: initializer.site),
            to: insertionBlock!)
          for j in 0 ..< subobjectParts.count {
            objects[subobject + [j]] = subobjectParts[j]
          }
        }
        emitInitialization(of: storage, to: objects[path]!, anchoredAt: name.site, into: &module)
      }
    }
  }

  /// Inserts the IR for the local binding `decl` into `module`.
  ///
  /// - Requires: `decl` is a local local `let` or `inout` binding.
  private mutating func emit(
    localBindingDecl decl: BindingDecl.Typed,
    borrowing capability: AccessEffect,
    into module: inout Module
  ) {
    precondition(program.isLocal(decl.id))
    precondition(read(decl.pattern.introducer.value, { ($0 == .let) || ($0 == .inout) }))

    // Borrowed binding requires an initializer.
    guard let initializer = decl.initializer else {
      report(.error(binding: capability, requiresInitializerAt: decl.pattern.introducer.site))
      for (_, name) in decl.pattern.subpattern.names {
        frames[name.decl] = .constant(.poison(PoisonConstant(type: .address(name.decl.type))))
      }
      return
    }

    // Initializing inout bindings requires a mutation marker.
    if (capability == .inout) && (initializer.kind != InoutExpr.self) {
      report(.error(inoutBindingRequiresMutationMarkerAt: .empty(at: initializer.site.first())))
    }

    let source = emitLValue(initializer, into: &module)
    for (path, name) in decl.pattern.subpattern.names {
      var part = emitElementAddr(source, at: path, anchoredAt: name.decl.site, into: &module)
      let partType = program.relations.canonical(module.type(of: part).ast)

      if !program.relations.areEquivalent(name.decl.type, partType) {
        if let u = ExistentialType(name.decl.type) {
          let witnessTable = PointerConstant(
            module.syntax.id,
            module.addGlobal(.witnessTable(.init(describing: partType))))
          part =
            module.append(
              module.makeBorrow(capability, from: part, anchoredAt: name.decl.site),
              to: insertionBlock!)[0]
          part =
            module.append(
              module.makeWrapAddr(
                part, .constant(.pointer(witnessTable)), as: u,
                anchoredAt: name.decl.site),
              to: insertionBlock!)[0]
        }
      }

      let b = module.append(
        module.makeBorrow(
          capability, from: part, correspondingTo: name.decl, anchoredAt: name.decl.site),
        to: insertionBlock!)[0]
      frames[name.decl] = b
    }
  }

  /// Inserts the IR for the top-level declaration `d` into `module`.
  private mutating func emit(
    synthesizedDecl d: SynthesizedDecl,
    into module: inout Module
  ) {
    assert(program.module(containing: d.scope) == module.syntax.id)
    switch d.kind {
    case .moveInitialization:
      emitMoveInitialization(typed: .init(d.type)!, in: d.scope, into: &module)
    case .moveAssignment:
      emitMoveAssignment(typed: .init(d.type)!, in: d.scope, into: &module)
    case .copy:
      fatalError("not implemented")
    }
  }

  private mutating func emitMoveInitialization(
    typed t: LambdaType,
    in scope: AnyScopeID,
    into module: inout Module
  ) {
    let site = module.syntax.site
    let f = Function.ID(synthesized: program.moveDecl(.set), for: ^t)
    if !module.declareFunction(identifiedBy: f, typed: t, at: site) { return }

    let entry = module.appendBlock(
      taking: module.functions[f]!.inputs.map({ .address($0.bareType) }), to: f)
    insertionBlock = entry

    let receiver = Operand.parameter(entry, 0)
    let argument = Operand.parameter(entry, 1)

    switch t.output.base {
    case is ProductType, is TupleType:
      // Nothing to do if the receiver doesn't have any stored property.
      let layout = AbstractTypeLayout(of: module.type(of: receiver).ast, definedIn: program)
      if layout.properties.isEmpty { break }

      // Move initialize each property.
      for (i, p) in layout.properties.enumerated() {
        let source = module.append(
          module.makeElementAddr(argument, at: [i], anchoredAt: site),
          to: insertionBlock!)[0]
        let part = module.append(
          module.makeLoad(source, anchoredAt: site),
          to: insertionBlock!)[0]

        let target = module.append(
          module.makeElementAddr(receiver, at: [i], anchoredAt: site),
          to: insertionBlock!)[0]

        if p.type.base is BuiltinType {
          module.append(
            module.makeStore(part, at: target, anchoredAt: site),
            to: insertionBlock!)
        } else {
          let c = program.conformance(of: p.type, to: program.ast.sinkableTrait, exposedTo: scope)!
          emitMove(
            .set, of: part, to: target, conformanceToSinkable: c,
            anchoredAt: site, into: &module)
        }
      }

    default:
      fatalError("not implemented")
    }

    module.append(module.makeReturn(.constant(.void), anchoredAt: site), to: insertionBlock!)
  }

  private mutating func emitMoveAssignment(
    typed t: LambdaType,
    in scope: AnyScopeID,
    into module: inout Module
  ) {
    let site = module.syntax.site
    let f = Function.ID(synthesized: program.moveDecl(.inout), for: ^t)
    if !module.declareFunction(identifiedBy: f, typed: t, at: site) { return }

    let entry = module.appendBlock(
      taking: module.functions[f]!.inputs.map({ .address($0.bareType) }), to: f)
    insertionBlock = entry

    let receiver = Operand.parameter(entry, 0)
    let argument = Operand.parameter(entry, 1)

    // Deinitialize the receiver.
    let l = module.append(module.makeLoad(receiver, anchoredAt: site), to: insertionBlock!)[0]
    module.append(module.makeDeinit(l, anchoredAt: site), to: insertionBlock!)

    // Apply the move-initializer.
    let c = program.conformance(
      of: module.type(of: receiver).ast, to: program.ast.sinkableTrait, exposedTo: scope)!
    let r = module.append(module.makeLoad(argument, anchoredAt: site), to: insertionBlock!)[0]
    emitMove(.set, of: r, to: receiver, conformanceToSinkable: c, anchoredAt: site, into: &module)

    module.append(module.makeReturn(.constant(.void), anchoredAt: site), to: insertionBlock!)
  }

  // MARK: Statements

  /// Inserts the IR for `stmt` into `module`.
  private mutating func emit<ID: StmtID>(stmt: ID.TypedNode, into module: inout Module) {
    switch stmt.kind {
    case AssignStmt.self:
      emit(assignStmt: AssignStmt.Typed(stmt)!, into: &module)
    case BraceStmt.self:
      emit(braceStmt: BraceStmt.Typed(stmt)!, into: &module)
    case ConditionalStmt.self:
      emit(conditionalStmt: ConditionalStmt.Typed(stmt)!, into: &module)
    case DeclStmt.self:
      emit(declStmt: DeclStmt.Typed(stmt)!, into: &module)
    case DiscardStmt.self:
      emit(discardStmt: DiscardStmt.Typed(stmt)!, into: &module)
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
    guard stmt.left.kind == InoutExpr.self else {
      report(.error(assignmentLHSRequiresMutationMarkerAt: .empty(at: stmt.left.site.first())))
      return
    }

    // The RHS is evaluated before the LHS.
    let rhs = emitRValue(stmt.right, into: &module)
    let lhs = emitLValue(stmt.left, into: &module)

    // Built-in types do not require deinitialization.
    let l = program.relations.canonical(stmt.left.type)
    if l.base is BuiltinType {
      emitInitialization(of: lhs, to: rhs, anchoredAt: stmt.site, into: &module)
      return
    }

    let c = program.conformance(of: l, to: program.ast.sinkableTrait, exposedTo: frames.top.scope)!
    let assign = module.appendBlock(to: insertionBlock!.function)
    let initialize = module.appendBlock(to: insertionBlock!.function)
    let tail = module.appendBlock(to: insertionBlock!.function)

    // static_branch initialized(%lhs), assign, initialize
    module.append(
      module.makeStaticBranch(
        if: lhs, is: .initialized, then: assign, else: initialize,
        anchoredAt: stmt.site),
      to: insertionBlock!)

    // %x0 = borrow [inout] %lhs
    // %x1 = call @T.take_value.inout, %x0, %rhs
    insertionBlock = assign
    emitMove(
      .inout, of: rhs, to: lhs, conformanceToSinkable: c,
      anchoredAt: stmt.site, into: &module)
    module.append(module.makeBranch(to: tail, anchoredAt: stmt.site), to: insertionBlock!)

    // %y0 = borrow [set] %lhs
    // %y1 = call @T.take_value.set, %y0, %rhs
    insertionBlock = initialize
    emitMove(
      .set, of: rhs, to: lhs, conformanceToSinkable: c,
      anchoredAt: stmt.site, into: &module)
    module.append(module.makeBranch(to: tail, anchoredAt: stmt.site), to: insertionBlock!)

    insertionBlock = tail
  }

  private mutating func emit(braceStmt stmt: BraceStmt.Typed, into module: inout Module) {
    frames.push(.init(scope: AnyScopeID(stmt.id)))
    for s in stmt.stmts {
      emit(stmt: s, into: &module)
    }
    emitStackDeallocs(in: &module, site: stmt.site)
    frames.pop()
  }

  private mutating func emit(
    conditionalStmt stmt: ConditionalStmt.Typed, into module: inout Module
  ) {
    let (firstBranch, secondBranch) = emitTest(condition: stmt.condition, into: &module)
    let tail: Block.ID

    insertionBlock = firstBranch
    emit(braceStmt: stmt.success, into: &module)
    if let s = stmt.failure {
      tail = module.appendBlock(to: insertionBlock!.function)
      module.append(module.makeBranch(to: tail, anchoredAt: stmt.site), to: insertionBlock!)
      insertionBlock = secondBranch
      emit(stmt: s, into: &module)
    } else {
      tail = secondBranch
    }

    module.append(module.makeBranch(to: tail, anchoredAt: stmt.site), to: insertionBlock!)
    insertionBlock = tail
  }

  private mutating func emit(declStmt stmt: DeclStmt.Typed, into module: inout Module) {
    switch stmt.decl.kind {
    case BindingDecl.self:
      emit(localBindingDecl: BindingDecl.Typed(stmt.decl)!, into: &module)
    default:
      unexpected(stmt.decl)
    }
  }

  private mutating func emit(discardStmt stmt: DiscardStmt.Typed, into module: inout Module) {
    let v = emitRValue(stmt.expr, into: &module)
    module.append(module.makeDeinit(v, anchoredAt: stmt.site), to: insertionBlock!)
  }

  private mutating func emit(doWhileStmt stmt: DoWhileStmt.Typed, into module: inout Module) {
    let loopBody = module.appendBlock(to: insertionBlock!.function)
    let loopTail = module.appendBlock(to: insertionBlock!.function)
    module.append(
      module.makeBranch(to: loopBody, anchoredAt: .empty(at: stmt.site.first())),
      to: insertionBlock!)
    insertionBlock = loopBody

    // Note: we're not using `emit(braceStmt:into:)` because we need to evaluate the loop
    // condition before exiting the scope.
    frames.push(.init(scope: AnyScopeID(stmt.body.id)))
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
    let loopHead = module.appendBlock(to: insertionBlock!.function)
    let loopTail = module.appendBlock(to: insertionBlock!.function)

    // Emit the condition(s).
    module.append(
      module.makeBranch(to: loopHead, anchoredAt: .empty(at: stmt.site.first())),
      to: insertionBlock!)
    insertionBlock = loopHead

    for item in stmt.condition {
      let next = module.appendBlock(to: insertionBlock!.function)

      frames.push(.init(scope: AnyScopeID(stmt.id)))
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
    case CastExpr.self:
      return emitRValue(cast: CastExpr.Typed(expr)!, into: &module)
    case ConditionalExpr.self:
      return emitRValue(conditional: ConditionalExpr.Typed(expr)!, into: &module)
    case FloatLiteralExpr.self:
      return emitRValue(floatLiteral: FloatLiteralExpr.Typed(expr)!, into: &module)
    case FunctionCallExpr.self:
      return emitRValue(functionCall: FunctionCallExpr.Typed(expr)!, into: &module)
    case IntegerLiteralExpr.self:
      return emitRValue(integerLiteral: IntegerLiteralExpr.Typed(expr)!, into: &module)
    case LambdaExpr.self:
      return emitRValue(lambda: LambdaExpr.Typed(expr)!, into: &module)
    case NameExpr.self:
      return emitRValue(name: NameExpr.Typed(expr)!, into: &module)
    case SequenceExpr.self:
      return emitRValue(sequence: SequenceExpr.Typed(expr)!, into: &module)
    case StringLiteralExpr.self:
      return emitRValue(stringLiteral: StringLiteralExpr.Typed(expr)!, into: &module)
    case TupleExpr.self:
      return emitRValue(tuple: TupleExpr.Typed(expr)!, into: &module)
    case TupleMemberExpr.self:
      return emitRValue(tuple: TupleMemberExpr.Typed(expr)!, into: &module)
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
    cast expr: CastExpr.Typed,
    into module: inout Module
  ) -> Operand {
    switch expr.direction {
    case .up:
      return emitRValue(upcast: expr, into: &module)
    case .down:
      return emitRValue(downcast: expr, into: &module)
    default:
      fatalError("not implemented")
    }
  }

  private mutating func emitRValue(
    upcast expr: CastExpr.Typed,
    into module: inout Module
  ) -> Operand {
    precondition(expr.direction == .up)

    let lhs = emitRValue(expr.left, into: &module)
    let rhs = MetatypeType(expr.right.type)!.instance

    // Nothing to do if the LHS already has the desired type.
    if program.relations.areEquivalent(expr.left.type, rhs) {
      return lhs
    }

    fatalError("not implemented")
  }

  private mutating func emitRValue(
    downcast expr: CastExpr.Typed,
    into module: inout Module
  ) -> Operand {
    precondition(expr.direction == .down)

    let lhs = emitRValue(expr.left, into: &module)
    let rhs = MetatypeType(expr.right.type)!.instance

    // Nothing to do if the LHS already has the desired type.
    if program.relations.areEquivalent(expr.left.type, rhs) {
      return lhs
    }

    if expr.left.type.base is ExistentialType {
      let x = module.append(
        module.makeOpen(lhs, as: rhs, anchoredAt: expr.site), to: insertionBlock!)
      emitGuard(x[1], at: expr.site, into: &module)
      return module.append(module.makeLoad(x[0], anchoredAt: expr.site), to: insertionBlock!)[0]
    }

    fatalError("not implemented")
  }

  private mutating func emitRValue(
    conditional expr: ConditionalExpr.Typed,
    into module: inout Module
  ) -> Operand {
    // If the expression is supposed to return a value, allocate storage for it.
    var resultStorage: Operand?
    if expr.type != .void {
      resultStorage =
        module.append(
          module.makeAllocStack(expr.type, anchoredAt: expr.site),
          to: insertionBlock!)[0]
      frames.top.allocs.append(resultStorage!)
    }

    let (firstBranch, secondBranch) = emitTest(condition: expr.condition, into: &module)
    let tail = module.appendBlock(to: insertionBlock!.function)

    // Emit the success branch.
    insertionBlock = firstBranch
    frames.push(.init(scope: AnyScopeID(expr.id)))
    let a = emitRValue(program[expr.success], into: &module)
    if let s = resultStorage {
      emitInitialization(of: s, to: a, anchoredAt: program[expr.success].site, into: &module)
    }
    emitStackDeallocs(in: &module, site: expr.site)
    frames.pop()
    module.append(module.makeBranch(to: tail, anchoredAt: expr.site), to: insertionBlock!)

    // Emit the failure branch.
    insertionBlock = secondBranch
    let i = frames.top.allocs.count
    let b = emitRValue(program[expr.failure], into: &module)
    if let s = resultStorage {
      emitInitialization(of: s, to: b, anchoredAt: program[expr.failure].site, into: &module)
    }
    for a in frames.top.allocs[i...] {
      module.append(module.makeDeallocStack(for: a, anchoredAt: expr.site), to: insertionBlock!)
    }
    frames.top.allocs.removeSubrange(i...)

    module.append(module.makeBranch(to: tail, anchoredAt: expr.site), to: insertionBlock!)

    // Emit the value of the expression.
    insertionBlock = tail
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
    // Handle built-in functions separately.
    if case .builtinFunction(let f) = NameExpr.Typed(expr.callee)?.decl {
      return emit(apply: f, to: expr.arguments, at: expr.site, into: &module)
    }

    // Callee must have a lambda type.
    let calleeType = LambdaType(expr.callee.type)!

    // Arguments are evaluated first, from left to right.
    var arguments: [Operand] = []
    for (p, a) in zip(calleeType.inputs, expr.arguments) {
      arguments.append(emit(argument: program[a.value], to: ParameterType(p.type)!, into: &module))
    }

    // Handle memberwise initializer calls.
    if case .direct(let d) = NameExpr.Typed(expr.callee)?.decl {
      if InitializerDecl.Typed(d)?.isMemberwise ?? false {
        return module.append(
          module.makeRecord(calleeType.output, aggregating: arguments, anchoredAt: expr.site),
          to: insertionBlock!)[0]
      }
    }

    let (callee, liftedArguments) = emitCallee(expr.callee, into: &module)
    return module.append(
      module.makeCall(applying: callee, to: liftedArguments + arguments, anchoredAt: expr.site),
      to: insertionBlock!)[0]
  }

  /// Emits the IR of a call to `f` with given `arguments` at `site` into `module`, inserting
  /// instructions at the end of `self.insertionBlock`.
  private mutating func emit(
    apply f: BuiltinFunction,
    to arguments: [LabeledArgument],
    at site: SourceRange,
    into module: inout Module
  ) -> Operand {
    var a: [Operand] = []
    for e in arguments {
      a.append(emitRValue(program[e.value], into: &module))
    }
    return module.append(
      module.makeLLVM(applying: f, to: a, anchoredAt: site), to: insertionBlock!)[0]
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
    lambda e: LambdaExpr.Typed,
    into module: inout Module
  ) -> Operand {
    let d = emit(functionDecl: e.decl, into: &module)
    let f = FunctionRef(to: d, type: .address(e.decl.type))
    return module.append(
      module.makePartialApply(wrapping: .function(f), with: .constant(.void), anchoredAt: e.site),
      to: insertionBlock!)[0]
  }

  private mutating func emitRValue(
    name e: NameExpr.Typed,
    into module: inout Module
  ) -> Operand {
    switch e.decl {
    case .direct(let d):
      guard let s = frames[d] else { fatalError("not implemented") }
      if module.type(of: s).isObject {
        return s
      } else {
        return module.append(module.makeLoad(s, anchoredAt: e.site), to: insertionBlock!)[0]
      }

    case .member(let d):
      return emitRValue(memberExpr: e, declaredBy: d, into: &module)

    case .builtinFunction:
      fatalError("not implemented")

    case .builtinType:
      fatalError("not implemented")
    }
  }

  private mutating func emitRValue(
    memberExpr e: NameExpr.Typed,
    declaredBy d: AnyDeclID.TypedNode,
    into module: inout Module
  ) -> Operand {
    switch d.kind {
    case VarDecl.self:
      return emitRValue(memberBinding: e, declaredBy: .init(d)!, into: &module)
    default:
      fatalError("not implemented")
    }
  }

  /// Emits the IR for consuming a stored binding `e` at the end of `self.insertionBlock`.
  ///
  /// - Requires: `m.decl` is a `VarDecl`.
  private mutating func emitRValue(
    memberBinding e: NameExpr.Typed,
    declaredBy d: VarDecl.Typed,
    into module: inout Module
  ) -> Operand {
    let base: Operand
    let propertyIndex: Int

    // Member reference without a domain expression are implicitly bound to `self`.
    if let domain = e.domainExpr {
      base = emitLValue(domain, into: &module)
      propertyIndex = AbstractTypeLayout(of: domain.type, definedIn: program)
        .offset(of: d.baseName)!
    } else {
      base = frames[receiver!]!

      let receiverType = ParameterType(receiver!.type)!
      propertyIndex = AbstractTypeLayout(of: receiverType.bareType, definedIn: program)
        .offset(of: d.baseName)!
    }

    let s = module.append(
      module.makeElementAddr(base, at: [propertyIndex], anchoredAt: e.site),
      to: insertionBlock!)[0]
    return module.append(
      module.makeLoad(s, anchoredAt: e.site),
      to: insertionBlock!)[0]
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
      let ref = FunctionRef(to: .init(FunctionDecl.ID(calleeDecl)!), type: .address(calleeType))
      let f = Operand.constant(.function(ref))

      // Emit the call.
      return module.append(
        module.makeCall(applying: f, to: [l, r], anchoredAt: program.ast.site(of: expr)),
        to: insertionBlock!)[0]

    case .leaf(let expr):
      return (convention == .sink)
        ? emitRValue(program[expr], into: &module)
        : emitLValue(program[expr], meantFor: convention, into: &module)
    }
  }

  private mutating func emitRValue(
    stringLiteral syntax: StringLiteralExpr.Typed,
    into module: inout Module
  ) -> Operand {
    let bytes = syntax.value.data(using: .utf8)!
    let size = emitWord(bytes.count, at: syntax.site, into: &module)

    let p = PointerConstant(module.syntax.id, module.addGlobal(.buffer(.init(bytes))))
    let base = emitCoreInstance(
      of: "RawPointer", aggregating: [.constant(.pointer(p))], at: syntax.site, into: &module)

    return emitCoreInstance(
      of: "String", aggregating: [size, base], at: syntax.site, into: &module)
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

  private mutating func emitRValue(
    tuple syntax: TupleMemberExpr.Typed,
    into module: inout Module
  ) -> Operand {
    let base = emitLValue(syntax.tuple, into: &module)
    let element = module.append(
      module.makeElementAddr(base, at: [syntax.index.value], anchoredAt: syntax.index.site),
      to: insertionBlock!)[0]
    return module.append(
      module.makeLoad(element, anchoredAt: syntax.index.site),
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
    }

    let f = emitLambdaCallee(callee, into: &module)
    return (f, [])
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

      let ref = FunctionRef(to: .init(FunctionDecl.ID(d.id)!), type: .address(calleeType))
      return (.constant(.function(ref)), [])

    case .direct(let d) where d.kind == InitializerDecl.self:
      // Callee is a direct reference to an initializer declaration.
      let ref = FunctionRef(to: .init(constructor: .init(d.id)!), type: .address(calleeType))
      return (.constant(.function(ref)), [])

    case .member(let d) where d.kind == FunctionDecl.self:
      // Callee is a member reference to a function or method.
      let ref = FunctionRef(to: .init(FunctionDecl.ID(d.id)!), type: .address(calleeType.lifted))
      let fun = Operand.constant(.function(ref))

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
        return (fun, i)
      } else {
        let i = module.append(
          module.makeLoad(receiver, anchoredAt: callee.site),
          to: insertionBlock!)
        return (fun, i)
      }

    case .builtinFunction, .builtinType:
      // Calls to built-ins should have been handled already.
      unreachable()

    default:
      // Callee is a lambda.
      let f = emitLambdaCallee(.init(callee), into: &module)
      return (f, [])
    }
  }

  /// Inserts the IR for given `callee` into `module` at the end of the current insertion block and
  /// returns its value.
  ///
  /// - Requires: `callee` has a lambda type.
  private mutating func emitLambdaCallee(
    _ callee: AnyExprID.TypedNode,
    into module: inout Module
  ) -> Operand {
    switch LambdaType(callee.type)!.receiverEffect {
    case .yielded:
      unreachable()

    case .set:
      fatalError("not implemented")

    case .sink:
      return emitRValue(callee, into: &module)

    case let k:
      let l = emitLValue(callee, into: &module)
      let b = module.makeBorrow(k, from: l, anchoredAt: callee.site)
      return module.append(b, to: insertionBlock!)[0]
    }
  }

  /// Returns `(success: a, failure: b)` where `a` is the basic block reached if all items in
  /// `condition` hold and `b` is the basic block reached otherwise, inserting new instructions
  /// at the end of the current insertion block.
  private mutating func emitTest(
    condition: [ConditionItem], into module: inout Module
  ) -> (success: Block.ID, failure: Block.ID) {
    let failure = module.appendBlock(to: insertionBlock!.function)
    let success = condition.reduce(insertionBlock!) { (b, c) -> Block.ID in
      switch c {
      case .expr(let e):
        let test = emitBranchCondition(program[e], into: &module)
        let next = module.appendBlock(to: b.function)
        module.append(
          module.makeCondBranch(if: test, then: next, else: failure, anchoredAt: program[e].site),
          to: b)
        return next

      case .decl:
        fatalError("not implemented")
      }
    }

    return (success: success, failure: failure)
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
        .error(integerLiteral: s, overflowsWhenStoredInto: literalType, at: anchor))
      return .constant(.poison(PoisonConstant(type: .object(literalType))))
    }

    return module.append(
      module.makeRecord(
        literalType, aggregating: [.constant(.integer(IntegerConstant(b)))], anchoredAt: anchor),
      to: insertionBlock!)[0]
  }

  /// Inserts the IR for the construction of an instance of a core type named `n`with given `value`
  /// into `module` at the end of the current insertion block, anchoring instructions at `site`.
  ///
  /// - precondition: `type` is a core type.
  private mutating func emitCoreInstance(
    of n: String,
    aggregating parts: [Operand],
    at site: SourceRange,
    into module: inout Module
  ) -> Operand {
    let t = program.ast.coreType(named: n)!
    let o = module.makeRecord(t, aggregating: parts, anchoredAt: site)
    return module.append(o, to: insertionBlock!)[0]
  }

  /// Inserts the IR for the construction of a a machine word with given `value` into `module` at
  /// the end of the current insertion block, anchoring instructions at `site`.
  private mutating func emitWord(
    _ value: Int,
    at site: SourceRange,
    into module: inout Module
  ) -> Operand {
    emitCoreInstance(
      of: "Int",
      aggregating: [.constant(.integer(IntegerConstant(value, bitWidth: 64)))],
      at: site,
      into: &module)
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
    case TupleMemberExpr.self:
      return emitLValue(name: TupleMemberExpr.Typed(syntax)!, into: &module)
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

    emitInitialization(of: storage, to: value, anchoredAt: syntax.site, into: &module)
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

  private mutating func emitLValue(
    name syntax: TupleMemberExpr.Typed,
    into module: inout Module
  ) -> Operand {
    let base = emitLValue(syntax.tuple, into: &module)
    return module.append(
      module.makeElementAddr(base, at: [syntax.index.value], anchoredAt: syntax.index.site),
      to: insertionBlock!)[0]
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
        of: module.type(of: receiverAddress).ast, definedIn: program)

      let i = receiverLayout.offset(of: VarDecl.Typed(decl)!.baseName)!
      return module.append(
        module.makeElementAddr(receiverAddress, at: [i], anchoredAt: anchor),
        to: insertionBlock!)[0]

    default:
      fatalError("not implemented")
    }
  }

  // MARK: Helpers

  /// Appends the IR for computing the address of the property at `path` rooted at `base` into
  /// `module`, anchoring new at `anchor`.
  ///
  /// - Returns: The result of `element_addr base, path` instruction if `path` is not empty;
  ///   otherwise, returns `base` unchanged.
  private mutating func emitElementAddr(
    _ base: Operand, at path: PartPath,
    anchoredAt anchor: SourceRange, into module: inout Module
  ) -> Operand {
    if path.isEmpty { return base }
    return module.append(
      module.makeElementAddr(base, at: path, anchoredAt: anchor),
      to: insertionBlock!)[0]
  }

  /// Inserts the IR for initializing `storage` with `value` at the end of the current insertion
  /// block, anchoring new instructions at `anchor` into `module`.
  private mutating func emitInitialization(
    of storage: Operand,
    to value: Operand,
    anchoredAt anchor: SourceRange,
    into module: inout Module
  ) {
    let s = module.append(
      module.makeBorrow(.set, from: storage, anchoredAt: anchor),
      to: insertionBlock!)[0]
    module.append(
      module.makeStore(value, at: s, anchoredAt: anchor),
      to: insertionBlock!)
  }

  /// Appends the IR for a call to move-initialize/assign `storage` with `value` into `module`,
  /// using conformance `c` to identify the implementations of these operations and anchoring new
  /// instructions at `anchor`.
  ///
  /// Use pass `.set` or `.inout` to `access` to use the move-initialization or move-assignment,
  /// respectively.
  private mutating func emitMove(
    _ access: AccessEffect,
    of value: Operand,
    to storage: Operand,
    conformanceToSinkable c: Conformance,
    anchoredAt anchor: SourceRange,
    into module: inout Module
  ) {
    let moveInit = program.moveDecl(access)
    switch c.implementations[moveInit]! {
    case .concrete:
      fatalError("not implemented")

    case .synthetic(let t):
      assert(t[.isCanonical])

      let callee = LambdaType(t)!.lifted
      let ref = FunctionRef(
        to: .init(synthesized: program.moveDecl(access), for: t),
        type: .address(callee))
      let fun = Operand.constant(.function(ref))

      let receiver = module.append(
        module.makeBorrow(access, from: storage, anchoredAt: anchor),
        to: insertionBlock!)[0]
      module.append(
        module.makeCall(applying: fun, to: [receiver, value], anchoredAt: anchor),
        to: insertionBlock!)
    }
  }

  /// Emits a deallocation instruction for each allocation in the top frame of `self.frames`.
  private mutating func emitStackDeallocs(in module: inout Module, site: SourceRange) {
    while let a = frames.top.allocs.popLast() {
      module.append(
        module.makeDeallocStack(for: a, anchoredAt: site),
        to: insertionBlock!)
    }
  }

  /// Emits the IR trapping iff `predicate`, which is an object of type `i1`, into `module`,
  /// anchoring new instructions at `anchor`.
  private mutating func emitGuard(
    _ predicate: Operand, at anchor: SourceRange, into module: inout Module
  ) {
    let failure = module.appendBlock(to: insertionBlock!.function)
    let success = module.appendBlock(to: insertionBlock!.function)
    module.append(
      module.makeCondBranch(if: predicate, then: success, else: failure, anchoredAt: anchor),
      to: insertionBlock!)

    insertionBlock = failure
    module.append(module.makeUnreachable(anchoredAt: anchor), to: insertionBlock!)

    insertionBlock = success
  }

  /// Returns the result of applying `action` to `self` in a copy of `self` whose insertion block,
  /// frames, and receiver are clear.
  private mutating func withClearContext<T>(_ action: (inout Self) throws -> T) rethrows -> T {
    var b: Block.ID? = nil
    var r: ParameterDecl.Typed? = nil
    var f = Stack()

    swap(&b, &insertionBlock)
    swap(&r, &receiver)
    swap(&f, &frames)
    defer {
      swap(&b, &insertionBlock)
      swap(&r, &receiver)
      swap(&f, &frames)
    }
    return try action(&self)
  }

}

extension Emitter {

  /// The local variables and allocations of a lexical scope.
  fileprivate struct Frame {

    /// The lexical scope corresponding this this frame.
    let scope: AnyScopeID

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
    mutating func push(_ newFrame: Frame) {
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

  static func error(assignmentLHSRequiresMutationMarkerAt site: SourceRange) -> Diagnostic {
    .error("left-hand side of assignment must be marked for mutation", at: site)
  }

  static func error(inoutBindingRequiresMutationMarkerAt site: SourceRange) -> Diagnostic {
    .error("initialization of inout binding must be marked for mutation", at: site)
  }

  static func error(
    binding a: AccessEffect, requiresInitializerAt site: SourceRange
  ) -> Diagnostic {
    .error("declaration of \(a) binding requires an initializer", at: site)
  }

  static func error(
    integerLiteral s: String, overflowsWhenStoredInto t: AnyType,
    at site: SourceRange
  ) -> Diagnostic {
    .error("integer literal '\(s)' overflows when stored into '\(t)'", at: site)
  }

}

extension TypedProgram {

  /// Returns the conformance of `model` to `concept` exposed to `useSite` or `nil` if no such
  /// conformance exists.
  fileprivate func conformance(
    of model: AnyType, to concept: TraitType, exposedTo useSite: AnyScopeID
  ) -> Conformance? {
    guard
      let allConformances = relations.conformances[relations.canonical(model)],
      let conformancesToConcept = allConformances[concept]
    else { return nil }

    // Return the first conformance exposed to `useSite`,
    let fileImports = imports[source(containing: useSite), default: []]
    return conformancesToConcept.first { (c) in
      if let m = ModuleDecl.ID(c.scope), fileImports.contains(m) {
        return true
      } else {
        return isContained(useSite, in: c.scope)
      }
    }
  }

  /// Returns the declaration of `Sinkable.take_value`'s requirement for given `access`.
  ///
  /// Use the access `.set` or `.inout` to get the declaration of the move-initialization or
  /// move-assignment, respectively.
  ///
  /// - Requires: `access` is either `.set` or `.inout`.
  fileprivate func moveDecl(_ access: AccessEffect) -> MethodImpl.ID {
    ast[ast.sinkableTrait.decl].members.first { (m) -> MethodImpl.ID? in
      guard
        let d = MethodDecl.ID(m),
        ast[d].identifier.value == "take_value"
      else { return nil }
      return ast[d].impls.first(where: { ast[$0].introducer.value == access })
    }!
  }

  /// Returns the declaration of `Copyable.copy`'s requirement.
  fileprivate func copyDecl() -> FunctionDecl.ID {
    ast[ast.copyableTrait.decl].members.first { (m) -> FunctionDecl.ID? in
      guard
        let d = FunctionDecl.ID(m),
        ast[d].identifier?.value == "copy"
      else { return nil }
      return d
    }!
  }

}
