/// Implementation notes
/// ====================
///
/// Unless documented otherwise, all methods prefixed by `emit` insert new IR in `Emitter.module`
/// and append new instructions at the end of `Emitter.insertionBlock`.

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

  /// The module into which new IR is inserted.
  private var module: Module!

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

  /// The scope corresponding to the current insertion block.
  private var insertionScope: AnyScopeID? {
    insertionBlock.map({ module[$0].scope })
  }

  /// The address of the return value in the current function, if any.
  private var returnValue: Operand? {
    guard
      let f = insertionBlock?.function,
      let b = module.entry(of: f),
      !module[f].isSubscript
    else { return nil }
    return .parameter(b, module[f].inputs.count)
  }

  /// Reports the given diagnostic.
  private mutating func report(_ d: Diagnostic) {
    diagnostics.insert(d)
  }

  /// Appends `newInstruction` into `self.module`, at the end of `self.insertionBlock`.
  @discardableResult
  private mutating func append<I: Instruction>(_ newInstruction: I) -> [Operand] {
    module.append(newInstruction, to: insertionBlock!)
  }

  // MARK: Declarations

  /// Inserts the IR for the top-level declarations of `d` into `module`, reporting errors and
  /// warnings to `diagnostics`.
  mutating func emit(
    module d: ModuleDecl.ID,
    into module: inout Module,
    diagnostics: inout DiagnosticSet
  ) {
    self.module = module
    swap(&self.diagnostics, &diagnostics)
    defer {
      module = self.module.release()
      swap(&self.diagnostics, &diagnostics)
    }

    // Lower the top-level declarations.
    for u in program.ast.topLevelDecls(d) {
      emit(topLevel: program[u])
    }

    // Lower the synthesized implementations.
    for i in program.synthesizedDecls[d, default: []] {
      emit(synthesizedDecl: i)
    }
  }

  /// Inserts the IR for the top-level declaration `d`.
  ///
  /// - Requires: `d` is at module scope.
  private mutating func emit(topLevel d: AnyDeclID.TypedNode) {
    precondition(program.isAtModuleScope(d.id))
    switch d.kind {
    case BindingDecl.self:
      emit(globalBindingDecl: .init(d)!)
    case ConformanceDecl.self:
      emit(conformanceDecl: .init(d)!)
    case ExtensionDecl.self:
      emit(extensionDecl: .init(d)!)
    case FunctionDecl.self:
      emit(functionDecl: .init(d)!)
    case OperatorDecl.self:
      break
    case NamespaceDecl.self:
      emit(namespaceDecl: .init(d)!)
    case ProductTypeDecl.self:
      emit(productDecl: .init(d)!)
    case TraitDecl.self:
      emit(traitDecl: .init(d)!)
    case TypeAliasDecl.self:
      break
    default:
      unexpected(d)
    }
  }

  /// Inserts the IR for `d`.
  private mutating func emit(conformanceDecl d: ConformanceDecl.Typed) {
    emit(members: d.members)
  }

  /// Inserts the IR for `d`.
  private mutating func emit(extensionDecl d: ExtensionDecl.Typed) {
    emit(members: d.members)
  }

  /// Inserts the IR for `d`, returning the ID of the lowered function.
  @discardableResult
  private mutating func emit(functionDecl d: FunctionDecl.Typed) -> Function.ID {
    withClearContext({ $0._emit(functionDecl: d) })
  }

  /// Inserts the IR for `d`, returning the ID of the lowered function.
  ///
  /// - Requires: `self` has a clear lowering context.
  private mutating func _emit(functionDecl d: FunctionDecl.Typed) -> Function.ID {
    let f = module.demandFunctionDeclaration(lowering: d)
    guard let b = d.body else {
      if d.isForeignInterface { emitFFI(d) }
      return f
    }

    // Create the function entry.
    let entry = module.appendEntry(in: program.scopeContainingBody(of: d.id)!, to: f)

    // Configure the locals.
    var locals = TypedDeclProperty<Operand>()

    for (i, c) in d.explicitCaptures.enumerated() {
      locals[c] = .parameter(entry, i)
    }
    for (i, c) in d.implicitCaptures!.enumerated() {
      locals[program[c.decl]] = .parameter(entry, i + d.explicitCaptures.count)
    }

    var captureCount = d.explicitCaptures.count + d.implicitCaptures!.count
    if let r = d.receiver {
      locals[r] = .parameter(entry, captureCount)
      captureCount += 1
    }

    for (i, p) in d.parameters.enumerated() {
      locals[p] = .parameter(entry, i + captureCount)
    }

    // Configure the emitter context.
    self.insertionBlock = entry
    self.receiver = d.receiver
    self.frames.push(.init(locals: locals))
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    // Emit the body.
    switch b {
    case .block(let s):
      emit(stmt: s)

    case .expr(let e):
      store(returnValue: emitRValue(e), at: e.site)
      emitStackDeallocs(site: e.site)
      append(module.makeReturn(at: e.site))
    }

    return f
  }

  /// Inserts the IR for calling `d`, which is a foreign function interface.
  private mutating func emitFFI(_ d: FunctionDecl.Typed) {
    let f = module.demandFunctionDeclaration(lowering: d)

    // Create the function entry.
    let entry = module.appendEntry(in: d.id, to: f)

    // Configure the emitter context.
    self.insertionBlock = entry
    self.frames.push()
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    // Convert Val arguments to their foreign representation. Note that the last parameter of the
    // entry is the address of the FFI's return value.
    var arguments: [Operand] = []
    for i in 0 ..< module[entry].inputs.count - 1 {
      let a = emitConvertToForeign(.parameter(entry, i), at: d.site)
      arguments.append(a)
    }

    // Emit the call to the foreign function.
    let returnType = module.functions[f]!.output
    let foreignResult = append(
      module.makeCallFFI(
        returning: .object(returnType), applying: d.foreignName!, to: arguments, at: d.site))[0]

    // Convert the result of the FFI to its Val representation and return it.
    let returnValue: Operand
    if returnType.isVoidOrNever {
      returnValue = .void
    } else {
      returnValue = emitConvert(foreign: foreignResult, to: returnType, at: d.site)
    }

    store(returnValue: returnValue, at: d.site)
    emitStackDeallocs(site: d.site)
    append(module.makeReturn(at: d.site))
  }

  /// Inserts the IR for `d`.
  private mutating func emit(initializerDecl d: InitializerDecl.Typed) {
    // Nothing to do for memberwise initializer.
    if d.isMemberwise { return }
    let f = module.demandInitializerDeclaration(lowering: d)

    // Create the function entry.
    let entry = module.appendEntry(in: d.body!.id, to: f)

    // Configure the locals.
    var locals = TypedDeclProperty<Operand>()
    locals[d.receiver] = .parameter(entry, 0)
    for (i, parameter) in d.parameters.enumerated() {
      locals[parameter] = .parameter(entry, i + 1)
    }

    // Configure the emitter context.
    let r = self.receiver
    insertionBlock = entry
    receiver = program[d.receiver]
    frames.push(.init(locals: locals))
    defer {
      frames.pop()
      receiver = r
      assert(frames.isEmpty)
    }

    // Emit the body.
    emit(stmt: d.body!)
  }

  /// Inserts the IR for `d`.
  private mutating func emit(subscriptDecl d: SubscriptDecl.Typed) {
    for i in d.impls {
      emit(subscriptImpl: i)
    }
  }

  /// Inserts the IR for `d`.
  private mutating func emit(subscriptImpl d: SubscriptImpl.Typed) {
    let f = module.demandSubscriptDeclaration(lowering: d)
    guard let b = d.body else { return }

    // Create the function entry.
    let entry = module.appendEntry(in: program.scopeContainingBody(of: d.id)!, to: f)

    // Configure the locals.
    var locals = TypedDeclProperty<Operand>()

    let bundle = SubscriptDecl.Typed(d.parent!)!
    for (i, c) in bundle.explicitCaptures.enumerated() {
      locals[c] = .parameter(entry, i)
    }
    for (i, c) in bundle.implicitCaptures!.enumerated() {
      locals[program[c.decl]] = .parameter(entry, i + bundle.explicitCaptures.count)
    }

    var captureCount = bundle.explicitCaptures.count + bundle.implicitCaptures!.count
    if let receiver = d.receiver {
      locals[receiver] = .parameter(entry, captureCount)
      captureCount += 1
    }

    if let parameters = bundle.parameters {
      for (i, p) in parameters.enumerated() {
        locals[program[p]] = .parameter(entry, i + captureCount)
      }
    }

    // Configure the emitter context.
    self.insertionBlock = entry
    self.receiver = d.receiver
    self.frames.push(.init(locals: locals))
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    // Emit the body.
    switch b {
    case .block(let s):
      emit(stmt: program[s])

    case .expr(let e):
      let s = emitLValue(program[e])
      let b = append(
        module.makeBorrow(d.introducer.value, from: s, at: program.ast[e].site))[0]
      append(module.makeYield(d.introducer.value, b, at: program.ast[e].site))
      emitStackDeallocs(site: program.ast[e].site)
      append(module.makeReturn(at: program.ast[e].site))
    }
  }

  /// Inserts the IR for `decl`.
  private mutating func emit(namespaceDecl decl: NamespaceDecl.Typed) {
    for m in decl.members {
      emit(topLevel: m)
    }
  }

  /// Inserts the IR for `d`.
  private mutating func emit(productDecl d: ProductTypeDecl.Typed) {
    _ = module.addGlobal(MetatypeConstant(.init(d.type)!))
    emit(members: d.members)
  }

  /// Inserts the IR for `d`.
  private mutating func emit(traitDecl d: TraitDecl.Typed) {
    _ = module.addGlobal(MetatypeConstant(.init(d.type)!))
  }

  /// Inserts the IR for given declaration `members`.
  private mutating func emit(members: [AnyDeclID]) {
    for m in members {
      switch m.kind {
      case FunctionDecl.self:
        emit(functionDecl: .init(program[m])!)
      case InitializerDecl.self:
        emit(initializerDecl: .init(program[m])!)
      case SubscriptDecl.self:
        emit(subscriptDecl: .init(program[m])!)
      default:
        continue
      }
    }
  }

  /// Inserts the IR for `d`.
  ///
  /// - Requires: `d` is a global binding.
  private mutating func emit(globalBindingDecl d: BindingDecl.Typed) {
    fatalError("not implemented")
  }

  /// Inserts the IR for the local binding `decl`.
  ///
  /// - Requires: `decl` is a local binding.
  private mutating func emit(localBindingDecl decl: BindingDecl.Typed) {
    switch decl.pattern.introducer.value {
    case .var, .sinklet:
      emit(storedLocalBindingDecl: decl)
    case .let:
      emit(localBindingDecl: decl, borrowing: .let)
    case .inout:
      emit(localBindingDecl: decl, borrowing: .inout)
    }
  }

  /// Inserts the IR for the local binding `decl`.
  ///
  /// - Requires: `decl` is a local local `var` or `sink let` binding.
  private mutating func emit(storedLocalBindingDecl decl: BindingDecl.Typed) {
    precondition(program.isLocal(decl.id))
    precondition(read(decl.pattern.introducer.value, { ($0 == .var) || ($0 == .sinklet) }))

    // Allocate storage for all the names declared by `decl`.
    let storage = emitAllocStack(for: decl.type, at: decl.site)

    // Declare each introduced name and initialize them if possible.
    let lhs = decl.pattern.subpattern.id
    if let initializer = decl.initializer {
      program.ast.walking(pattern: lhs, expression: initializer.id) { (path, p, rhs) in
        // Declare the introduced name if `p` is a name pattern. Otherwise, drop the value of the
        // the corresponding expression.
        if let name = NamePattern.ID(p) {
          declare(name: program[name], referringTo: path, initializedTo: rhs)
        } else {
          let part = emitRValue(program[rhs])
          append(module.makeDeinit(part, at: program.ast[p].site))
        }
      }
    } else {
      for (path, name) in program.ast.names(in: lhs) {
        _ = declare(name: program[name], referringTo: path)
      }
    }

    /// Inserts the IR to declare `name`, which refers to the sub-location at `pathInStorage`, and
    /// initialize it to the result of `rhs`.
    func declare(
      name: NamePattern.Typed, referringTo pathInStorage: PartPath,
      initializedTo rhs: AnyExprID
    ) {
      // TODO: Handle existentials
      let sublocation = declare(name: name, referringTo: pathInStorage)
      emitInitialization(of: sublocation, to: rhs)
    }

    /// Inserts the IR to declare `name`, which refers to the sub-location at `pathInStorage`,
    /// returning that sub-location.
    func declare(name: NamePattern.Typed, referringTo pathInStorage: PartPath) -> Operand {
      let sublocation = append(
        module.makeElementAddr(storage, at: pathInStorage, at: name.site))[0]
      frames[name.decl] = sublocation
      return sublocation
    }
  }

  /// Inserts the IR for the local binding `decl`.
  ///
  /// - Requires: `decl` is a local local `let` or `inout` binding.
  private mutating func emit(
    localBindingDecl decl: BindingDecl.Typed,
    borrowing capability: AccessEffect
  ) {
    precondition(program.isLocal(decl.id))
    precondition(read(decl.pattern.introducer.value, { ($0 == .let) || ($0 == .inout) }))

    // Borrowed binding requires an initializer.
    guard let initializer = decl.initializer else {
      report(.error(binding: capability, requiresInitializerAt: decl.pattern.introducer.site))
      for (_, name) in decl.pattern.subpattern.names {
        frames[name.decl] = .constant(Poison(type: .address(name.decl.type)))
      }
      return
    }

    // Initializing inout bindings requires a mutation marker.
    if (capability == .inout) && (initializer.kind != InoutExpr.self) {
      report(.error(inoutBindingRequiresMutationMarkerAt: .empty(at: initializer.site.first())))
    }

    let source = emitLValue(initializer)
    let isSink = module.isSink(source, in: insertionBlock!.function)

    for (path, name) in decl.pattern.subpattern.names {
      var part = emitElementAddr(source, at: path, at: name.decl.site)
      let partType = module.type(of: part).ast

      if !program.relations.areEquivalent(name.decl.type, partType) {
        if let u = ExistentialType(name.decl.type) {
          let box = emitExistential(
            u, borrowing: capability, from: part,
            at: name.decl.site)
          part = box
        }
      }

      if isSink {
        let b = module.makeAccess(
          [.sink, capability], from: part, correspondingTo: name.decl,
          at: name.decl.site)
        frames[name.decl] = append(b)[0]
      } else {
        let b = module.makeBorrow(
          capability, from: part, correspondingTo: name.decl,
          at: name.decl.site)
        frames[name.decl] = append(b)[0]
      }
    }
  }

  /// Returns an existential container of type `t` borrowing `capability` from `witness`.
  private mutating func emitExistential(
    _ t: ExistentialType,
    borrowing capability: AccessEffect,
    from witness: Operand,
    at site: SourceRange
  ) -> Operand {
    let witnessTable = emitWitnessTable(of: module.type(of: witness).ast, usedIn: insertionScope!)
    let g = PointerConstant(module.syntax.id, module.addGlobal(witnessTable))

    let x0 = append(module.makeBorrow(capability, from: witness, at: site))[0]
    let x1 = append(module.makeWrapAddr(x0, .constant(g), as: t, at: site))[0]
    return x1
  }

  /// Returns the witness table of `t` in `s`.
  private mutating func emitWitnessTable(
    of t: AnyType, usedIn s: AnyScopeID
  ) -> WitnessTable {
    .init(for: t, conformingTo: emitConformances(of: t, exposedTo: s))
  }

  /// Returns the lowered conformances of `model` that are exposed to `useScope`.
  private mutating func emitConformances(
    of model: AnyType, exposedTo useScope: AnyScopeID
  ) -> Set<LoweredConformance> {
    guard let conformances = program.relations.conformances[model] else { return [] }

    var result: Set<LoweredConformance> = []
    for concept in conformances.keys {
      let c = program.conformance(of: model, to: concept, exposedTo: useScope)!
      result.insert(emitConformance(c, in: useScope))
    }
    return result
  }

  /// Returns the lowered form of `c`, generating function references in `useScope`.
  private mutating func emitConformance(
    _ c: Conformance, in useScope: AnyScopeID
  ) -> LoweredConformance {
    var implementations = LoweredConformance.ImplementationMap()
    for (r, i) in c.implementations.storage {
      switch i {
      case .concrete(let d):
        implementations[r] = emitRequirementImplementation(d, in: useScope)

      case .synthetic(let d):
        let f = emit(synthesizedDecl: d)
        implementations[r] = .function(.init(to: f, usedIn: useScope, in: module))
      }
    }

    return .init(concept: c.concept, source: c.source, implementations: implementations)
  }

  /// Returns the lowered form of the requirement implementation `d` in `useScope`.
  private mutating func emitRequirementImplementation(
    _ d: AnyDeclID, in useScope: AnyScopeID
  ) -> LoweredConformance.Implementation {
    switch d.kind {
    case FunctionDecl.self:
      let f = program[FunctionDecl.ID(d)!]
      let r = FunctionReference(to: f, usedIn: useScope, in: &module)
      return .function(r)

    case InitializerDecl.self:
      let f = program[InitializerDecl.ID(d)!]
      let r = FunctionReference(to: f, usedIn: useScope, in: &module)
      return .function(r)

    default:
      fatalError("not implemented")
    }
  }

  /// Inserts the IR for the top-level declaration `d`.
  @discardableResult
  private mutating func emit(synthesizedDecl d: SynthesizedDecl) -> Function.ID {
    switch d.kind {
    case .moveInitialization:
      return synthesizeMoveInitImplementation(typed: .init(d.type)!, in: d.scope)
    case .moveAssignment:
      return synthesizeMoveAssignImplementation(typed: .init(d.type)!, in: d.scope)
    case .copy:
      fatalError("not implemented")
    }
  }

  /// Synthesize the implementation of `t`'s move initialization operator in `scope`.
  private mutating func synthesizeMoveInitImplementation(
    typed t: LambdaType, in scope: AnyScopeID
  ) -> Function.ID {
    let f = Function.ID(synthesized: program.ast.moveRequirement(.set), for: ^t)
    module.declareSyntheticFunction(f, typed: t)
    if (module[f].entry != nil) || (program.module(containing: scope) != module.syntax.id) {
      return f
    }

    let site = module.syntax.site
    let entry = module.appendEntry(in: scope, to: f)
    insertionBlock = entry
    self.frames.push()
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    let receiver = Operand.parameter(entry, 0)
    let argument = Operand.parameter(entry, 1)

    switch program.relations.canonical(t.output).base {
    case is ProductType, is TupleType:
      let layout = AbstractTypeLayout(of: module.type(of: receiver).ast, definedIn: program)

      // Emit a load/store of the argument if it doesn't have any stored property.
      if layout.properties.isEmpty {
        let v = append(module.makeLoad(argument, at: site))[0]
        append(module.makeStore(v, at: receiver, at: site))
        break
      }

      // Move initialize each property.
      for (i, p) in layout.properties.enumerated() {
        let source = append(module.makeElementAddr(argument, at: [i], at: site))[0]
        let part = append(module.makeLoad(source, at: site))[0]
        let target = append(module.makeElementAddr(receiver, at: [i], at: site))[0]

        if p.type.base is BuiltinType {
          append(module.makeStore(part, at: target, at: site))
        } else {
          let c = program.conformance(of: p.type, to: program.ast.movableTrait, exposedTo: scope)!
          emitMove(
            .set, of: part, to: target, withConformanceToMovable: c,
            at: site)
        }
      }

    default:
      fatalError("not implemented")
    }

    store(returnValue: .void, at: site)
    emitStackDeallocs(site: site)
    append(module.makeReturn(at: site))
    return f
  }

  /// Synthesize the implementation of `t`'s move assignment operator in `scope`.
  private mutating func synthesizeMoveAssignImplementation(
    typed t: LambdaType, in scope: AnyScopeID
  ) -> Function.ID {
    let f = Function.ID(synthesized: program.ast.moveRequirement(.inout), for: ^t)
    module.declareSyntheticFunction(f, typed: t)
    if (module[f].entry != nil) || (program.module(containing: scope) != module.syntax.id) {
      return f
    }

    let site = module.syntax.site
    let entry = module.appendEntry(in: scope, to: f)
    insertionBlock = entry
    self.frames.push()
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    let receiver = Operand.parameter(entry, 0)
    let argument = Operand.parameter(entry, 1)

    // Deinitialize the receiver.
    append(module.makeDeinit(receiver, at: site))

    // Apply the move-initializer.
    let c = program.conformance(
      of: module.type(of: receiver).ast, to: program.ast.movableTrait, exposedTo: scope)!
    let r = append(module.makeLoad(argument, at: site))[0]
    emitMove(.set, of: r, to: receiver, withConformanceToMovable: c, at: site)

    store(returnValue: .void, at: site)
    emitStackDeallocs(site: site)
    append(module.makeReturn(at: site))
    return f
  }

  // MARK: Statements

  /// Inserts the IR for `stmt`.
  private mutating func emit<ID: StmtID>(stmt: ID.TypedNode) {
    switch stmt.kind {
    case AssignStmt.self:
      emit(assignStmt: AssignStmt.Typed(stmt)!)
    case BraceStmt.self:
      emit(braceStmt: BraceStmt.Typed(stmt)!)
    case ConditionalStmt.self:
      emit(conditionalStmt: ConditionalStmt.Typed(stmt)!)
    case DeclStmt.self:
      emit(declStmt: DeclStmt.Typed(stmt)!)
    case DiscardStmt.self:
      emit(discardStmt: DiscardStmt.Typed(stmt)!)
    case DoWhileStmt.self:
      emit(doWhileStmt: DoWhileStmt.Typed(stmt)!)
    case ExprStmt.self:
      emit(exprStmt: ExprStmt.Typed(stmt)!)
    case ReturnStmt.self:
      emit(returnStmt: ReturnStmt.Typed(stmt)!)
    case WhileStmt.self:
      emit(whileStmt: WhileStmt.Typed(stmt)!)
    case YieldStmt.self:
      emit(yieldStmt: YieldStmt.Typed(stmt)!)
    default:
      unexpected(stmt)
    }
  }

  private mutating func emit(assignStmt stmt: AssignStmt.Typed) {
    // The left operand of an assignment should always be marked for mutation, even if the
    // statement actually denotes initialization.
    guard stmt.left.kind == InoutExpr.self else {
      report(.error(assignmentLHSRequiresMutationMarkerAt: .empty(at: stmt.left.site.first())))
      return
    }

    // The RHS is evaluated before the LHS.
    let rhs = emitRValue(stmt.right)
    let lhs = emitLValue(stmt.left)

    // Built-in types do not require deinitialization.
    let l = program.relations.canonical(stmt.left.type)
    if l.base is BuiltinType {
      emitInitialization(of: lhs, to: rhs, at: stmt.site)
      return
    }

    let c = program.conformance(of: l, to: program.ast.movableTrait, exposedTo: insertionScope!)!
    append(module.makeMove(rhs, to: lhs, usingConformance: c, at: stmt.site))
  }

  private mutating func emit(braceStmt stmt: BraceStmt.Typed) {
    frames.push()
    for s in stmt.stmts {
      emit(stmt: s)
    }
    emitStackDeallocs(site: stmt.site)
    frames.pop()
  }

  private mutating func emit(conditionalStmt stmt: ConditionalStmt.Typed) {
    let (firstBranch, secondBranch) = emitTest(condition: stmt.condition, in: AnyScopeID(stmt.id))
    let tail: Block.ID

    insertionBlock = firstBranch
    emit(braceStmt: stmt.success)
    if let s = stmt.failure {
      tail = module.appendBlock(in: module[firstBranch].scope, to: firstBranch.function)
      append(module.makeBranch(to: tail, at: stmt.site))
      insertionBlock = secondBranch
      emit(stmt: s)
    } else {
      tail = secondBranch
    }

    append(module.makeBranch(to: tail, at: stmt.site))
    insertionBlock = tail
  }

  private mutating func emit(declStmt stmt: DeclStmt.Typed) {
    switch stmt.decl.kind {
    case BindingDecl.self:
      emit(localBindingDecl: BindingDecl.Typed(stmt.decl)!)
    default:
      unexpected(stmt.decl)
    }
  }

  private mutating func emit(discardStmt stmt: DiscardStmt.Typed) {
    let v = emitRValue(stmt.expr)
    append(module.makeDeinit(v, at: stmt.site))
  }

  private mutating func emit(doWhileStmt stmt: DoWhileStmt.Typed) {
    let loopBody = module.appendBlock(in: stmt.body.id, to: insertionBlock!.function)
    let loopTail = module.appendBlock(in: stmt.body.id, to: insertionBlock!.function)
    append(module.makeBranch(to: loopBody, at: .empty(at: stmt.site.first())))
    insertionBlock = loopBody

    // Note: we're not using `emit(braceStmt:into:)` because we need to evaluate the loop
    // condition before exiting the scope.
    frames.push()
    for s in stmt.body.stmts {
      emit(stmt: s)
    }

    let c = emitBranchCondition(stmt.condition)
    emitStackDeallocs(site: stmt.site)
    frames.pop()

    append(module.makeCondBranch(if: c, then: loopBody, else: loopTail, at: stmt.condition.site))
    insertionBlock = loopTail
  }

  private mutating func emit(exprStmt stmt: ExprStmt.Typed) {
    _ = emitRValue(stmt.expr)
  }

  private mutating func emit(returnStmt stmt: ReturnStmt.Typed) {
    let value: Operand
    if let expr = stmt.value {
      value = emitRValue(expr)
    } else {
      value = .void
    }

    store(returnValue: value, at: stmt.site)
    emitStackDeallocs(site: stmt.site)
    append(module.makeReturn(at: stmt.site))
  }

  private mutating func emit(whileStmt stmt: WhileStmt.Typed) {
    // Enter the loop.
    let head = module.appendBlock(in: stmt.id, to: insertionBlock!.function)
    append(module.makeBranch(to: head, at: .empty(at: stmt.site.first())))

    // Test the conditions.
    insertionBlock = head
    let (body, exit) = emitTest(condition: stmt.condition, in: AnyScopeID(stmt.id))

    // Execute the body.
    insertionBlock = body
    emit(braceStmt: stmt.body)
    append(module.makeBranch(to: head, at: .empty(at: stmt.site.first())))

    // Exit.
    insertionBlock = exit
  }

  private mutating func emit(yieldStmt stmt: YieldStmt.Typed) {
    // TODO: Read mutability of current subscript

    let s = emitLValue(program[stmt.value])
    let b = append(module.makeBorrow(.let, from: s, at: stmt.site))[0]
    append(module.makeYield(.let, b, at: stmt.site))
  }

  private mutating func store(returnValue v: Operand, at site: SourceRange) {
    let entry = module.entry(of: insertionBlock!.function)!
    let x0 = Operand.parameter(entry, module[entry].inputs.count - 1)
    let x1 = append(module.makeBorrow(.set, from: x0, at: site))[0]
    append(module.makeStore(v, at: x1, at: site))
  }

  // MARK: r-values

  /// Inserts the IR for the rvalue `expr`.
  private mutating func emitRValue<ID: ExprID>(_ expr: ID.TypedNode) -> Operand {
    defer {
      // Mark the execution path unreachable if the computed value has type `Never`.
      if program.relations.areEquivalent(expr.type, .never) {
        emitStackDeallocs(site: expr.site)
        append(module.makeUnreachable(at: expr.site))
      }
    }

    switch expr.kind {
    case BooleanLiteralExpr.self:
      return emitRValue(booleanLiteral: BooleanLiteralExpr.Typed(expr)!)
    case CastExpr.self:
      return emitRValue(cast: CastExpr.Typed(expr)!)
    case ConditionalExpr.self:
      return emitRValue(conditional: ConditionalExpr.Typed(expr)!)
    case FloatLiteralExpr.self:
      return emitRValue(floatLiteral: FloatLiteralExpr.Typed(expr)!)
    case FunctionCallExpr.self:
      return emitRValue(functionCall: FunctionCallExpr.Typed(expr)!)
    case IntegerLiteralExpr.self:
      return emitRValue(integerLiteral: IntegerLiteralExpr.Typed(expr)!)
    case LambdaExpr.self:
      return emitRValue(lambda: LambdaExpr.Typed(expr)!)
    case NameExpr.self:
      return emitRValue(name: NameExpr.Typed(expr)!)
    case PragmaLiteralExpr.self:
      return emitRValue(pragma: PragmaLiteralExpr.Typed(expr)!)
    case SequenceExpr.self:
      return emitRValue(sequence: SequenceExpr.Typed(expr)!)
    case StringLiteralExpr.self:
      return emitRValue(stringLiteral: StringLiteralExpr.Typed(expr)!)
    case TupleExpr.self:
      return emitRValue(tuple: TupleExpr.Typed(expr)!)
    case TupleMemberExpr.self:
      return emitRValue(tuple: TupleMemberExpr.Typed(expr)!)
    default:
      unexpected(expr)
    }
  }

  private mutating func emitRValue(booleanLiteral expr: BooleanLiteralExpr.Typed) -> Operand {
    let bool = program.ast.coreType("Bool")!
    let s = module.makeRecord(bool, aggregating: [.i1(expr.value)], at: expr.site)
    return append(s)[0]
  }

  private mutating func emitRValue(cast expr: CastExpr.Typed) -> Operand {
    switch expr.direction {
    case .up:
      return emitRValue(upcast: expr)
    case .down:
      return emitRValue(downcast: expr)
    default:
      fatalError("not implemented")
    }
  }

  private mutating func emitRValue(upcast expr: CastExpr.Typed) -> Operand {
    precondition(expr.direction == .up)

    let lhs = emitRValue(expr.left)
    let rhs = MetatypeType(expr.right.type)!.instance

    // Nothing to do if the LHS already has the desired type.
    if program.relations.areEquivalent(expr.left.type, rhs) {
      return lhs
    }

    fatalError("not implemented")
  }

  private mutating func emitRValue(downcast expr: CastExpr.Typed) -> Operand {
    precondition(expr.direction == .down)

    let lhs = emitRValue(expr.left)
    let rhs = MetatypeType(expr.right.type)!.instance

    // Nothing to do if the LHS already has the desired type.
    if program.relations.areEquivalent(expr.left.type, rhs) {
      return lhs
    }

    if expr.left.type.base is ExistentialType {
      let x = append(module.makeOpen(lhs, as: rhs, at: expr.site))
      emitGuard(x[1], at: expr.site)
      return append(module.makeLoad(x[0], at: expr.site))[0]
    }

    fatalError("not implemented")
  }

  private mutating func emitRValue(conditional expr: ConditionalExpr.Typed) -> Operand {
    // If the expression is supposed to return a value, allocate storage for it.
    var resultStorage: Operand?
    if expr.type != .void {
      resultStorage = emitAllocStack(for: expr.type, at: expr.site)
    }

    let (firstBranch, secondBranch) = emitTest(condition: expr.condition, in: AnyScopeID(expr.id))
    let tail = module.appendBlock(in: insertionScope!, to: insertionBlock!.function)

    // Emit the success branch.
    insertionBlock = firstBranch
    frames.push()
    if let s = resultStorage {
      emitInitialization(of: s, to: expr.success)
    } else {
      _ = emitRValue(program[expr.success])
    }
    emitStackDeallocs(site: expr.site)
    frames.pop()
    append(module.makeBranch(to: tail, at: expr.site))

    // Emit the failure branch.
    insertionBlock = secondBranch
    let i = frames.top.allocs.count
    if let s = resultStorage {
      emitInitialization(of: s, to: expr.failure)
    } else {
      _ = emitRValue(program[expr.failure])
    }
    for a in frames.top.allocs[i...] {
      append(module.makeDeallocStack(for: a, at: expr.site))
    }
    frames.top.allocs.removeSubrange(i...)

    append(module.makeBranch(to: tail, at: expr.site))

    // Emit the value of the expression.
    insertionBlock = tail
    if let s = resultStorage {
      return append(module.makeLoad(s, at: expr.site))[0]
    } else {
      return .void
    }
  }

  private mutating func emitRValue(floatLiteral expr: FloatLiteralExpr.Typed) -> Operand {
    emitNumericLiteral(expr.value, typed: program.relations.canonical(expr.type), at: expr.site)
  }

  private mutating func emitRValue(functionCall expr: FunctionCallExpr.Typed) -> Operand {
    let s = emitLValue(functionCall: expr)
    return append(module.makeLoad(s, at: expr.site))[0]
  }

  private mutating func emitLValue(functionCall expr: FunctionCallExpr.Typed) -> Operand {
    let result = emitAllocStack(for: expr.type, at: expr.site)

    // Handle built-ins and constructor calls.
    if let n = NameExpr.Typed(expr.callee) {
      switch n.declaration {
      case .builtinFunction(let f):
        let x0 = emit(apply: f, to: expr.arguments, at: expr.site)
        let x1 = append(module.makeBorrow(.set, from: result, at: expr.site))[0]
        append(module.makeStore(x0, at: x1, at: expr.site))
        return result

      case .constructor:
        emitInitializerCall(expr, initializing: result)
        return result

      default:
        break
      }
    }

    // Arguments are evaluated first, from left to right.
    let syntheticSite = expr.site.file.emptyRange(at: expr.site.end)
    let arguments = emit(
      arguments: expr.arguments, to: expr.callee,
      synthesizingDefaultArgumentsAt: syntheticSite)

    // Callee and captures are evaluated next.
    let (callee, captures) = emitCallee(expr.callee)

    // Call is evaluated last.
    let x1 = append(module.makeBorrow(.set, from: result, at: expr.site))[0]
    append(
      module.makeCall(
        applying: callee, to: captures + arguments, writingResultTo: x1, at: expr.site))
    return result
  }

  /// Inserts the IR for given constructor `call`, which initializes storage `r` by applying
  /// initializer `d` parameterized by `a`.
  ///
  /// - Parameters:
  ///   - call: The syntax of the call.
  ///   - s: The address of uninitialized storage typed by the receiver of `call`. This storage is
  ///     borrowed for initialization after evaluating `call`'s arguments and before the call.
  private mutating func emitInitializerCall(
    _ call: FunctionCallExpr.Typed, initializing s: Operand
  ) {
    let callee = NameExpr.Typed(call.callee)!
    guard case .constructor(let d, let a) = callee.declaration else { preconditionFailure() }

    // Handle memberwise constructor calls.
    if program.ast[d].isMemberwise {
      emitMemberwiseInitializerCall(call, initializing: s)
      return
    }

    // Arguments are evaluated first, from left to right.
    let syntheticSite = call.site.file.emptyRange(at: call.site.end)
    let arguments = emit(
      arguments: call.arguments, to: call.callee,
      synthesizingDefaultArgumentsAt: syntheticSite)

    // Receiver is captured next.
    let receiver = append(module.makeBorrow(.set, from: s, at: call.site))[0]

    // Call is evaluated last.
    let f = FunctionReference(
      to: program[d], parameterizedBy: a,
      usedIn: insertionScope!, in: &module)

    let x0 = emitAllocStack(for: .void, at: call.site)
    let x1 = append(module.makeBorrow(.set, from: x0, at: call.site))[0]
    append(
      module.makeCall(
        applying: .constant(f), to: [receiver] + arguments, writingResultTo: x1, at: call.site))
  }

  /// Inserts the IR for given constructor `call`, which initializes storage `r` by applying
  /// memberwise initializer `d`.
  ///
  /// - Parameters:
  ///   - call: The syntax of the call.
  ///   - s: The address of uninitialized storage typed by the receiver of `d`. This storage is
  ///     borrowed for initialization after evaluating `call`'s arguments and before calling `d`.
  ///   - d: The initializer referenced by `call`'s callee.
  private mutating func emitMemberwiseInitializerCall(
    _ call: FunctionCallExpr.Typed,
    initializing s: Operand
  ) {
    let callee = LambdaType(call.callee.type)!
    var arguments: [Operand] = []
    for (p, e) in zip(callee.inputs, call.arguments) {
      let a = program[e.value]
      arguments.append(emit(argument: a, to: ParameterType(p.type)!))
    }

    let x0 = append(
      module.makeRecord(callee.output, aggregating: arguments, at: call.site))[0]
    let x1 = append(module.makeBorrow(.set, from: s, at: call.site))[0]
    append(module.makeStore(x0, at: x1, at: call.site))
  }

  /// Inserts the IR for `arguments`, which is an argument passed to a function of type `callee`.
  ///
  /// - Parameters:
  ///   - syntheticSite: The site at which default pragma arguments are anchored.
  private mutating func emit(
    arguments: [LabeledArgument],
    to callee: AnyExprID.TypedNode,
    synthesizingDefaultArgumentsAt syntheticSite: SourceRange
  ) -> [Operand] {
    let calleeType = LambdaType(callee.type)!
    let calleeDecl = NameExpr.Typed(callee)?.declaration.decl
    let defaults = calleeDecl.flatMap(program.ast.defaultArguments(of:))

    var result: [Operand] = []
    var i = 0
    for (j, p) in calleeType.inputs.enumerated() {
      let a: AnyExprID
      if (i < arguments.count) && (arguments[i].label?.value == p.label) {
        a = arguments[i].value
        i += 1
      } else if let e = defaults?[j] {
        a = e
      } else {
        unreachable()
      }

      let v = emit(argument: program[a], to: ParameterType(p.type)!, at: syntheticSite)
      result.append(v)
    }

    assert(i == arguments.count)
    return result
  }

  /// Inserts the IR for the argument `expr` passed to a parameter of type `parameter`.
  ///
  /// - Parameters:
  ///   - site: The source range in which `syntax` is being evaluated if it's a pragma literals.
  ///     Defaults to `syntax.site`.
  private mutating func emit(
    argument syntax: AnyExprID.TypedNode,
    to parameter: ParameterType,
    at site: SourceRange? = nil
  ) -> Operand {
    if let e = PragmaLiteralExpr.Typed(syntax) {
      let anchor = site ?? syntax.site
      let v = emitRValue(pragma: e, at: anchor)

      switch parameter.access {
      case .let, .inout, .set:
        let s = emitLValue(converting: v, at: anchor)
        let b = module.makeBorrow(parameter.access, from: s, at: anchor)
        return append(b)[0]

      case .sink:
        return v

      default:
        fatalError("not implemented")
      }
    }

    switch parameter.access {
    case .let, .inout, .set:
      let s = emitLValue(syntax)
      let b = module.makeBorrow(parameter.access, from: s, at: syntax.site)
      return append(b)[0]

    case .sink:
      return emitRValue(syntax)

    default:
      fatalError("not implemented")
    }
  }

  /// Emits the IR of a call to `f` with given `arguments` at `site`.
  private mutating func emit(
    apply f: BuiltinFunction,
    to arguments: [LabeledArgument],
    at site: SourceRange
  ) -> Operand {
    switch f.name {
    case .llvm(let n):
      var a: [Operand] = []
      for e in arguments {
        a.append(emitRValue(program[e.value]))
      }
      return append(
        module.makeLLVM(applying: n, to: a, at: site))[0]

    case .addressOf:
      let source = emitLValue(program[arguments[0].value])
      return append(
        module.makeAddressToPointer(source, at: site))[0]
    }
  }

  private mutating func emitRValue(integerLiteral expr: IntegerLiteralExpr.Typed) -> Operand {
    emitNumericLiteral(expr.value, typed: program.relations.canonical(expr.type), at: expr.site)
  }

  private mutating func emitRValue(lambda e: LambdaExpr.Typed) -> Operand {
    _ = emit(functionDecl: e.decl)
    let f = FunctionReference(to: program[e.decl], usedIn: insertionScope!, in: &module)
    return append(module.makePartialApply(wrapping: f, with: .void, at: e.site))[0]
  }

  private mutating func emitRValue(name e: NameExpr.Typed) -> Operand {
    switch e.declaration {
    case .direct(let d, _):
      guard let s = frames[program[d]] else { fatalError("not implemented") }
      if module.type(of: s).isObject {
        return s
      } else {
        return append(module.makeLoad(s, at: e.site))[0]
      }

    case .member(let d, _):
      return emitRValue(memberExpr: e, declaredBy: program[d])

    case .constructor:
      fatalError("not implemented")

    case .builtinFunction:
      fatalError("not implemented")

    case .builtinType:
      fatalError("not implemented")
    }
  }

  private mutating func emitRValue(
    memberExpr e: NameExpr.Typed, declaredBy d: AnyDeclID.TypedNode
  ) -> Operand {
    switch d.kind {
    case VarDecl.self:
      return emitRValue(memberBinding: e, declaredBy: .init(d)!)
    default:
      fatalError("not implemented")
    }
  }

  /// Emits the IR for consuming a stored binding `e` at the end of `self.insertionBlock`.
  ///
  /// - Requires: `m.decl` is a `VarDecl`.
  private mutating func emitRValue(
    memberBinding e: NameExpr.Typed, declaredBy d: VarDecl.Typed
  ) -> Operand {
    // Member reference without a domain expression are implicitly bound to `self`.
    let base: Operand
    if let p = e.domainExpr {
      base = emitLValue(p)
    } else {
      base = frames[receiver!]!
    }

    let l = AbstractTypeLayout(of: module.type(of: base).ast, definedIn: program)
    let i = l.offset(of: d.baseName)!

    let x0 = append(module.makeElementAddr(base, at: [i], at: e.site))[0]
    let x1 = append(module.makeLoad(x0, at: e.site))[0]
    return x1
  }

  /// Inserts the IR of `syntax`.
  ///
  /// - Parameters:
  ///   - site: The source range in which `syntax` is being evaluated. Defaults to `syntax.site`.
  private mutating func emitRValue(
    pragma syntax: PragmaLiteralExpr.Typed,
    at site: SourceRange? = nil
  ) -> Operand {
    let s = site ?? syntax.site
    switch program.ast[syntax.id].kind {
    case .file:
      return emitString(s.file.url.absoluteURL.path, at: s)
    case .line:
      return emitInt(s.first().line.number, at: s)
    }
  }

  private mutating func emitRValue(sequence expr: SequenceExpr.Typed) -> Operand {
    let s = emitLValue(sequence: expr)
    return append(module.makeLoad(s, at: expr.site))[0]
  }

  private mutating func emitRValue(stringLiteral syntax: StringLiteralExpr.Typed) -> Operand {
    emitString(syntax.value, at: syntax.site)
  }

  private mutating func emitRValue(tuple syntax: TupleExpr.Typed) -> Operand {
    if syntax.elements.isEmpty { return .void }

    var elements: [Operand] = []
    for e in syntax.elements {
      elements.append(emitRValue(program[e.value]))
    }
    return append(module.makeRecord(syntax.type, aggregating: elements, at: syntax.site))[0]
  }

  private mutating func emitRValue(tuple syntax: TupleMemberExpr.Typed) -> Operand {
    let base = emitLValue(syntax.tuple)
    let element = append(
      module.makeElementAddr(base, at: [syntax.index.value], at: syntax.index.site))[0]
    return append(module.makeLoad(element, at: syntax.index.site))[0]
  }

  /// Inserts the IR for given `callee` and returns `(c, a)`, where `c` is the callee's value and
  /// `a` are arguments to lifted parameters.
  ///
  /// Lifted arguments are produced if `callee` is a reference to a function with captures, such as
  /// a bound member function or a local function declaration with a non-empty environment.
  ///
  /// - Requires: `callee` has a lambda type.
  private mutating func emitCallee(
    _ callee: AnyExprID.TypedNode
  ) -> (callee: Operand, captures: [Operand]) {
    switch callee.kind {
    case NameExpr.self:
      return emitNamedCallee(.init(callee)!)

    case InoutExpr.self:
      // TODO: Handle the mutation marker, somehow.
      return emitCallee(InoutExpr.Typed(callee)!.subject)

    default:
      let f = emitLambdaCallee(callee)
      return (f, [])
    }
  }

  /// Inserts the IR for given `callee` and returns `(c, a)`, where `c` is the callee's value and
  /// `a` are arguments to lifted parameters.
  ///
  /// - Requires: `callee` has a lambda type.
  private mutating func emitNamedCallee(
    _ callee: NameExpr.Typed
  ) -> (callee: Operand, captures: [Operand]) {
    let calleeType = LambdaType(program.relations.canonical(callee.type))!

    switch callee.declaration {
    case .direct(let d, let a) where d.kind == FunctionDecl.self:
      // Callee is a direct reference to a function declaration.
      guard calleeType.environment == .void else {
        fatalError("not implemented")
      }

      let r = FunctionReference(
        to: program[FunctionDecl.ID(d)!], parameterizedBy: a,
        usedIn: insertionScope!, in: &module)
      return (.constant(r), [])

    case .member(let d, let a) where d.kind == FunctionDecl.self:
      // Callee is a member reference to a function or method.
      let r = FunctionReference(
        to: program[FunctionDecl.ID(d)!], parameterizedBy: a,
        usedIn: insertionScope!, in: &module)

      // Emit the location of the receiver.
      let receiver: Operand
      switch callee.domain {
      case .none:
        receiver = frames[self.receiver!]!
      case .expr(let e):
        receiver = emitLValue(e)
      case .implicit:
        unreachable()
      }

      // Load or borrow the receiver.
      if let t = RemoteType(calleeType.captures[0].type) {
        let i = append(module.makeBorrow(t.access, from: receiver, at: callee.site))
        return (Operand.constant(r), i)
      } else {
        let i = append(module.makeLoad(receiver, at: callee.site))
        return (Operand.constant(r), i)
      }

    case .builtinFunction, .builtinType:
      // Calls to built-ins should have been handled already.
      unreachable()

    default:
      // Callee is a lambda.
      let f = emitLambdaCallee(.init(callee))
      return (f, [])
    }
  }

  /// Inserts the IR for given `callee` and returns its value.
  ///
  /// - Requires: `callee` has a lambda type.
  private mutating func emitLambdaCallee(_ callee: AnyExprID.TypedNode) -> Operand {
    switch LambdaType(callee.type)!.receiverEffect {
    case .yielded:
      unreachable()

    case .set:
      fatalError("not implemented")

    case .sink:
      return emitRValue(callee)

    case let k:
      let l = emitLValue(callee)
      let b = module.makeBorrow(k, from: l, at: callee.site)
      return append(b)[0]
    }
  }

  /// Returns `(success: a, failure: b)` where `a` is the basic block reached if all items in
  /// `condition` hold and `b` is the basic block reached otherwise, creating new basic blocks
  /// in `scope`.
  private mutating func emitTest(
    condition: [ConditionItem], in scope: AnyScopeID
  ) -> (success: Block.ID, failure: Block.ID) {
    let f = insertionBlock!.function
    let failure = module.appendBlock(in: scope, to: f)

    for item in condition {
      switch item {
      case .expr(let e):
        let condition = program[e]
        let test = pushing(Frame(), { $0.emitBranchCondition(condition) })
        let next = module.appendBlock(in: scope, to: f)
        append(module.makeCondBranch(if: test, then: next, else: failure, at: program[e].site))
        insertionBlock = next

      case .decl:
        fatalError("not implemented")
      }
    }

    return (success: insertionBlock!, failure: failure)
  }

  /// Inserts the IR for string `s`, anchoring instructions at site.
  private mutating func emitString(_ s: String, at site: SourceRange) -> Operand {
    var bytes = s.unescaped.data(using: .utf8)!
    let size = emitWord(bytes.count, at: site)
    bytes.append(contentsOf: [0])

    let p = PointerConstant(module.syntax.id, module.addGlobal(BufferConstant(bytes)))
    let base = emitCoreInstance(of: "RawPointer", aggregating: [.constant(p)], at: site)
    return emitCoreInstance(of: "String", aggregating: [size, base], at: site)
  }

  /// Inserts the IR for integer `i` anchoring instructions at site.
  private mutating func emitInt(_ i: Int, at site: SourceRange) -> Operand {
    let t = program.ast.coreType("Int")!
    let s = module.makeRecord(
      t, aggregating: [.constant(IntegerConstant(i, bitWidth: 64))], at: site)
    return append(s)[0]
  }

  /// Inserts the IR for branch condition `expr`.
  ///
  /// - Requires: `expr.type` is `Val.Bool`
  private mutating func emitBranchCondition(_ expr: AnyExprID.TypedNode) -> Operand {
    precondition(program.relations.canonical(expr.type) == program.ast.coreType("Bool")!)
    let x0 = emitLValue(expr)
    let x1 = append(module.makeElementAddr(x0, at: [0], at: expr.site))[0]
    let x2 = append(module.makeLoad(x1, at: expr.site))[0]
    return x2
  }

  /// Inserts the IR for numeric literal `s` with type `literalType`, anchoring new instructions
  /// at `site`.
  ///
  /// - Requires `literalType` must be one of the core numeric types.
  private mutating func emitNumericLiteral(
    _ s: String, typed literalType: AnyType, at site: SourceRange
  ) -> Operand {
    switch literalType {
    case program.ast.coreType("Double")!:
      let v = FloatingPointConstant.double(s)
      return append(module.makeRecord(literalType, aggregating: [.constant(v)], at: site))[0]

    case program.ast.coreType("Float")!:
      let v = FloatingPointConstant.float(s)
      return append(module.makeRecord(literalType, aggregating: [.constant(v)], at: site))[0]

    default:
      return emitIntegerLiteral(s, typed: literalType, at: site)
    }
  }

  /// Inserts the IR for numeric literal `s` with type `literalType`, anchoring new instructions
  /// at `site`.
  ///
  /// - Requires `literalType` must be one of the core integer types.
  private mutating func emitIntegerLiteral(
    _ s: String, typed literalType: AnyType, at site: SourceRange
  ) -> Operand {
    let bits: WideUInt?
    switch literalType {
    case program.ast.coreType("Int")!:
      bits = .init(valLiteral: s, signed: true, bitWidth: 64)
    case program.ast.coreType("Int32")!:
      bits = .init(valLiteral: s, signed: true, bitWidth: 32)
    case program.ast.coreType("Int8")!:
      bits = .init(valLiteral: s, signed: true, bitWidth: 8)
    default:
      unreachable("unexpected numeric type")
    }

    guard let b = bits else {
      diagnostics.insert(
        .error(integerLiteral: s, overflowsWhenStoredInto: literalType, at: site))
      return .constant(Poison(type: .object(literalType)))
    }

    return append(
      module.makeRecord(
        literalType, aggregating: [.constant(IntegerConstant(b))], at: site))[0]
  }

  /// Inserts the IR for the construction of an instance of a core type named `n`with given
  /// `value`, anchoring instructions at `site`.
  ///
  /// - precondition: `type` is a core type.
  private mutating func emitCoreInstance(
    of n: String,
    aggregating parts: [Operand],
    at site: SourceRange
  ) -> Operand {
    let t = program.ast.coreType(n)!
    let o = module.makeRecord(t, aggregating: parts, at: site)
    return append(o)[0]
  }

  /// Inserts the IR for the construction of a a machine word with given `value`, anchoring
  /// instructions at `site`.
  private mutating func emitWord(_ value: Int, at site: SourceRange) -> Operand {
    emitCoreInstance(
      of: "Int",
      aggregating: [.constant(IntegerConstant(value, bitWidth: 64))],
      at: site)
  }

  /// Inserts the IR for converting `foreign` to a value of type `ir`.
  private mutating func emitConvert(
    foreign: Operand,
    to ir: AnyType,
    at site: SourceRange
  ) -> Operand {
    precondition(module.type(of: foreign).isObject)

    let foreignConvertible = program.ast.coreTrait("ForeignConvertible")!
    let foreignConvertibleConformance = program.conformance(
      of: ir, to: foreignConvertible, exposedTo: insertionScope!)!
    let r = program.ast.requirements(
      Name(stem: "init", labels: ["foreign_value"]), in: foreignConvertible.decl)[0]

    switch foreignConvertibleConformance.implementations[r]! {
    case .concrete(let m):
      let convert = FunctionReference(
        to: program[InitializerDecl.ID(m)!],
        usedIn: insertionScope!, in: &module)
      let t = LambdaType(convert.type.ast)!.output

      let x0 = emitAllocStack(for: ir, at: site)
      let x1 = append(module.makeBorrow(.set, from: x0, at: site))[0]
      let x2 = emitAllocStack(for: t, at: site)
      let x3 = append(module.makeBorrow(.set, from: x2, at: site))[0]
      append(
        module.makeCall(
          applying: .constant(convert), to: [x1, foreign], writingResultTo: x3, at: site))
      return append(module.makeLoad(x0, at: site))[0]

    case .synthetic:
      fatalError("not implemented")
    }
  }

  /// Appends the IR to convert `o` to a FFI argument.
  private mutating func emitConvertToForeign(_ o: Operand, at site: SourceRange) -> Operand {
    let t = module.type(of: o)
    precondition(t.isAddress)

    let foreignConvertible = program.ast.coreTrait("ForeignConvertible")!
    let foreignConvertibleConformance = program.conformance(
      of: t.ast, to: foreignConvertible, exposedTo: insertionScope!)!
    let r = program.ast.requirements("foreign_value", in: foreignConvertible.decl)[0]

    // TODO: Handle cases where the foreign representation of `t` is not built-in

    switch foreignConvertibleConformance.implementations[r]! {
    case .concrete(let m):
      let convert = FunctionReference(
        to: program[FunctionDecl.ID(m)!],
        usedIn: insertionScope!, in: &module)
      let t = LambdaType(convert.type.ast)!.output

      let x0 = append(module.makeBorrow(.let, from: o, at: site))
      let x1 = emitAllocStack(for: t, at: site)
      let x2 = append(module.makeBorrow(.set, from: x1, at: site))[0]
      append(module.makeCall(applying: .constant(convert), to: x0, writingResultTo: x2, at: site))
      return append(module.makeLoad(x1, at: site))[0]

    case .synthetic:
      fatalError("not implemented")
    }
  }

  // MARK: l-values

  /// Inserts the IR for the lvalue `syntax` block.
  private mutating func emitLValue(_ syntax: AnyExprID.TypedNode) -> Operand {
    switch syntax.kind {
    case CastExpr.self:
      return emitLValue(cast: .init(syntax)!)
    case FunctionCallExpr.self:
      return emitLValue(functionCall: .init(syntax)!)
    case InoutExpr.self:
      return emitLValue(inoutExpr: .init(syntax)!)
    case NameExpr.self:
      return emitLValue(name: .init(syntax)!)
    case SequenceExpr.self:
      return emitLValue(sequence: .init(syntax)!)
    case TupleMemberExpr.self:
      return emitLValue(tupleMember: .init(syntax)!)
    default:
      return emitLValue(converting: syntax)
    }
  }

  /// Inserts the IR for `rvalue` converted as a lvalue.
  private mutating func emitLValue(converting rvalue: AnyExprID.TypedNode) -> Operand {
    let t = program.relations.canonical(rvalue.type)
    let s = emitAllocStack(for: t, at: rvalue.site)
    emitInitialization(of: s, to: rvalue.id)
    return s
  }

  /// Inserts the IR for converting `rvalue` as a lvalue at `site`.
  private mutating func emitLValue(converting rvalue: Operand, at site: SourceRange) -> Operand {
    let t = module.type(of: rvalue).ast
    let s = emitAllocStack(for: t, at: site)
    emitInitialization(of: s, to: rvalue, at: site)
    return s
  }

  private mutating func emitLValue(cast syntax: CastExpr.Typed) -> Operand {
    switch syntax.direction {
    case .pointerConversion:
      let source = emitRValue(syntax.left)
      let target = RemoteType(program.relations.canonical(syntax.type))!
      return append(module.makePointerToAddress(source, to: target, at: syntax.site))[0]

    default:
      fatalError("not implemented")
    }
  }

  private mutating func emitLValue(inoutExpr syntax: InoutExpr.Typed) -> Operand {
    return emitLValue(syntax.subject)
  }

  private mutating func emitLValue(name syntax: NameExpr.Typed) -> Operand {
    switch syntax.declaration {
    case .direct(let d, _):
      return emitLValue(directReferenceTo: program[d])

    case .member(let d, _):
      let r = emitLValue(receiverOf: syntax)
      return emitProperty(boundTo: r, declaredBy: program[d], at: syntax.site)

    case .constructor:
      fatalError()

    case .builtinFunction, .builtinType:
      // Built-in functions and types are never used as l-value.
      unreachable()
    }
  }

  /// Inserts the IR denoting a direct reference to `d` at the end of the current insertion block.
  private mutating func emitLValue(directReferenceTo d: AnyDeclID.TypedNode) -> Operand {
    // Check if `d` is a local.
    if let s = frames[d] { return s }

    switch d.kind {
    case ProductTypeDecl.self:
      let t = MetatypeType(of: d.type)
      let g = module.addGlobal(MetatypeConstant(MetatypeType(d.type)!))
      let s = module.makeGlobalAddr(
        of: g, in: module.syntax.id, typed: ^MetatypeType(of: t), at: d.site)
      return append(s)[0]

    default:
      fatalError("not implemented")
    }
  }

  /// Inserts the IR denoting the domain of `syntax`.
  private mutating func emitLValue(receiverOf syntax: NameExpr.Typed) -> Operand {
    switch syntax.domain {
    case .none:
      return frames[receiver!]!
    case .implicit:
      fatalError("not implemented")
    case .expr(let e):
      return emitLValue(e)
    }
  }

  private mutating func emitLValue(sequence syntax: SequenceExpr.Typed) -> Operand {
    emitLValue(foldedSequence: syntax.folded)
  }

  private mutating func emitLValue(foldedSequence syntax: FoldedSequenceExpr) -> Operand {
    switch syntax {
    case .infix(let callee, let lhs, let rhs):
      let t = program.exprTypes[callee.expr]!
      let calleeType = LambdaType(program.relations.canonical(t))!.lifted

      // Emit the operands, starting with RHS.
      let r = emitInfixOperand(rhs, passed: ParameterType(calleeType.inputs[1].type)!.access)
      let l = emitInfixOperand(lhs, passed: ParameterType(calleeType.inputs[0].type)!.access)

      // The callee must be a reference to member function.
      guard case .member(let d, _) = program.referredDecls[callee.expr] else { unreachable() }
      let oper = Operand.constant(
        FunctionReference(to: program[FunctionDecl.ID(d)!], usedIn: insertionScope!, in: &module))

      // Emit the call.
      let site = program.ast.site(of: syntax)
      let x0 = emitAllocStack(for: calleeType.output, at: site)
      let x1 = append(module.makeBorrow(.set, from: x0, at: site))[0]
      append(module.makeCall(applying: oper, to: [l, r], writingResultTo: x1, at: site))
      return x0

    case .leaf(let e):
      return emitLValue(program[e])
    }
  }

  private mutating func emitInfixOperand(
    _ syntax: FoldedSequenceExpr, passed access: AccessEffect
  ) -> Operand {
    let source = emitLValue(foldedSequence: syntax)
    let site = program.ast.site(of: syntax)

    switch access {
    case .let, .inout, .set:
      return append(module.makeBorrow(access, from: source, at: site))[0]
    case .sink:
      return append(module.makeLoad(source, at: site))[0]
    case .yielded:
      unreachable()
    }
  }

  private mutating func emitLValue(tupleMember syntax: TupleMemberExpr.Typed) -> Operand {
    let base = emitLValue(syntax.tuple)
    return append(module.makeElementAddr(base, at: [syntax.index.value], at: syntax.index.site))[0]
  }

  /// Returns the address of the member declared by `d` and bound to `receiver`, inserting IR
  /// anchored at `site`.
  private mutating func emitProperty(
    boundTo receiver: Operand,
    declaredBy d: AnyDeclID.TypedNode,
    at site: SourceRange
  ) -> Operand {
    switch d.kind {
    case SubscriptDecl.self:
      return emitComputedProperty(boundTo: receiver, declaredByBundle: .init(d)!, at: site)

    case VarDecl.self:
      let l = AbstractTypeLayout(of: module.type(of: receiver).ast, definedIn: program)
      let i = l.offset(of: VarDecl.Typed(d)!.baseName)!
      return append(module.makeElementAddr(receiver, at: [i], at: site))[0]

    default:
      fatalError("not implemented")
    }
  }

  /// Returns the address of the computed property declared by `d` and bound to `receiver`,
  /// inserting IR anchored at `site`.
  private mutating func emitComputedProperty(
    boundTo receiver: Operand,
    declaredByBundle d: SubscriptDecl.Typed,
    at site: SourceRange
  ) -> Operand {
    // TODO: Handle generics

    if let i = d.impls.uniqueElement {
      return emitComputedProperty(boundTo: receiver, declaredBy: i, at: site)
    }

    let t = SubscriptType(program.relations.canonical(d.type))!
    let r = append(module.makeAccess(t.capabilities, from: receiver, at: site))[0]

    var variants: [AccessEffect: Function.ID] = [:]
    for v in d.impls {
      variants[v.introducer.value] = module.demandSubscriptDeclaration(lowering: v)
    }

    return append(
      module.makeProjectBundle(applying: variants, of: d.id, typed: t, to: [r], at: site))[0]
  }

  /// Returns the address of the computed property declared by `d` and bound to `receiver`,
  /// inserting IR anchored at `site`.
  private mutating func emitComputedProperty(
    boundTo receiver: Operand,
    declaredBy d: SubscriptImpl.Typed,
    at site: SourceRange
  ) -> Operand {
    let t = SubscriptImplType(d.type)!
    let o = RemoteType(d.introducer.value, program.relations.canonical(t.output))
    let r = append(module.makeBorrow(o.access, from: receiver, at: site))[0]
    let f = module.demandSubscriptDeclaration(lowering: d)
    return append(module.makeProject(o, applying: f, to: [r], at: site))[0]
  }

  // MARK: Helpers

  /// Inserts a stack allocation for an object of type `t`.
  private mutating func emitAllocStack(
    for t: AnyType, at site: SourceRange
  ) -> Operand {
    let u = program.relations.canonical(t)
    let s = append(module.makeAllocStack(u, at: site))[0]
    frames.top.allocs.append(s)
    return s
  }

  /// Appends the IR for computing the address of the property at `path` rooted at `base`,
  /// anchoring new instructions at `site`.
  ///
  /// - Returns: The result of `element_addr base, path` instruction if `path` is not empty;
  ///   otherwise, returns `base` unchanged.
  private mutating func emitElementAddr(
    _ base: Operand, at path: PartPath, at site: SourceRange
  ) -> Operand {
    if path.isEmpty { return base }
    return append(module.makeElementAddr(base, at: path, at: site))[0]
  }

  /// Inserts the IR for initializing `storage` with `value` at the end of the current insertion
  /// block.
  private mutating func emitInitialization(of storage: Operand, to value: AnyExprID) {
    if let tuple = TupleExpr.ID(value) {
      emitInitialization(of: storage, to: tuple)
    } else if let call = FunctionCallExpr.ID(value),
      let n = NameExpr.ID(program.ast[call].callee),
      case .constructor = program.referredDecls[n]!
    {
      emitInitializerCall(program[call], initializing: storage)
    } else {
      let v = emitRValue(program[value])
      // TODO: Replace by emitMove
      emitInitialization(of: storage, to: v, at: program.ast[value].site)
    }
  }

  /// Inserts the IR for initializing `storage` with `value` at the end of the current insertion
  /// block.
  private mutating func emitInitialization(of storage: Operand, to value: TupleExpr.ID) {
    for (i, e) in program.ast[value].elements.enumerated() {
      let s = append(
        module.makeElementAddr(storage, at: [i], at: program.ast[e.value].site))[0]
      emitInitialization(of: s, to: e.value)
    }
  }

  /// Inserts the IR for initializing `storage` with `value` at the end of the current insertion
  /// block, anchoring new instructions at `site`.
  private mutating func emitInitialization(
    of storage: Operand, to value: Operand, at site: SourceRange
  ) {
    let s = append(module.makeBorrow(.set, from: storage, at: site))[0]
    append(module.makeStore(value, at: s, at: site))
  }

  /// Appends the IR for a call to move-initialize/assign `storage` with `value`, using `c` to
  /// identify the implementations of these operations and anchoring new instructions at `site`.
  ///
  /// Use pass `.set` or `.inout` to `access` to use the move-initialization or move-assignment,
  /// respectively.
  private mutating func emitMove(
    _ access: AccessEffect,
    of value: Operand,
    to storage: Operand,
    withConformanceToMovable c: Conformance,
    at site: SourceRange
  ) {
    let oper = module.demandMoveOperatorDeclaration(access, from: c)
    let move = Operand.constant(FunctionReference(to: oper, usedIn: c.scope, in: module))

    let x0 = append(module.makeBorrow(access, from: storage, at: site))[0]
    let x1 = emitAllocStack(for: .void, at: site)
    let x2 = append(module.makeBorrow(.set, from: x1, at: site))[0]
    append(module.makeCall(applying: move, to: [x0, value], writingResultTo: x2, at: site))
  }

  /// Emits a deallocation instruction for each allocation in the top frame of `self.frames`.
  private mutating func emitStackDeallocs(site: SourceRange) {
    while let a = frames.top.allocs.popLast() {
      append(module.makeDeallocStack(for: a, at: site))
    }
  }

  /// Emits the IR trapping iff `predicate`, which is an object of type `i1`, anchoring new
  /// instructions at `site`.
  private mutating func emitGuard(_ predicate: Operand, at site: SourceRange) {
    let failure = module.appendBlock(in: insertionScope!, to: insertionBlock!.function)
    let success = module.appendBlock(in: insertionScope!, to: insertionBlock!.function)
    append(module.makeCondBranch(if: predicate, then: success, else: failure, at: site))

    insertionBlock = failure
    append(module.makeUnreachable(at: site))
    insertionBlock = success
  }

  /// Returns the result of calling `action` on a copy of `self` in which a `newFrame` is the top
  /// frame.
  ///
  /// `newFrame` is pushed on `self.frames` before `action` is called. When `action` returns,
  /// outstanding stack allocations are deallocated and `newFrame` is popped. References to stack
  /// memory allocated by `action` are invalidated when this method returns.
  private mutating func pushing<T>(_ newFrame: Frame, _ action: (inout Self) -> T) -> T {
    frames.push(newFrame)
    defer {
      emitStackDeallocs(site: .empty(atEndOf: program.ast[insertionScope!].site))
      frames.pop()
    }
    return action(&self)
  }

  /// Returns the result of calling `action` on a copy of `self` whose insertion block, frames,
  /// and receiver are clear.
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
    mutating func push(_ newFrame: Frame = .init()) {
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

}
