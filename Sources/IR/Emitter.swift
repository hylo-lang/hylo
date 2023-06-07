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

    // Configure the emitter context.
    let entry = module.appendEntry(in: program.scopeContainingBody(of: d.id)!, to: f)
    let bodyFrame = outermostFrame(of: d, entering: entry)

    self.insertionBlock = entry
    self.receiver = d.receiver

    // Emit the body.
    switch b {
    case .block(let s):
      pushing(bodyFrame, { $0.emit(braceStmt: s) })
    case .expr(let e):
      pushing(bodyFrame, { $0.store(value: e, to: $0.returnValue!) })
      append(module.makeReturn(at: e.site))
    }

    return f
  }

  /// Returns the frame enclosing the body of `d`, whose entry block is `entry`.
  private func outermostFrame(of d: FunctionDecl.Typed, entering entry: Block.ID) -> Frame {
    var locals = TypedDeclProperty<Operand>()

    // Exlicit captures appear first.
    for (i, c) in d.explicitCaptures.enumerated() {
      locals[c] = .parameter(entry, i)
    }

    // Implicit captures appear next.
    for (i, c) in d.implicitCaptures!.enumerated() {
      locals[program[c.decl]] = .parameter(entry, i + d.explicitCaptures.count)
    }

    // Function receiver appears next.
    var captureCount = d.explicitCaptures.count + d.implicitCaptures!.count
    if let r = d.receiver {
      locals[r] = .parameter(entry, captureCount)
      captureCount += 1
    }

    // Explicit parameters appear last.
    for (i, p) in d.parameters.enumerated() {
      locals[p] = .parameter(entry, i + captureCount)
    }

    return Frame(locals: locals)
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
    if returnType.isVoidOrNever {
      store(value: .void, to: returnValue!, at: d.site)
    } else {
      let v = emitConvert(foreign: foreignResult, to: returnType, at: d.site)
      store(value: v, to: returnValue!, at: d.site)
    }

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
    defer {
      receiver = r
    }

    // Emit the body.
    pushing(Frame(locals: locals), { $0.emit(braceStmt: d.body!) })
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
          let part = store(value: program[rhs])
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

    store(value: .void, to: returnValue!, at: site)
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
    let t = module.type(of: receiver).ast
    let c = program.conformance(of: t, to: program.ast.movableTrait, exposedTo: scope)!
    let r = append(module.makeLoad(argument, at: site))[0]
    emitMove(.set, of: r, to: receiver, withConformanceToMovable: c, at: site)

    store(value: .void, to: returnValue!, at: site)
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
    var rhs = store(value: stmt.right)
    rhs = append(module.makeLoad(rhs, at: stmt.site))[0]
    let lhs = emitLValue(stmt.left)

    let t = program.relations.canonical(stmt.left.type)

    // Built-in types do not require deinitialization.
    if t.base is BuiltinType {
      store(value: rhs, to: lhs, at: stmt.site)
      return
    }

    // The LHS can be assumed to conform to `Movable` if type checking passed.
    let c = program.conformance(of: t, to: program.ast.movableTrait, exposedTo: insertionScope!)!
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
    let v = store(value: stmt.expr)
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
    let v = store(value: stmt.expr)
    if module.type(of: v).ast.isVoidOrNever {
      append(module.makeDeinit(v, at: stmt.site))
    } else {
      // TODO: complain about unused value
      fatalError("not implemented")
    }
  }

  private mutating func emit(returnStmt stmt: ReturnStmt.Typed) {
    if let e = stmt.value {
      store(value: e, to: returnValue!)
    } else {
      store(value: .void, to: returnValue!, at: stmt.site)
    }

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

  // MARK: values

  private mutating func store(value: Operand, to storage: Operand, at site: SourceRange) {
    let x0 = append(module.makeBorrow(.set, from: storage, at: site))[0]
    append(module.makeStore(value, at: x0, at: site))
  }

  /// Inserts the IR for storing the value of `syntax` to a fresh stack allocation, returning the
  /// address of this allocation.
  @discardableResult
  private mutating func store<ID: ExprID>(value syntax: ID.TypedNode) -> Operand {
    let s = emitAllocStack(for: syntax.type, at: syntax.site)
    store(value: syntax, to: s)
    return s
  }

  /// Inserts the IR for storing the value of `syntax` to `storage`.
  ///
  /// `storage` must be the address of some uninitialized memory block capable of storing the value
  /// of `syntax`.
  private mutating func store<ID: ExprID>(value syntax: ID.TypedNode, to storage: Operand) {
    switch syntax.kind {
    case BooleanLiteralExpr.self:
      store(booleanLiteral: .init(syntax)!, to: storage)
    case CastExpr.self:
      store(cast: .init(syntax)!, to: storage)
    case ConditionalExpr.self:
      store(conditional: .init(syntax)!, to: storage)
    case FloatLiteralExpr.self:
      store(floatLiteral: .init(syntax)!, to: storage)
    case FunctionCallExpr.self:
      store(functionCall: .init(syntax)!, to: storage)
    case IntegerLiteralExpr.self:
      store(integerLiteral: .init(syntax)!, to: storage)
    case LambdaExpr.self:
      store(lambda: .init(syntax)!, to: storage)
    case NameExpr.self:
      store(name: .init(syntax)!, to: storage)
    case PragmaLiteralExpr.self:
      store(pragma: .init(syntax)!, to: storage)
    case SequenceExpr.self:
      store(sequence: .init(syntax)!, to: storage)
    case StringLiteralExpr.self:
      store(stringLiteral: .init(syntax)!, to: storage)
    case TupleExpr.self:
      store(tuple: .init(syntax)!, to: storage)
    case TupleMemberExpr.self:
      store(tupleMember: .init(syntax)!, to: storage)
    default:
      unexpected(syntax)
    }
  }

  private mutating func store(booleanLiteral e: BooleanLiteralExpr.Typed, to storage: Operand) {
    let x0 = append(module.makeElementAddr(storage, at: [0], at: e.site))[0]
    let x1 = append(module.makeBorrow(.set, from: x0, at: e.site))[0]
    append(module.makeStore(.i1(e.value), at: x1, at: e.site))
  }

  private mutating func store(cast e: CastExpr.Typed, to storage: Operand) {
    switch e.direction {
    case .up:
      store(upcast: e, to: storage)
    case .down:
      store(downcast: e, to: storage)
    case .pointerConversion:
      fatalError("not implemented")
    }
  }

  private mutating func store(upcast e: CastExpr.Typed, to storage: Operand) {
    assert(e.direction == .up)
    let target = MetatypeType(e.right.type)!.instance

    // Store the LHS to `storage` if it already has the desired type.
    if program.relations.areEquivalent(e.left.type, target) {
      store(value: e.left, to: storage)
      return
    }

    // Otherwise, wrap the LHS.
    fatalError("not implemented")
  }

  private mutating func store(downcast e: CastExpr.Typed, to storage: Operand) {
    assert(e.direction == .down)
    let target = MetatypeType(e.right.type)!.instance

    // Store the LHS to `storage` if it already has the desired type.
    if program.relations.areEquivalent(e.left.type, target) {
      store(value: e.left, to: storage)
      return
    }

    // Otherwise, unpack or repack the LHS.
    let lhs = store(value: e.left)

    // TODO
    _ = lhs
    fatalError("not implementeds")
  }

  private mutating func store(conditional e: ConditionalExpr.Typed, to storage: Operand) {
    let (success, failure) = emitTest(condition: e.condition, in: AnyScopeID(e.id))
    let tail = module.appendBlock(in: insertionScope!, to: insertionBlock!.function)

    // Emit the success branch.
    insertionBlock = success
    pushing(Frame(), { $0.emitInitialization(of: storage, to: e.success) })
    append(module.makeBranch(to: tail, at: e.site))

    // Emit the failure branch.
    insertionBlock = failure
    pushing(Frame(), { $0.emitInitialization(of: storage, to: e.failure) })
    append(module.makeBranch(to: tail, at: e.site))

    insertionBlock = tail
  }

  private mutating func store(floatLiteral e: FloatLiteralExpr.Typed, to storage: Operand) {
    store(numericLiteral: e.id, to: storage)
  }

  private mutating func store(functionCall e: FunctionCallExpr.Typed, to storage: Operand) {
    // Handle built-ins and constructor calls.
    if let n = NameExpr.Typed(e.callee) {
      switch n.declaration {
      case .builtinFunction(let f):
        let x0 = emit(apply: f, to: e.arguments, at: e.site)
        let x1 = append(module.makeBorrow(.set, from: storage, at: e.site))[0]
        append(module.makeStore(x0, at: x1, at: e.site))
        return

      case .constructor:
        emitInitializerCall(e, initializing: storage)
        return

      default:
        break
      }
    }

    // Explicit arguments are evaluated first, from left to right.
    let syntheticSite = e.site.file.emptyRange(at: e.site.end)
    let explicitArguments = emit(
      arguments: e.arguments, to: e.callee,
      synthesizingDefaultArgumentsAt: syntheticSite)

    // Callee and captures are evaluated next.
    let (callee, captures) = emitCallee(e.callee)
    let allArguments = captures + explicitArguments

    // Call is evaluated last.
    let o = append(module.makeBorrow(.set, from: storage, at: e.site))[0]
    append(module.makeCall(applying: callee, to: allArguments, writingResultTo: o, at: e.site))
  }

  private mutating func store(integerLiteral e: IntegerLiteralExpr.Typed, to storage: Operand) {
    store(numericLiteral: e.id, to: storage)
  }

  private mutating func store(lambda e: LambdaExpr.Typed, to storage: Operand) {
    let f = emit(functionDecl: e.decl)
    let r = FunctionReference(to: f, usedIn: insertionScope!, in: module)

    let x0 = append(module.makePartialApply(wrapping: r, with: .void, at: e.site))[0]
    let x1 = append(module.makeBorrow(.set, from: storage, at: e.site))[0]
    append(module.makeStore(x0, at: x1, at: e.site))
  }

  private mutating func store(name e: NameExpr.Typed, to storage: Operand) {
    let x0 = emitLValue(name: e)
    let x1 = append(module.makeLoad(x0, at: e.site))[0]

    let t = module.type(of: storage).ast
    if t.isBuiltin {
      let x2 = append(module.makeBorrow(.set, from: storage, at: e.site))[0]
      append(module.makeStore(x1, at: x2, at: e.site))
    } else {
      let c = program.conformance(of: t, to: program.ast.movableTrait, exposedTo: insertionScope!)!
      emitMove(.set, of: x1, to: storage, withConformanceToMovable: c, at: e.site)
    }
  }

  /// Writes the value of `e` to `storage`.
  ///
  /// - Parameters:
  ///   - site: The source range in which `e` is being evaluated. Defaults to `e.site`.
  private mutating func store(
    pragma e: PragmaLiteralExpr.Typed, to storage: Operand,
    at site: SourceRange? = nil
  ) {
    let anchor = site ?? e.site
    switch program.ast[e.id].kind {
    case .file:
      store(string: anchor.file.url.absoluteURL.path, to: storage, at: anchor)
    case .line:
      store(int: anchor.first().line.number, to: storage, at: anchor)
    }
  }

  private mutating func store(sequence e: SequenceExpr.Typed, to storage: Operand) {
    store(foldedSequence: e.folded, to: storage)
  }

  private mutating func store(foldedSequence e: FoldedSequenceExpr, to storage: Operand) {
    switch e {
    case .infix(let callee, let lhs, let rhs):
      let t = program.exprTypes[callee.expr]!
      let calleeType = LambdaType(program.relations.canonical(t))!.lifted

      // Emit the operands, starting with RHS.
      let r = emit(infixOperand: rhs, passed: ParameterType(calleeType.inputs[1].type)!.access)
      let l = emit(infixOperand: lhs, passed: ParameterType(calleeType.inputs[0].type)!.access)

      // The callee must be a reference to member function.
      guard case .member(let d, _) = program.referredDecls[callee.expr] else { unreachable() }
      let oper = Operand.constant(
        FunctionReference(to: program[FunctionDecl.ID(d)!], usedIn: insertionScope!, in: &module))

      // Emit the call.
      let site = program.ast.site(of: e)
      let x0 = append(module.makeBorrow(.set, from: storage, at: site))[0]
      append(module.makeCall(applying: oper, to: [l, r], writingResultTo: x0, at: site))

    case .leaf(let v):
      store(value: program[v], to: storage)
    }
  }

  private mutating func store(stringLiteral e: StringLiteralExpr.Typed, to storage: Operand) {
    store(string: e.value, to: storage, at: e.site)
  }

  private mutating func store(tuple e: TupleExpr.Typed, to storage: Operand) {
    for (i, element) in e.elements.enumerated() {
      let syntax = program[element.value]
      let xi = append(module.makeElementAddr(storage, at: [i], at: syntax.site))[0]
      store(value: syntax, to: xi)
    }
  }

  private mutating func store(tupleMember e: TupleMemberExpr.Typed, to storage: Operand) {
    let x0 = emitLValue(e.tuple)
    let x1 = append(module.makeElementAddr(x0, at: [e.index.value], at: e.index.site))[0]
    let x2 = append(module.makeLoad(x1, at: e.site))[0]

    let t = module.type(of: storage).ast
    let c = program.conformance(of: t, to: program.ast.movableTrait, exposedTo: insertionScope!)!
    emitMove(.set, of: x2, to: storage, withConformanceToMovable: c, at: e.site)
  }

  /// Writes the value of `literal` to `storage`.
  private mutating func store<T: NumericLiteralExpr>(
    numericLiteral literal: T.ID, to storage: Operand
  ) {
    let literalType = program.relations.canonical(program.exprTypes[literal]!)

    switch literalType {
    case program.ast.coreType("Int")!:
      store(integer: literal, signed: true, bitWidth: 64, to: storage)
    case program.ast.coreType("Int32")!:
      store(integer: literal, signed: true, bitWidth: 32, to: storage)
    case program.ast.coreType("Int8")!:
      store(integer: literal, signed: true, bitWidth: 8, to: storage)
    case program.ast.coreType("Double")!:
      store(floatingPoint: literal, to: storage, evaluatedBy: FloatingPointConstant.double(_:))
    case program.ast.coreType("Float")!:
      store(floatingPoint: literal, to: storage, evaluatedBy: FloatingPointConstant.float(_:))
    default:
      fatalError("not implemented")
    }
  }

  /// Writes the value of `literal` to `storage`, knowing it is a core floating-point instance
  /// evaluated by `evaluate`.
  private mutating func store<T: NumericLiteralExpr>(
    floatingPoint literal: T.ID, to storage: Operand,
    evaluatedBy evaluate: (String) -> FloatingPointConstant
  ) {
    let syntax = program.ast[literal]
    let x0 = append(module.makeElementAddr(storage, at: [0], at: syntax.site))[0]
    let x1 = append(module.makeBorrow(.set, from: x0, at: syntax.site))[0]
    let x2 = Operand.constant(evaluate(syntax.value))
    append(module.makeStore(x2, at: x1, at: syntax.site))
  }

  /// Writes the value of `literal` to `storage`, knowing it is a core integer instance with given
  /// sign and width.
  private mutating func store<T: NumericLiteralExpr>(
    integer literal: T.ID, signed: Bool, bitWidth: Int, to storage: Operand
  ) {
    let syntax = program.ast[literal]
    guard let bits = WideUInt(valLiteral: syntax.value, signed: signed, bitWidth: bitWidth) else {
      diagnostics.insert(
        .error(
          integerLiteral: syntax.value,
          overflowsWhenStoredInto: program.exprTypes[literal]!,
          at: syntax.site))
      return
    }

    let x0 = append(module.makeElementAddr(storage, at: [0], at: syntax.site))[0]
    let x1 = append(module.makeBorrow(.set, from: x0, at: syntax.site))[0]
    let x2 = Operand.constant(IntegerConstant(bits))
    append(module.makeStore(x2, at: x1, at: syntax.site))
  }

  /// Writes an instance of `Val.Int` with value `v` to `storage`, anchoring new instruction at
  /// `site`.
  ///
  /// - Requires: `storage` is the address of uninitialized memory of type `Val.Int`.
  private mutating func store(int v: Int, to storage: Operand, at site: SourceRange) {
    let x0 = append(module.makeElementAddr(storage, at: [0], at: site))[0]
    let x1 = append(module.makeBorrow(.set, from: x0, at: site))[0]
    append(module.makeStore(.word(v), at: x1, at: site))
  }

  /// Writes an instance of `Val.String` with value `v` to `storage`, anchoring new instruction at
  /// `site`.
  ///
  /// - Requires: `storage` is the address of uninitialized memory of type `Val.String`.
  private mutating func store(string v: String, to storage: Operand, at site: SourceRange) {
    var bytes = v.unescaped.data(using: .utf8)!
    let utf8 = PointerConstant(module.syntax.id, module.addGlobal(BufferConstant(bytes)))
    let size = bytes.count

    // Make sure the string is null-terminated.
    bytes.append(contentsOf: [0])

    let x0 = append(module.makeElementAddr(storage, at: [0], at: site))[0]
    store(int: size, to: x0, at: site)

    let x1 = append(module.makeElementAddr(storage, at: [1, 0], at: site))[0]
    let x2 = append(module.makeBorrow(.set, from: x1, at: site))[0]
    append(module.makeStore(.constant(utf8), at: x2, at: site))
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
    let argumentSite: SourceRange
    let storage: Operand

    if let e = PragmaLiteralExpr.Typed(syntax) {
      argumentSite = site ?? syntax.site
      storage = append(module.makeAllocStack(e.type, at: argumentSite))[0]
      store(pragma: e, to: storage, at: argumentSite)
    } else {
      argumentSite = syntax.site
      storage = emitLValue(syntax)
    }

    return take(parameter.access, on: storage, at: argumentSite)
  }

  /// Inserts the IR for infix operand `e` passed with convention `access`.
  private mutating func emit(
    infixOperand e: FoldedSequenceExpr, passed access: AccessEffect
  ) -> Operand {
    let storage: Operand

    switch e {
    case .infix(let callee, _, _):
      let t = program.exprTypes[callee.expr]!
      let u = LambdaType(program.relations.canonical(t))!.lifted
      storage = emitAllocStack(for: u.output, at: program.ast.site(of: e))
      store(foldedSequence: e, to: storage)

    case .leaf(let e):
      storage = emitLValue(program[e])
    }

    return take(access, on: storage, at: program.ast.site(of: e))
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
        let x0 = store(value: program[e.value])
        let x1 = append(module.makeLoad(x0, at: site))[0]
        a.append(x1)
      }
      return append(module.makeLLVM(applying: n, to: a, at: site))[0]

    case .addressOf:
      let source = emitLValue(program[arguments[0].value])
      return append(
        module.makeAddressToPointer(source, at: site))[0]
    }
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
    case .set, .sink:
      fatalError("not implemented")

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
    case TupleMemberExpr.self:
      return emitLValue(tupleMember: .init(syntax)!)
    default:
      return store(value: syntax)
    }
  }

  private mutating func emitLValue(cast syntax: CastExpr.Typed) -> Operand {
    switch syntax.direction {
    case .pointerConversion:
      let x0 = emitLValue(syntax.left)
      let x1 = append(module.makeLoad(x0, at: syntax.site))[0]
      let target = RemoteType(program.relations.canonical(syntax.type))!
      return append(module.makePointerToAddress(x1, to: target, at: syntax.site))[0]

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

  /// Inserts the IR for taking given `access` on `source` at `site`.
  private mutating func take(
    _ access: AccessEffect, on source: Operand, at site: SourceRange
  ) -> Operand {
    switch access {
    case .let, .inout, .set:
      return append(module.makeBorrow(access, from: source, at: site))[0]
    case .sink:
      return append(module.makeLoad(source, at: site))[0]
    case .yielded:
      unreachable()
    }
  }

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
      store(value: program[value], to: storage)
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
