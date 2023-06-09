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
  mutating func lower(
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
      lower(topLevel: program[u])
    }

    // Lower the synthesized implementations.
    for i in program.synthesizedDecls[d, default: []] {
      lower(synthesized: i)
    }
  }

  /// Inserts the IR for the top-level declaration `d`.
  ///
  /// - Requires: `d` is at module scope.
  private mutating func lower(topLevel d: AnyDeclID.TypedNode) {
    precondition(program.isAtModuleScope(d.id))
    switch d.kind {
    case BindingDecl.self:
      lower(globalBinding: .init(d)!)
    case ConformanceDecl.self:
      lower(conformance: .init(d)!)
    case ExtensionDecl.self:
      lower(extension: .init(d)!)
    case FunctionDecl.self:
      lower(function: .init(d)!)
    case OperatorDecl.self:
      break
    case NamespaceDecl.self:
      lower(namespace: .init(d)!)
    case ProductTypeDecl.self:
      lower(product: .init(d)!)
    case TraitDecl.self:
      lower(trait: .init(d)!)
    case TypeAliasDecl.self:
      break
    default:
      unexpected(d)
    }
  }

  /// Inserts the IR for `d`.
  private mutating func lower(conformance d: ConformanceDecl.Typed) {
    lower(members: d.members)
  }

  /// Inserts the IR for `d`.
  private mutating func lower(extension d: ExtensionDecl.Typed) {
    lower(members: d.members)
  }

  /// Inserts the IR for `d`, returning the ID of the lowered function.
  @discardableResult
  private mutating func lower(function d: FunctionDecl.Typed) -> Function.ID {
    withClearContext({ $0.lowerInClearContext(function: d) })
  }

  /// Inserts the IR for `d`, returning the ID of the lowered function.
  ///
  /// - Requires: `self` has a clear lowering context.
  private mutating func lowerInClearContext(function d: FunctionDecl.Typed) -> Function.ID {
    let f = module.demandFunctionDeclaration(lowering: d)
    guard let b = d.body else {
      if d.isForeignInterface { lower(ffi: d) }
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
      pushing(bodyFrame, { $0.emitStore(value: e, to: $0.returnValue!) })
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
  private mutating func lower(ffi d: FunctionDecl.Typed) {
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
    switch returnType {
    case .never:
      append(module.makeUnreachable(at: d.site))

    case .void:
      emitStore(value: .void, to: returnValue!, at: d.site)
      emitStackDeallocs(site: d.site)
      append(module.makeReturn(at: d.site))

    default:
      let v = emitConvert(foreign: foreignResult, to: returnType, at: d.site)
      emitStore(value: v, to: returnValue!, at: d.site)
      emitStackDeallocs(site: d.site)
      append(module.makeReturn(at: d.site))
    }
  }

  /// Inserts the IR for `d`.
  private mutating func lower(initializer d: InitializerDecl.Typed) {
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
  private mutating func lower(subscript d: SubscriptDecl.Typed) {
    for i in d.impls {
      lower(subscriptImpl: i)
    }
  }

  /// Inserts the IR for `d`.
  private mutating func lower(subscriptImpl d: SubscriptImpl.Typed) {
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
  private mutating func lower(namespace decl: NamespaceDecl.Typed) {
    for m in decl.members {
      lower(topLevel: m)
    }
  }

  /// Inserts the IR for `d`.
  private mutating func lower(product d: ProductTypeDecl.Typed) {
    _ = module.addGlobal(MetatypeConstant(.init(d.type)!))
    lower(members: d.members)
  }

  /// Inserts the IR for `d`.
  private mutating func lower(trait d: TraitDecl.Typed) {
    _ = module.addGlobal(MetatypeConstant(.init(d.type)!))
  }

  /// Inserts the IR for given declaration `members`.
  private mutating func lower(members: [AnyDeclID]) {
    for m in members {
      switch m.kind {
      case FunctionDecl.self:
        lower(function: .init(program[m])!)
      case InitializerDecl.self:
        lower(initializer: .init(program[m])!)
      case SubscriptDecl.self:
        lower(subscript: .init(program[m])!)
      default:
        continue
      }
    }
  }

  /// Inserts the IR for `d`.
  ///
  /// - Requires: `d` is a global binding.
  private mutating func lower(globalBinding d: BindingDecl.Typed) {
    fatalError("not implemented")
  }

  /// Inserts the IR for the local binding `d`.
  ///
  /// - Requires: `d` is a local binding.
  private mutating func lower(localBinding d: BindingDecl.Typed) {
    switch d.pattern.introducer.value {
    case .var, .sinklet:
      lower(storedLocalBinding: d)
    case .let, .inout:
      lower(projectedLocalBinding: d)
    }
  }

  /// Inserts the IR for stored local binding `d`.
  ///
  /// - Requires: `d` is a local `var` or `sink let` binding.
  private mutating func lower(storedLocalBinding d: BindingDecl.Typed) {
    precondition(program.isLocal(d.id))
    precondition(read(d.pattern.introducer.value, { ($0 == .var) || ($0 == .sinklet) }))

    // Allocate storage for all the names declared by `decl`.
    let storage = emitAllocStack(for: d.type, at: d.site)

    // Declare each introduced name and initialize them if possible.
    let lhs = d.pattern.subpattern.id
    if let initializer = d.initializer {
      program.ast.walking(pattern: lhs, expression: initializer.id) { (path, p, rhs) in
        // Declare the introduced name if `p` is a name pattern. Otherwise, drop the value of the
        // the corresponding expression.
        if let name = NamePattern.ID(p) {
          let part = declare(name: program[name], referringTo: path)
          emitStore(value: program[rhs], to: part)
        } else {
          let part = emitStore(value: program[rhs])
          append(module.makeDeinit(part, at: program.ast[p].site))
        }
      }
    } else {
      for (path, name) in program.ast.names(in: lhs) {
        _ = declare(name: program[name], referringTo: path)
      }
    }

    /// Inserts the IR to declare `name`, which refers to the sub-location at `pathInStorage`,
    /// returning that sub-location.
    func declare(name: NamePattern.Typed, referringTo pathInStorage: PartPath) -> Operand {
      let s = emitElementAddr(storage, at: pathInStorage, at: name.site)
      frames[name.decl] = s
      return s
    }
  }

  /// Inserts the IR for projected local binding `d` .
  ///
  /// - Requires: `d` is a local `let` or `inout` binding.
  private mutating func lower(projectedLocalBinding d: BindingDecl.Typed) {
    precondition(program.isLocal(d.id))

    let access: AccessEffect
    switch d.pattern.introducer.value {
    case .let:
      access = .let
    case .inout:
      access = .inout
    default:
      preconditionFailure()
    }

    // Borrowed binding requires an initializer.
    guard let initializer = d.initializer else {
      report(.error(binding: access, requiresInitializerAt: d.pattern.introducer.site))
      for (_, name) in d.pattern.subpattern.names {
        frames[name.decl] = .constant(Poison(type: .address(name.decl.type)))
      }
      return
    }

    // Initializing inout bindings requires a mutation marker.
    if (access == .inout) && (initializer.kind != InoutExpr.self) {
      report(.error(inoutBindingRequiresMutationMarkerAt: .empty(at: initializer.site.first())))
    }

    let source = emitLValue(initializer)
    let isSink = module.isSink(source, in: insertionBlock!.function)

    for (path, name) in d.pattern.subpattern.names {
      var part = emitElementAddr(source, at: path, at: name.decl.site)
      let partType = module.type(of: part).ast

      if !program.relations.areEquivalent(name.decl.type, partType) {
        if let u = ExistentialType(name.decl.type) {
          let box = emitExistential(
            u, borrowing: access, from: part,
            at: name.decl.site)
          part = box
        }
      }

      if isSink {
        let b = module.makeAccess(
          [.sink, access], from: part, correspondingTo: name.decl, at: name.decl.site)
        frames[name.decl] = append(b)[0]
      } else {
        let b = module.makeBorrow(
          access, from: part, correspondingTo: name.decl, at: name.decl.site)
        frames[name.decl] = append(b)[0]
      }
    }
  }

  /// Returns the lowered conformances of `model` that are exposed to `useScope`.
  private mutating func loweredConformances(
    of model: AnyType, exposedTo useScope: AnyScopeID
  ) -> Set<LoweredConformance> {
    guard let conformances = program.relations.conformances[model] else { return [] }

    var result: Set<LoweredConformance> = []
    for concept in conformances.keys {
      let c = program.conformance(of: model, to: concept, exposedTo: useScope)!
      result.insert(loweredConformance(c, in: useScope))
    }
    return result
  }

  /// Returns the lowered form of `c`, generating function references in `useScope`.
  private mutating func loweredConformance(
    _ c: Conformance, in useScope: AnyScopeID
  ) -> LoweredConformance {
    var implementations = LoweredConformance.ImplementationMap()
    for (r, i) in c.implementations.storage {
      switch i {
      case .concrete(let d):
        implementations[r] = loweredRequirementImplementation(d, in: useScope)

      case .synthetic(let d):
        let f = lower(synthesized: d)
        implementations[r] = .function(.init(to: f, usedIn: useScope, in: module))
      }
    }

    return .init(concept: c.concept, source: c.source, implementations: implementations)
  }

  /// Returns the lowered form of the requirement implementation `d` in `useScope`.
  private mutating func loweredRequirementImplementation(
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

  /// Synthesizes the implementation of `d`, returning the ID of the corresponding IR function.
  @discardableResult
  private mutating func lower(synthesized d: SynthesizedDecl) -> Function.ID {
    switch d.kind {
    case .moveInitialization:
      return lowerSynthesizedMoveInitImplementation(typed: .init(d.type)!, in: d.scope)
    case .moveAssignment:
      return lowerSynthesizedMoveAssignImplementation(typed: .init(d.type)!, in: d.scope)
    case .copy:
      fatalError("not implemented")
    }
  }

  /// Synthesizes the implementation of `t`'s move initialization operator in `scope`, returning
  /// the ID of the corresponding IR function.
  private mutating func lowerSynthesizedMoveInitImplementation(
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

      // If the object is empty, simply mark it initialized.
      if layout.properties.isEmpty {
        let x0 = append(module.makeUnsafeCast(.void, to: layout.type, at: site))[0]
        let x1 = append(module.makeBorrow(.set, from: receiver, at: site))[0]
        append(module.makeStore(x0, at: x1, at: site))
        append(module.makeDeinit(argument, at: site))
        break
      }

      // Otherwise, move initialize each property.
      for (i, p) in layout.properties.enumerated() {
        let source = emitElementAddr(argument, at: [i], at: site)
        let part = append(module.makeLoad(source, at: site))[0]
        let target = emitElementAddr(receiver, at: [i], at: site)

        if p.type.base is BuiltinType {
          append(module.makeStore(part, at: target, at: site))
        } else {
          let c = program.conformanceToMovable(of: p.type, exposedTo: scope)!
          emitMove(.set, of: part, to: target, withConformanceToMovable: c, at: site)
        }
      }

    default:
      fatalError("not implemented")
    }

    emitStore(value: .void, to: returnValue!, at: site)
    emitStackDeallocs(site: site)
    append(module.makeReturn(at: site))
    return f
  }

  /// Synthesizes the implementation of `t`'s move assignment operator in `scope`, returning the ID
  /// of the corresponding IR function.
  private mutating func lowerSynthesizedMoveAssignImplementation(
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
    let c = program.conformanceToMovable(of: t, exposedTo: scope)!
    let r = append(module.makeLoad(argument, at: site))[0]
    emitMove(.set, of: r, to: receiver, withConformanceToMovable: c, at: site)

    emitStore(value: .void, to: returnValue!, at: site)
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
    var rhs = emitStore(value: stmt.right)
    rhs = append(module.makeLoad(rhs, at: stmt.site))[0]
    let lhs = emitLValue(stmt.left)

    let t = program.relations.canonical(stmt.left.type)

    // Built-in types do not require deinitialization.
    if t.base is BuiltinType {
      emitStore(value: rhs, to: lhs, at: stmt.site)
      return
    }

    // The LHS can be assumed to conform to `Movable` if type checking passed.
    let c = program.conformanceToMovable(of: t, exposedTo: insertionScope!)!
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
      lower(localBinding: BindingDecl.Typed(stmt.decl)!)
    default:
      unexpected(stmt.decl)
    }
  }

  private mutating func emit(discardStmt stmt: DiscardStmt.Typed) {
    let v = emitStore(value: stmt.expr)
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

    let c = emit(branchCondition: stmt.condition)
    emitStackDeallocs(site: stmt.site)
    frames.pop()

    append(module.makeCondBranch(if: c, then: loopBody, else: loopTail, at: stmt.condition.site))
    insertionBlock = loopTail
  }

  private mutating func emit(exprStmt stmt: ExprStmt.Typed) {
    let v = emitStore(value: stmt.expr)
    if module.type(of: v).ast.isVoidOrNever {
      append(module.makeDeinit(v, at: stmt.site))
    } else {
      // TODO: complain about unused value
      fatalError("not implemented")
    }
  }

  private mutating func emit(returnStmt stmt: ReturnStmt.Typed) {
    if let e = stmt.value {
      emitStore(value: e, to: returnValue!)
    } else {
      emitStore(value: .void, to: returnValue!, at: stmt.site)
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

  // MARK: Values

  /// Inserts the IR for storing `value` to `storage`, anchoring new instructions at `site`.
  private mutating func emitStore(value: Operand, to storage: Operand, at site: SourceRange) {
    let t = module.type(of: storage).ast

    // Because `emitStore(value:to:at:)` is used to synthesize conformances to `Movable`, calling
    // the move-initializer of `Val.Void` would cause infinite recursion. Since the latter should
    // always be inlined anyway, we can emit a simple store in all cases.
    if t.isBuiltin || (t == .void) {
      let x0 = append(module.makeBorrow(.set, from: storage, at: site))[0]
      append(module.makeStore(value, at: x0, at: site))
    } else {
      let c = program.conformanceToMovable(of: t, exposedTo: insertionScope!)!
      emitMove(.set, of: value, to: storage, withConformanceToMovable: c, at: site)
    }
  }

  /// Inserts the IR for storing the value of `syntax` to a fresh stack allocation, returning the
  /// address of this allocation.
  @discardableResult
  private mutating func emitStore<ID: ExprID>(value syntax: ID.TypedNode) -> Operand {
    let s = emitAllocStack(for: syntax.type, at: syntax.site)
    emitStore(value: syntax, to: s)
    return s
  }

  /// Inserts the IR for storing the value of `syntax` to `storage`.
  ///
  /// `storage` must be the address of some uninitialized memory block capable of storing the value
  /// of `syntax`.
  private mutating func emitStore<ID: ExprID>(value syntax: ID.TypedNode, to storage: Operand) {
    switch syntax.kind {
    case BooleanLiteralExpr.self:
      emitStore(booleanLiteral: .init(syntax)!, to: storage)
    case CastExpr.self:
      emitStore(cast: .init(syntax)!, to: storage)
    case ConditionalExpr.self:
      emitStore(conditional: .init(syntax)!, to: storage)
    case FloatLiteralExpr.self:
      emitStore(floatLiteral: .init(syntax)!, to: storage)
    case FunctionCallExpr.self:
      emitStore(functionCall: .init(syntax)!, to: storage)
    case IntegerLiteralExpr.self:
      emitStore(integerLiteral: .init(syntax)!, to: storage)
    case LambdaExpr.self:
      emitStore(lambda: .init(syntax)!, to: storage)
    case NameExpr.self:
      emitStore(name: .init(syntax)!, to: storage)
    case PragmaLiteralExpr.self:
      emitStore(pragma: .init(syntax)!, to: storage)
    case SequenceExpr.self:
      emitStore(sequence: .init(syntax)!, to: storage)
    case StringLiteralExpr.self:
      emitStore(stringLiteral: .init(syntax)!, to: storage)
    case TupleExpr.self:
      emitStore(tuple: .init(syntax)!, to: storage)
    case TupleMemberExpr.self:
      emitStore(tupleMember: .init(syntax)!, to: storage)
    default:
      unexpected(syntax)
    }
  }

  private mutating func emitStore(
    booleanLiteral e: BooleanLiteralExpr.Typed, to storage: Operand
  ) {
    let x0 = emitElementAddr(storage, at: [0], at: e.site)
    let x1 = append(module.makeBorrow(.set, from: x0, at: e.site))[0]
    append(module.makeStore(.i1(e.value), at: x1, at: e.site))
  }

  private mutating func emitStore(cast e: CastExpr.Typed, to storage: Operand) {
    switch e.direction {
    case .up:
      emitStore(upcast: e, to: storage)
    case .down:
      emitStore(downcast: e, to: storage)
    case .pointerConversion:
      unreachable("pointer to address conversion evalutes to a lvalue")
    }
  }

  private mutating func emitStore(upcast e: CastExpr.Typed, to storage: Operand) {
    assert(e.direction == .up)
    let target = MetatypeType(e.right.type)!.instance

    // Store the LHS to `storage` if it already has the desired type.
    if program.relations.areEquivalent(e.left.type, target) {
      emitStore(value: e.left, to: storage)
      return
    }

    // Otherwise, wrap the LHS.
    fatalError("not implemented")
  }

  private mutating func emitStore(downcast e: CastExpr.Typed, to storage: Operand) {
    assert(e.direction == .down)
    let target = MetatypeType(e.right.type)!.instance

    // Store the LHS to `storage` if it already has the desired type.
    if program.relations.areEquivalent(e.left.type, target) {
      emitStore(value: e.left, to: storage)
      return
    }

    // Otherwise, unpack or repack the LHS.
    let lhs = emitStore(value: e.left)

    // TODO
    _ = lhs
    fatalError("not implementeds")
  }

  private mutating func emitStore(conditional e: ConditionalExpr.Typed, to storage: Operand) {
    let (success, failure) = emitTest(condition: e.condition, in: AnyScopeID(e.id))
    let tail = module.appendBlock(in: insertionScope!, to: insertionBlock!.function)

    // Emit the success branch.
    insertionBlock = success
    pushing(Frame(), { $0.emitStore(value: $0.program[e.success], to: storage) })
    append(module.makeBranch(to: tail, at: e.site))

    // Emit the failure branch.
    insertionBlock = failure
    pushing(Frame(), { $0.emitStore(value: $0.program[e.failure], to: storage) })
    append(module.makeBranch(to: tail, at: e.site))

    insertionBlock = tail
  }

  private mutating func emitStore(floatLiteral e: FloatLiteralExpr.Typed, to storage: Operand) {
    emitStore(numericLiteral: e.id, to: storage)
  }

  private mutating func emitStore(functionCall e: FunctionCallExpr.Typed, to storage: Operand) {
    // Handle built-ins and constructor calls.
    if let n = NameExpr.Typed(e.callee) {
      switch n.declaration {
      case .builtinFunction(let f):
        let x0 = emit(apply: f, to: e.arguments, at: e.site)
        let x1 = append(module.makeBorrow(.set, from: storage, at: e.site))[0]
        append(module.makeStore(x0, at: x1, at: e.site))
        return

      case .constructor:
        emit(initializerCall: e, initializing: storage)
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
    let (callee, captures) = emit(callee: e.callee)
    let allArguments = captures + explicitArguments

    // Call is evaluated last.
    let o = append(module.makeBorrow(.set, from: storage, at: e.site))[0]
    append(module.makeCall(applying: callee, to: allArguments, writingResultTo: o, at: e.site))
  }

  private mutating func emitStore(
    integerLiteral e: IntegerLiteralExpr.Typed, to storage: Operand
  ) {
    emitStore(numericLiteral: e.id, to: storage)
  }

  private mutating func emitStore(lambda e: LambdaExpr.Typed, to storage: Operand) {
    let f = lower(function: e.decl)
    let r = FunctionReference(to: f, usedIn: insertionScope!, in: module)

    let x0 = append(module.makePartialApply(wrapping: r, with: .void, at: e.site))[0]
    let x1 = append(module.makeBorrow(.set, from: storage, at: e.site))[0]
    append(module.makeStore(x0, at: x1, at: e.site))
  }

  private mutating func emitStore(name e: NameExpr.Typed, to storage: Operand) {
    let x0 = emitLValue(name: e)
    let x1 = append(module.makeLoad(x0, at: e.site))[0]
    emitStore(value: x1, to: storage, at: e.site)
  }

  /// Writes the value of `e` to `storage`.
  ///
  /// - Parameters:
  ///   - site: The source range in which `e` is being evaluated. Defaults to `e.site`.
  private mutating func emitStore(
    pragma e: PragmaLiteralExpr.Typed, to storage: Operand,
    at site: SourceRange? = nil
  ) {
    let anchor = site ?? e.site
    switch program.ast[e.id].kind {
    case .file:
      emitStore(string: anchor.file.url.absoluteURL.path, to: storage, at: anchor)
    case .line:
      emitStore(int: anchor.first().line.number, to: storage, at: anchor)
    }
  }

  private mutating func emitStore(sequence e: SequenceExpr.Typed, to storage: Operand) {
    emitStore(foldedSequence: e.folded, to: storage)
  }

  private mutating func emitStore(foldedSequence e: FoldedSequenceExpr, to storage: Operand) {
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
      emitStore(value: program[v], to: storage)
    }
  }

  private mutating func emitStore(stringLiteral e: StringLiteralExpr.Typed, to storage: Operand) {
    emitStore(string: e.value, to: storage, at: e.site)
  }

  private mutating func emitStore(tuple e: TupleExpr.Typed, to storage: Operand) {
    if e.elements.isEmpty {
      let t = program.relations.canonical(e.type)
      let x0 = append(module.makeUnsafeCast(.void, to: t, at: e.site))[0]
      let x1 = append(module.makeBorrow(.set, from: storage, at: e.site))[0]
      append(module.makeStore(x0, at: x1, at: e.site))
      return
    }

    for (i, element) in e.elements.enumerated() {
      let syntax = program[element.value]
      let xi = emitElementAddr(storage, at: [i], at: syntax.site)
      emitStore(value: syntax, to: xi)
    }
  }

  private mutating func emitStore(tupleMember e: TupleMemberExpr.Typed, to storage: Operand) {
    let x0 = emitLValue(tupleMember: e)
    let x1 = append(module.makeLoad(x0, at: e.site))[0]

    let t = module.type(of: storage).ast
    let c = program.conformanceToMovable(of: t, exposedTo: insertionScope!)!
    emitMove(.set, of: x1, to: storage, withConformanceToMovable: c, at: e.site)
  }

  /// Writes the value of `literal` to `storage`.
  private mutating func emitStore<T: NumericLiteralExpr>(
    numericLiteral literal: T.ID, to storage: Operand
  ) {
    let literalType = program.relations.canonical(program.exprTypes[literal]!)

    switch literalType {
    case program.ast.coreType("Int")!:
      emitStore(integer: literal, signed: true, bitWidth: 64, to: storage)
    case program.ast.coreType("Int32")!:
      emitStore(integer: literal, signed: true, bitWidth: 32, to: storage)
    case program.ast.coreType("Int8")!:
      emitStore(integer: literal, signed: true, bitWidth: 8, to: storage)
    case program.ast.coreType("Double")!:
      emitStore(floatingPoint: literal, to: storage, evaluatedBy: FloatingPointConstant.double(_:))
    case program.ast.coreType("Float")!:
      emitStore(floatingPoint: literal, to: storage, evaluatedBy: FloatingPointConstant.float(_:))
    default:
      fatalError("not implemented")
    }
  }

  /// Writes the value of `literal` to `storage`, knowing it is a core floating-point instance
  /// evaluated by `evaluate`.
  private mutating func emitStore<T: NumericLiteralExpr>(
    floatingPoint literal: T.ID, to storage: Operand,
    evaluatedBy evaluate: (String) -> FloatingPointConstant
  ) {
    let syntax = program.ast[literal]
    let x0 = emitElementAddr(storage, at: [0], at: syntax.site)
    let x1 = append(module.makeBorrow(.set, from: x0, at: syntax.site))[0]
    let x2 = Operand.constant(evaluate(syntax.value))
    append(module.makeStore(x2, at: x1, at: syntax.site))
  }

  /// Writes the value of `literal` to `storage`, knowing it is a core integer instance with given
  /// sign and width.
  private mutating func emitStore<T: NumericLiteralExpr>(
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

    let x0 = emitElementAddr(storage, at: [0], at: syntax.site)
    let x1 = append(module.makeBorrow(.set, from: x0, at: syntax.site))[0]
    let x2 = Operand.constant(IntegerConstant(bits))
    append(module.makeStore(x2, at: x1, at: syntax.site))
  }

  /// Writes an instance of `Val.Int` with value `v` to `storage`, anchoring new instruction at
  /// `site`.
  ///
  /// - Requires: `storage` is the address of uninitialized memory of type `Val.Int`.
  private mutating func emitStore(int v: Int, to storage: Operand, at site: SourceRange) {
    let x0 = emitElementAddr(storage, at: [0], at: site)
    let x1 = append(module.makeBorrow(.set, from: x0, at: site))[0]
    append(module.makeStore(.word(v), at: x1, at: site))
  }

  /// Writes an instance of `Val.String` with value `v` to `storage`, anchoring new instruction at
  /// `site`.
  ///
  /// - Requires: `storage` is the address of uninitialized memory of type `Val.String`.
  private mutating func emitStore(string v: String, to storage: Operand, at site: SourceRange) {
    var bytes = v.unescaped.data(using: .utf8)!
    let utf8 = PointerConstant(module.syntax.id, module.addGlobal(BufferConstant(bytes)))
    let size = bytes.count

    // Make sure the string is null-terminated.
    bytes.append(contentsOf: [0])

    let x0 = emitElementAddr(storage, at: [0], at: site)
    emitStore(int: size, to: x0, at: site)

    let x1 = emitElementAddr(storage, at: [1, 0], at: site)
    let x2 = append(module.makeBorrow(.set, from: x1, at: site))[0]
    append(module.makeStore(.constant(utf8), at: x2, at: site))
  }

  /// Inserts the IR for given constructor `call`, which initializes storage `r` by applying
  /// initializer `d` parameterized by `a`.
  ///
  /// - Parameters:
  ///   - call: The syntax of the call.
  ///   - s: The address of uninitialized storage typed by the receiver of `call`. This storage is
  ///     borrowed for initialization after evaluating `call`'s arguments and before the call.
  private mutating func emit(
    initializerCall call: FunctionCallExpr.Typed, initializing s: Operand
  ) {
    let callee = NameExpr.Typed(call.callee)!
    guard case .constructor(let d, let a) = callee.declaration else { preconditionFailure() }

    // Handle memberwise constructor calls.
    if program.ast[d].isMemberwise {
      emit(memberwiseInitializerCall: call, initializing: s)
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

  /// Inserts the IR for given memberwise constructor `call`, which initializes `receiver`.
  ///
  /// - Parameters:
  ///   - call: The syntax of the call.
  ///   - receiver: The address of uninitialized storage typed by the receiver of `d`. This storage
  ///     is borrowed for initialization after evaluating `call`'s arguments.
  private mutating func emit(
    memberwiseInitializerCall call: FunctionCallExpr.Typed, initializing receiver: Operand
  ) {
    let callee = LambdaType(program.relations.canonical(call.callee.type))!

    if callee.inputs.isEmpty {
      let x0 = append(module.makeUnsafeCast(.void, to: call.type, at: call.site))[0]
      let x1 = append(module.makeBorrow(.set, from: receiver, at: call.site))[0]
      append(module.makeStore(x0, at: x1, at: call.site))
      return
    }

    for i in callee.inputs.indices {
      // TODO: Handle remote types
      let p = ParameterType(callee.inputs[i].type)!
      if p.bareType.base is RemoteType {
        fatalError("not implemented")
      }

      let s = emitElementAddr(receiver, at: [i], at: call.site)
      emitStore(value: program[call.arguments[i].value], to: s)
    }
  }

  /// Inserts the IR for `arguments`, which is an argument passed to a function of type `callee`.
  ///
  /// - Parameters:
  ///   - syntheticSite: The site at which default pragma arguments are anchored.
  private mutating func emit(
    arguments: [LabeledArgument], to callee: AnyExprID.TypedNode,
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
    argument syntax: AnyExprID.TypedNode, to parameter: ParameterType, at site: SourceRange? = nil
  ) -> Operand {
    let argumentSite: SourceRange
    let storage: Operand

    if let e = PragmaLiteralExpr.Typed(syntax) {
      argumentSite = site ?? syntax.site
      storage = append(module.makeAllocStack(e.type, at: argumentSite))[0]
      emitStore(pragma: e, to: storage, at: argumentSite)
    } else {
      argumentSite = syntax.site
      storage = emitLValue(syntax)
    }

    return emitAcquire(parameter.access, on: storage, at: argumentSite)
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
      emitStore(foldedSequence: e, to: storage)

    case .leaf(let e):
      storage = emitLValue(program[e])
    }

    return emitAcquire(access, on: storage, at: program.ast.site(of: e))
  }

  /// Emits the IR of a call to `f` with given `arguments` at `site`.
  private mutating func emit(
    apply f: BuiltinFunction, to arguments: [LabeledArgument], at site: SourceRange
  ) -> Operand {
    switch f.name {
    case .llvm(let n):
      var a: [Operand] = []
      for e in arguments {
        let x0 = emitStore(value: program[e.value])
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
  private mutating func emit(
    callee: AnyExprID.TypedNode
  ) -> (callee: Operand, captures: [Operand]) {
    switch callee.kind {
    case NameExpr.self:
      return emit(namedCallee: .init(callee)!)

    case InoutExpr.self:
      // TODO: Handle the mutation marker, somehow.
      return emit(callee: InoutExpr.Typed(callee)!.subject)

    default:
      let f = emit(lambdaCallee: callee)
      return (f, [])
    }
  }

  /// Inserts the IR for given `callee` and returns `(c, a)`, where `c` is the callee's value and
  /// `a` are arguments to lifted parameters.
  ///
  /// - Requires: `callee` has a lambda type.
  private mutating func emit(
    namedCallee callee: NameExpr.Typed
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
      let f = emit(lambdaCallee: .init(callee))
      return (f, [])
    }
  }

  /// Inserts the IR for given `callee` and returns its value.
  ///
  /// - Requires: `callee` has a lambda type.
  private mutating func emit(lambdaCallee callee: AnyExprID.TypedNode) -> Operand {
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
        let test = pushing(Frame(), { $0.emit(branchCondition: condition) })
        let next = module.appendBlock(in: scope, to: f)
        append(module.makeCondBranch(if: test, then: next, else: failure, at: program[e].site))
        insertionBlock = next

      case .decl:
        fatalError("not implemented")
      }
    }

    return (success: insertionBlock!, failure: failure)
  }

  /// Inserts the IR for branch condition `e`.
  ///
  /// - Requires: `e.type` is `Val.Bool`
  private mutating func emit(branchCondition e: AnyExprID.TypedNode) -> Operand {
    precondition(program.relations.canonical(e.type) == program.ast.coreType("Bool")!)
    let x0 = emitLValue(e)
    let x1 = emitElementAddr(x0, at: [0], at: e.site)
    let x2 = append(module.makeLoad(x1, at: e.site))[0]
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

  /// Returns an existential container of type `t` borrowing `access` on `witness`.
  private mutating func emitExistential(
    _ t: ExistentialType, borrowing access: AccessEffect, from witness: Operand,
    at site: SourceRange
  ) -> Operand {
    let witnessTable = emitWitnessTable(of: module.type(of: witness).ast, usedIn: insertionScope!)
    let g = PointerConstant(module.syntax.id, module.addGlobal(witnessTable))

    let x0 = append(module.makeBorrow(access, from: witness, at: site))[0]
    let x1 = append(module.makeWrapAddr(x0, .constant(g), as: t, at: site))[0]
    return x1
  }

  /// Returns the witness table of `t` in `s`.
  private mutating func emitWitnessTable(of t: AnyType, usedIn s: AnyScopeID) -> WitnessTable {
    .init(for: t, conformingTo: loweredConformances(of: t, exposedTo: s))
  }

  // MARK: l-values

  /// Inserts the IR for the lvalue `syntax` block.
  private mutating func emitLValue(_ syntax: AnyExprID.TypedNode) -> Operand {
    switch syntax.kind {
    case CastExpr.self:
      return emitLValue(cast: .init(syntax)!)
    case InoutExpr.self:
      return emitLValue(inoutExpr: .init(syntax)!)
    case NameExpr.self:
      return emitLValue(name: .init(syntax)!)
    case TupleMemberExpr.self:
      return emitLValue(tupleMember: .init(syntax)!)
    default:
      return emitStore(value: syntax)
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
    return emitElementAddr(base, at: [syntax.index.value], at: syntax.index.site)
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
      return emitElementAddr(receiver, at: [i], at: site)

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

  /// Inserts the IR for aquiring `access` on `source` at `site`.
  private mutating func emitAcquire(
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
