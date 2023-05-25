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
    precondition(program.isAtModuleScope(d.id))
    switch d.kind {
    case BindingDecl.self:
      emit(globalBindingDecl: .init(d)!, into: &module)
    case ConformanceDecl.self:
      emit(conformanceDecl: .init(d)!, into: &module)
    case ExtensionDecl.self:
      emit(extensionDecl: .init(d)!, into: &module)
    case FunctionDecl.self:
      emit(functionDecl: .init(d)!, into: &module)
    case OperatorDecl.self:
      break
    case NamespaceDecl.self:
      emit(namespaceDecl: .init(d)!, into: &module)
    case ProductTypeDecl.self:
      emit(productDecl: .init(d)!, into: &module)
    case TraitDecl.self:
      emit(traitDecl: .init(d)!, into: &module)
    case TypeAliasDecl.self:
      break
    default:
      unexpected(d)
    }
  }

  /// Inserts the IR for `d` into `module`.
  private mutating func emit(conformanceDecl d: ConformanceDecl.Typed, into module: inout Module) {
    emit(members: d.members, into: &module)
  }

  /// Inserts the IR for `d` into `module`.
  private mutating func emit(extensionDecl d: ExtensionDecl.Typed, into module: inout Module) {
    emit(members: d.members, into: &module)
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
    let f = module.demandFunctionDeclaration(lowering: d)
    guard let b = d.body else {
      if d.isForeignInterface { emitFFI(d, into: &module) }
      return f
    }

    // Create the function entry.
    let entry = module.appendEntry(to: f)

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
    self.frames.push(.init(scope: AnyScopeID(d.id), locals: locals))

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
    let f = module.demandFunctionDeclaration(lowering: d)
    let entry = module.appendEntry(to: f)
    insertionBlock = entry
    frames.push(.init(scope: AnyScopeID(d.id)))
    defer {
      frames.pop()
      assert(frames.isEmpty)
    }

    // Emit FFI call.
    var arguments: [Operand] = []
    for i in module[entry].inputs.indices {
      let a = emitConvertToForeign(.parameter(entry, i), at: d.site, into: &module)
      arguments.append(a)
    }

    let output = module.functions[f]!.output
    let foreignResult = module.append(
      module.makeCallFFI(
        returning: .object(output),
        applying: d.foreignName!,
        to: arguments, anchoredAt: d.site),
      to: insertionBlock!)[0]

    // Handle FFIs without return values.
    if output.isVoidOrNever {
      module.append(module.makeReturn(.void, anchoredAt: d.site), to: insertionBlock!)
      return
    }

    let v = emitConvert(foreign: foreignResult, to: output, at: d.site, into: &module)
    emitStackDeallocs(in: &module, site: d.site)
    module.append(
      module.makeReturn(v, anchoredAt: d.site),
      to: insertionBlock!)
  }

  /// Inserts the IR for `d` into `module`.
  private mutating func emit(
    initializerDecl d: InitializerDecl.Typed,
    into module: inout Module
  ) {
    if d.isMemberwise { return }
    let f = module.demandInitializerDeclaration(lowering: d)
    let entry = module.appendEntry(to: f)
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

  /// Inserts the IR for `d` into `module`.
  private mutating func emit(subscriptDecl d: SubscriptDecl.Typed, into module: inout Module) {
    for i in d.impls {
      emit(subscriptImpl: i, into: &module)
    }
  }

  /// Inserts the IR for `d` into `module`.
  private mutating func emit(subscriptImpl d: SubscriptImpl.Typed, into module: inout Module) {
    let f = module.demandSubscriptDeclaration(lowering: d)
    guard let b = d.body else { return }

    // Create the function entry.
    let entry = module.appendEntry(to: f)

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
    self.frames.push(.init(scope: AnyScopeID(d.id), locals: locals))

    // Emit the body.
    switch b {
    case .block(let s):
      emit(stmt: program[s], into: &module)

    case .expr(let e):
      let s = emitLValue(program[e], into: &module)
      let b = module.append(
        module.makeBorrow(d.introducer.value, from: s, anchoredAt: program.ast[e].site),
        to: insertionBlock!)[0]
      module.append(
        module.makeYield(d.introducer.value, b, anchoredAt: program.ast[e].site),
        to: insertionBlock!)
      emitStackDeallocs(in: &module, site: program.ast[e].site)
      module.append(
        module.makeReturn(.void, anchoredAt: program.ast[e].site),
        to: insertionBlock!)
    }

    frames.pop()
    assert(frames.isEmpty)
  }

  /// Inserts the IR for `decl` into `module`.
  private mutating func emit(namespaceDecl decl: NamespaceDecl.Typed, into module: inout Module) {
    for m in decl.members {
      emit(topLevel: m, into: &module)
    }
  }

  /// Inserts the IR for `d` into `module`.
  private mutating func emit(productDecl d: ProductTypeDecl.Typed, into module: inout Module) {
    _ = module.addGlobal(MetatypeConstant(.init(d.type)!))
    emit(members: d.members, into: &module)
  }

  /// Inserts the IR for `d` into `module`.
  private mutating func emit(traitDecl d: TraitDecl.Typed, into module: inout Module) {
    _ = module.addGlobal(MetatypeConstant(.init(d.type)!))
  }

  /// Inserts the IR for given declaration `members` into `module`.
  private mutating func emit(members: [AnyDeclID], into module: inout Module) {
    for m in members {
      switch m.kind {
      case FunctionDecl.self:
        emit(functionDecl: .init(program[m])!, into: &module)
      case InitializerDecl.self:
        emit(initializerDecl: .init(program[m])!, into: &module)
      case SubscriptDecl.self:
        emit(subscriptDecl: .init(program[m])!, into: &module)
      default:
        continue
      }
    }
  }

  /// Inserts the IR for `d` into `module`.
  ///
  /// - Requires: `d` is a global binding.
  private mutating func emit(globalBindingDecl d: BindingDecl.Typed, into module: inout Module) {
    fatalError("not implemented")
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

    // Allocate storage for all the names declared by `decl`.
    let storage = emitAllocStack(for: decl.type, at: decl.site, into: &module)

    // Declare each introduced name and initialize them if possible.
    let lhs = decl.pattern.subpattern.id
    if let initializer = decl.initializer {
      program.ast.walking(pattern: lhs, expression: initializer.id) { (path, p, rhs) in
        // Declare the introduced name if `p` is a name pattern. Otherwise, drop the value of the
        // the corresponding expression.
        if let name = NamePattern.ID(p) {
          declare(name: program[name], referringTo: path, initializedTo: rhs)
        } else {
          let part = emitRValue(program[rhs], into: &module)
          module.append(
            module.makeDeinit(part, anchoredAt: program.ast[p].site),
            to: insertionBlock!)
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
      emitInitialization(of: sublocation, to: rhs, into: &module)
    }

    /// Inserts the IR to declare `name`, which refers to the sub-location at `pathInStorage`,
    /// returning that sub-location.
    func declare(name: NamePattern.Typed, referringTo pathInStorage: PartPath) -> Operand {
      let sublocation = module.append(
        module.makeElementAddr(storage, at: pathInStorage, anchoredAt: name.site),
        to: insertionBlock!)[0]
      frames[name.decl] = sublocation
      return sublocation
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
        frames[name.decl] = .constant(Poison(type: .address(name.decl.type)))
      }
      return
    }

    // Initializing inout bindings requires a mutation marker.
    if (capability == .inout) && (initializer.kind != InoutExpr.self) {
      report(.error(inoutBindingRequiresMutationMarkerAt: .empty(at: initializer.site.first())))
    }

    let source = emitLValue(initializer, into: &module)
    let isSourceSinkable = module.isSinkable(source, in: insertionBlock!.function)

    for (path, name) in decl.pattern.subpattern.names {
      var part = emitElementAddr(source, at: path, anchoredAt: name.decl.site, into: &module)
      let partType = module.type(of: part).ast

      if !program.relations.areEquivalent(name.decl.type, partType) {
        if let u = ExistentialType(name.decl.type) {
          let box = emitExistential(
            u, borrowing: capability, from: part,
            at: name.decl.site, into: &module)
          part = box
        }
      }

      if isSourceSinkable {
        let b = module.makeAccess(
          [.sink, capability], from: part, correspondingTo: name.decl,
          anchoredAt: name.decl.site)
        frames[name.decl] = module.append(b, to: insertionBlock!)[0]
      } else {
        let b = module.makeBorrow(
          capability, from: part, correspondingTo: name.decl,
          anchoredAt: name.decl.site)
        frames[name.decl] = module.append(b, to: insertionBlock!)[0]
      }
    }
  }

  /// Returns an existential container of type `t` borrowing `capability` from `witness`.
  private mutating func emitExistential(
    _ t: ExistentialType,
    borrowing capability: AccessEffect,
    from witness: Operand,
    at site: SourceRange,
    into module: inout Module
  ) -> Operand {
    let witnessTable = emitWitnessTable(
      of: module.type(of: witness).ast, usedIn: frames.top.scope, into: &module)
    let g = PointerConstant(module.syntax.id, module.addGlobal(witnessTable))

    let x0 = module.append(
      module.makeBorrow(capability, from: witness, anchoredAt: site),
      to: insertionBlock!)[0]
    return module.append(
      module.makeWrapAddr(x0, .constant(g), as: t, anchoredAt: site),
      to: insertionBlock!)[0]
  }

  /// Returns the witness table of `t` in `s`.
  private mutating func emitWitnessTable(
    of t: AnyType, usedIn s: AnyScopeID, into module: inout Module
  ) -> WitnessTable {
    .init(for: t, conformingTo: emitConformances(of: t, exposedTo: s, into: &module))
  }

  /// Returns the lowered conformances of `model` that are exposed to `useScope`.
  private mutating func emitConformances(
    of model: AnyType,
    exposedTo useScope: AnyScopeID,
    into module: inout Module
  ) -> Set<LoweredConformance> {
    guard let conformances = program.relations.conformances[model] else { return [] }

    var result: Set<LoweredConformance> = []
    for concept in conformances.keys {
      let c = program.conformance(of: model, to: concept, exposedTo: useScope)!
      result.insert(emitConformance(c, in: useScope, into: &module))
    }
    return result
  }

  /// Returns the lowered form of `c`, generating function references in `useScope`.
  private mutating func emitConformance(
    _ c: Conformance, in useScope: AnyScopeID, into module: inout Module
  ) -> LoweredConformance {
    var implementations = LoweredConformance.ImplementationMap()
    for (r, i) in c.implementations.storage {
      switch i {
      case .concrete(let d):
        implementations[r] = emitRequirementImplementation(d, in: useScope, into: &module)

      case .synthetic(let d):
        let f = emit(synthesizedDecl: d, into: &module)
        implementations[r] = .function(.init(to: f, usedIn: useScope, in: module))
      }
    }

    return .init(concept: c.concept, source: c.source, implementations: implementations)
  }

  /// Returns the lowered form of the requirement implementation `d` in `useScope`.
  private func emitRequirementImplementation(
    _ d: AnyDeclID, in useScope: AnyScopeID, into module: inout Module
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

  /// Inserts the IR for the top-level declaration `d` into `module`.
  @discardableResult
  private mutating func emit(
    synthesizedDecl d: SynthesizedDecl, into module: inout Module
  ) -> Function.ID {
    switch d.kind {
    case .moveInitialization:
      return synthesizeMoveInitImplementation(typed: .init(d.type)!, in: d.scope, into: &module)
    case .moveAssignment:
      return synthesizeMoveAssignImplementation(typed: .init(d.type)!, in: d.scope, into: &module)
    case .copy:
      fatalError("not implemented")
    }
  }

  /// Synthesize the implementation of `t`'s move initialization operator in `scope`.
  private mutating func synthesizeMoveInitImplementation(
    typed t: LambdaType,
    in scope: AnyScopeID,
    into module: inout Module
  ) -> Function.ID {
    let f = Function.ID(synthesized: program.ast.moveRequirement(.set), for: ^t)
    module.declareSyntheticFunction(f, typed: t)
    if (module[f].entry != nil) || (program.module(containing: scope) != module.syntax.id) {
      return f
    }

    let site = module.syntax.site
    let entry = module.appendEntry(to: f)
    insertionBlock = entry

    let receiver = Operand.parameter(entry, 0)
    let argument = Operand.parameter(entry, 1)

    switch program.relations.canonical(t.output).base {
    case is ProductType, is TupleType:
      let layout = AbstractTypeLayout(of: module.type(of: receiver).ast, definedIn: program)

      // Emit a load/store of the argument if it doesn't have any stored property.
      if layout.properties.isEmpty {
        let v = module.append(
          module.makeLoad(argument, anchoredAt: site),
          to: insertionBlock!)[0]
        module.append(
          module.makeStore(v, at: receiver, anchoredAt: site),
          to: insertionBlock!)
        break
      }

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

    module.append(module.makeReturn(.void, anchoredAt: site), to: insertionBlock!)
    return f
  }

  /// Synthesize the implementation of `t`'s move assignment operator in `scope`.
  private mutating func synthesizeMoveAssignImplementation(
    typed t: LambdaType,
    in scope: AnyScopeID,
    into module: inout Module
  ) -> Function.ID {
    let f = Function.ID(synthesized: program.ast.moveRequirement(.inout), for: ^t)
    module.declareSyntheticFunction(f, typed: t)
    if (module[f].entry != nil) || (program.module(containing: scope) != module.syntax.id) {
      return f
    }

    let site = module.syntax.site
    let entry = module.appendEntry(to: f)
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

    module.append(module.makeReturn(.void, anchoredAt: site), to: insertionBlock!)
    return f
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
    case YieldStmt.self:
      emit(yieldStmt: YieldStmt.Typed(stmt)!, into: &module)
    default:
      unexpected(stmt)
    }
  }

  private mutating func emit(assignStmt stmt: AssignStmt.Typed, into module: inout Module) {
    // The left operand of an assignment should always be marked for mutation, even if the
    // statement actually denotes initialization.
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
      emitInitialization(of: lhs, to: rhs, at: stmt.site, into: &module)
      return
    }

    let c = program.conformance(of: l, to: program.ast.sinkableTrait, exposedTo: frames.top.scope)!
    let m = module.makeMove(rhs, to: lhs, usingConformance: c, anchoredAt: stmt.site)
    module.append(m, to: insertionBlock!)
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
      value = .void
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

  private mutating func emit(yieldStmt stmt: YieldStmt.Typed, into module: inout Module) {
    // TODO: Read mutability of current subscript

    let s = emitLValue(program[stmt.value], into: &module)
    let b = module.append(
      module.makeBorrow(.let, from: s, anchoredAt: stmt.site),
      to: insertionBlock!)[0]
    module.append(
      module.makeYield(.let, b, anchoredAt: stmt.site),
      to: insertionBlock!)
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
    case PragmaLiteralExpr.self:
      return emitRValue(pragma: PragmaLiteralExpr.Typed(expr)!, into: &module)
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
    let bool = program.ast.coreType("Bool")!
    let s = module.makeRecord(bool, aggregating: [.i1(expr.value)], anchoredAt: expr.site)
    return module.append(s, to: insertionBlock!)[0]
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
    if let s = resultStorage {
      emitInitialization(of: s, to: expr.success, into: &module)
    } else {
      _ = emitRValue(program[expr.success], into: &module)
    }
    emitStackDeallocs(in: &module, site: expr.site)
    frames.pop()
    module.append(module.makeBranch(to: tail, anchoredAt: expr.site), to: insertionBlock!)

    // Emit the failure branch.
    insertionBlock = secondBranch
    let i = frames.top.allocs.count
    if let s = resultStorage {
      emitInitialization(of: s, to: expr.failure, into: &module)
    } else {
      _ = emitRValue(program[expr.failure], into: &module)
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
      return .void
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
    // Handle built-ins and constructor calls.
    if let n = NameExpr.Typed(expr.callee) {
      switch n.declaration {
      case .builtinFunction(let f):
        return emit(apply: f, to: expr.arguments, at: expr.site, into: &module)

      case .constructor:
        let s = emitAllocStack(for: expr.type, at: expr.site, into: &module)
        emitInitializerCall(expr, initializing: s, into: &module)
        return module.append(
          module.makeLoad(s, anchoredAt: expr.site),
          to: insertionBlock!)[0]

      default:
        break
      }
    }

    // Arguments are evaluated first, from left to right.
    let syntheticSite = expr.site.file.emptyRange(at: expr.site.end)
    let arguments = emit(
      arguments: expr.arguments, to: expr.callee, synthesizingDefaultArgumentsAt: syntheticSite,
      into: &module)

    // Callee and captures are evaluated next.
    let (callee, captures) = emitCallee(expr.callee, into: &module)

    // Call is evaluated last.
    return module.append(
      module.makeCall(applying: callee, to: captures + arguments, anchoredAt: expr.site),
      to: insertionBlock!)[0]
  }

  /// Inserts the IR for given constructor `call`, which initializes storage `r` by applying
  /// initializer `d` parameterized by `a`.
  ///
  /// - Parameters:
  ///   - call: The syntax of the call.
  ///   - s: The address of uninitialized storage typed by the receiver of `call`. This storage is
  ///     borrowed for initialization after evaluating `call`'s arguments and before the call.
  private mutating func emitInitializerCall(
    _ call: FunctionCallExpr.Typed,
    initializing s: Operand,
    into module: inout Module
  ) {
    let callee = NameExpr.Typed(call.callee)!
    guard case .constructor(let d, let a) = callee.declaration else { preconditionFailure() }

    // Handle memberwise constructor calls.
    if program.ast[d].isMemberwise {
      emitMemberwiseInitializerCall(call, initializing: s, into: &module)
      return
    }

    // Evaluate all arguments.
    let syntheticSite = call.site.file.emptyRange(at: call.site.end)
    let arguments = emit(
      arguments: call.arguments, to: call.callee, synthesizingDefaultArgumentsAt: syntheticSite,
      into: &module)

    // Initialize storage.
    let f = FunctionReference(
      to: program[d], parameterizedBy: a, usedIn: frames.top.scope, in: &module)
    let receiver = module.append(
      module.makeBorrow(.set, from: s, anchoredAt: call.site),
      to: insertionBlock!)[0]
    module.append(
      module.makeCall(applying: .constant(f), to: [receiver] + arguments, anchoredAt: call.site),
      to: insertionBlock!)
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
    initializing s: Operand,
    into module: inout Module
  ) {
    let callee = LambdaType(call.callee.type)!
    var arguments: [Operand] = []
    for (p, e) in zip(callee.inputs, call.arguments) {
      let a = program[e.value]
      arguments.append(emit(argument: a, to: ParameterType(p.type)!, into: &module))
    }

    let x0 = module.append(
      module.makeRecord(callee.output, aggregating: arguments, anchoredAt: call.site),
      to: insertionBlock!)[0]
    let x1 = module.append(
      module.makeBorrow(.set, from: s, anchoredAt: call.site),
      to: insertionBlock!)[0]
    module.append(
      module.makeStore(x0, at: x1, anchoredAt: call.site),
      to: insertionBlock!)
  }

  /// Inserts the IR for `arguments`, which is an argument passed to a function of type `callee`,
  /// into `module` at the end of the current insertion block.
  ///
  /// - Parameters:
  ///   - syntheticSite: The site at which default pragma arguments are anchored.
  private mutating func emit(
    arguments: [LabeledArgument],
    to callee: AnyExprID.TypedNode,
    synthesizingDefaultArgumentsAt syntheticSite: SourceRange,
    into module: inout Module
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

      let v = emit(
        argument: program[a], to: ParameterType(p.type)!, at: syntheticSite, into: &module)
      result.append(v)
    }

    assert(i == arguments.count)
    return result
  }

  /// Inserts the IR for the argument `expr` passed to a parameter of type `parameter` into
  /// `module` at the end of the current insertion block.
  ///
  /// - Parameters:
  ///   - site: The source range in which `syntax` is being evaluated if it's a pragma literals.
  ///     Defaults to `syntax.site`.
  private mutating func emit(
    argument syntax: AnyExprID.TypedNode,
    to parameter: ParameterType,
    at site: SourceRange? = nil,
    into module: inout Module
  ) -> Operand {
    if let e = PragmaLiteralExpr.Typed(syntax) {
      let anchor = site ?? syntax.site
      let v = emitRValue(pragma: e, at: anchor, into: &module)

      switch parameter.access {
      case .let, .inout, .set:
        let s = emitLValue(converting: v, at: anchor, into: &module)
        let b = module.makeBorrow(parameter.access, from: s, anchoredAt: anchor)
        return module.append(b, to: insertionBlock!)[0]

      case .sink:
        return v

      default:
        fatalError("not implemented")
      }
    }

    switch parameter.access {
    case .let, .inout, .set:
      let s = emitLValue(syntax, into: &module)
      let b = module.makeBorrow(parameter.access, from: s, anchoredAt: syntax.site)
      return module.append(b, to: insertionBlock!)[0]

    case .sink:
      return emitRValue(syntax, into: &module)

    default:
      fatalError("not implemented")
    }
  }

  /// Emits the IR of a call to `f` with given `arguments` at `site` into `module`, inserting
  /// instructions at the end of `self.insertionBlock`.
  private mutating func emit(
    apply f: BuiltinFunction,
    to arguments: [LabeledArgument],
    at site: SourceRange,
    into module: inout Module
  ) -> Operand {
    switch f.name {
    case .llvm(let n):
      var a: [Operand] = []
      for e in arguments {
        a.append(emitRValue(program[e.value], into: &module))
      }
      return module.append(
        module.makeLLVM(applying: n, to: a, anchoredAt: site), to: insertionBlock!)[0]

    case .addressOf:
      let source = emitLValue(program[arguments[0].value], into: &module)
      return module.append(
        module.makeAddressToPointer(source, anchoredAt: site), to: insertionBlock!)[0]
    }
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
    _ = emit(functionDecl: e.decl, into: &module)
    let f = FunctionReference(to: program[e.decl], usedIn: frames.top.scope, in: &module)
    return module.append(
      module.makePartialApply(wrapping: f, with: .void, anchoredAt: e.site),
      to: insertionBlock!)[0]
  }

  private mutating func emitRValue(
    name e: NameExpr.Typed,
    into module: inout Module
  ) -> Operand {
    switch e.declaration {
    case .direct(let d, _):
      guard let s = frames[program[d]] else { fatalError("not implemented") }
      if module.type(of: s).isObject {
        return s
      } else {
        return module.append(module.makeLoad(s, anchoredAt: e.site), to: insertionBlock!)[0]
      }

    case .member(let d, _):
      return emitRValue(memberExpr: e, declaredBy: program[d], into: &module)

    case .constructor:
      fatalError("not implemented")

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
    // Member reference without a domain expression are implicitly bound to `self`.
    let base: Operand
    if let p = e.domainExpr {
      base = emitLValue(p, into: &module)
    } else {
      base = frames[receiver!]!
    }

    let l = AbstractTypeLayout(of: module.type(of: base).ast, definedIn: program)
    let i = l.offset(of: d.baseName)!

    let x0 = module.append(
      module.makeElementAddr(base, at: [i], anchoredAt: e.site),
      to: insertionBlock!)[0]
    return module.append(
      module.makeLoad(x0, anchoredAt: e.site),
      to: insertionBlock!)[0]
  }

  /// Inserts the IR `syntax` into `module`
  ///
  /// - Parameters:
  ///   - site: The source range in which `syntax` is being evaluated. Defaults to `syntax.site`.
  private mutating func emitRValue(
    pragma syntax: PragmaLiteralExpr.Typed,
    at site: SourceRange? = nil,
    into module: inout Module
  ) -> Operand {
    let s = site ?? syntax.site
    switch program.ast[syntax.id].kind {
    case .file:
      return emitString(s.file.url.absoluteURL.path, at: s, into: &module)
    case .line:
      return emitInt(s.first().line.number, at: s, into: &module)
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
      guard case .member(let calleeDecl, _) = program.referredDecls[callee.expr] else {
        unreachable()
      }
      let f = FunctionReference(
        to: program[FunctionDecl.ID(calleeDecl)!], usedIn: frames.top.scope, in: &module)

      // Emit the call.
      let call = module.makeCall(
        applying: .constant(f), to: [l, r], anchoredAt: program.ast.site(of: expr))
      return module.append(call, to: insertionBlock!)[0]

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
    emitString(syntax.value, at: syntax.site, into: &module)
  }

  private mutating func emitRValue(
    tuple syntax: TupleExpr.Typed,
    into module: inout Module
  ) -> Operand {
    if syntax.elements.isEmpty { return .void }

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
  ) -> (callee: Operand, captures: [Operand]) {
    switch callee.kind {
    case NameExpr.self:
      return emitNamedCallee(.init(callee)!, into: &module)

    case InoutExpr.self:
      // TODO: Handle the mutation marker, somehow.
      return emitCallee(InoutExpr.Typed(callee)!.subject, into: &module)

    default:
      let f = emitLambdaCallee(callee, into: &module)
      return (f, [])
    }
  }

  /// Inserts the IR for given `callee` into `module` at the end of the current insertion block and
  /// returns `(c, a)`, where `c` is the callee's value and `a` are arguments to lifted parameters.
  ///
  /// - Requires: `callee` has a lambda type.
  private mutating func emitNamedCallee(
    _ callee: NameExpr.Typed,
    into module: inout Module
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
        usedIn: frames.top.scope, in: &module)
      return (.constant(r), [])

    case .member(let d, let a) where d.kind == FunctionDecl.self:
      // Callee is a member reference to a function or method.
      let r = FunctionReference(
        to: program[FunctionDecl.ID(d)!], parameterizedBy: a,
        usedIn: frames.top.scope, in: &module)

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
        return (Operand.constant(r), i)
      } else {
        let i = module.append(
          module.makeLoad(receiver, anchoredAt: callee.site),
          to: insertionBlock!)
        return (Operand.constant(r), i)
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

  /// Inserts the IR for string `s` into `module` at the end of the current insertion block,
  /// anchoring instructions at site.
  private mutating func emitString(
    _ s: String,
    at site: SourceRange,
    into module: inout Module
  ) -> Operand {
    var bytes = s.unescaped.data(using: .utf8)!
    let size = emitWord(bytes.count, at: site, into: &module)
    bytes.append(contentsOf: [0])

    let p = PointerConstant(module.syntax.id, module.addGlobal(BufferConstant(bytes)))
    let base = emitCoreInstance(
      of: "RawPointer", aggregating: [.constant(p)], at: site, into: &module)
    return emitCoreInstance(of: "String", aggregating: [size, base], at: site, into: &module)
  }

  /// Inserts the IR for integer `i` into `module` at the end of the current insertion block,
  /// anchoring instructions at site.
  private mutating func emitInt(
    _ i: Int,
    at site: SourceRange,
    into module: inout Module
  ) -> Operand {
    let t = program.ast.coreType("Int")!
    let s = module.makeRecord(
      t, aggregating: [.constant(IntegerConstant(i, bitWidth: 64))], anchoredAt: site)
    return module.append(s, to: insertionBlock!)[0]
  }

  /// Inserts the IR for branch condition `expr` into `module` at the end of the current insertion
  /// block.
  ///
  /// - Requires: `expr.type` is `Val.Bool`
  private mutating func emitBranchCondition(
    _ expr: AnyExprID.TypedNode,
    into module: inout Module
  ) -> Operand {
    precondition(program.relations.canonical(expr.type) == program.ast.coreType("Bool")!)
    let x0 = emitLValue(expr, into: &module)
    let x1 = module.append(
      module.makeElementAddr(x0, at: [0], anchoredAt: expr.site),
      to: insertionBlock!)[0]
    let x2 = module.append(
      module.makeLoad(x1, anchoredAt: expr.site),
      to: insertionBlock!)[0]
    return x2
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
    case program.ast.coreType("Double")!:
      let v = FloatingPointConstant.double(s)
      return module.append(
        module.makeRecord(literalType, aggregating: [.constant(v)], anchoredAt: anchor),
        to: insertionBlock!)[0]

    case program.ast.coreType("Float")!:
      let v = FloatingPointConstant.float(s)
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
        .error(integerLiteral: s, overflowsWhenStoredInto: literalType, at: anchor))
      return .constant(Poison(type: .object(literalType)))
    }

    return module.append(
      module.makeRecord(
        literalType, aggregating: [.constant(IntegerConstant(b))], anchoredAt: anchor),
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
    let t = program.ast.coreType(n)!
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
      aggregating: [.constant(IntegerConstant(value, bitWidth: 64))],
      at: site,
      into: &module)
  }

  /// Inserts the IR for converting `foreign` to a value of type `ir`.
  private mutating func emitConvert(
    foreign: Operand,
    to ir: AnyType,
    at site: SourceRange,
    into module: inout Module
  ) -> Operand {
    precondition(module.type(of: foreign).isObject)

    let useScope = frames.top.scope
    let foreignConvertible = program.ast.coreTrait("ForeignConvertible")!
    let foreignConvertibleConformance = program.conformance(
      of: ir, to: foreignConvertible, exposedTo: useScope)!
    let r = program.ast.requirements(
      Name(stem: "init", labels: ["foreign_value"]), in: foreignConvertible.decl)[0]

    switch foreignConvertibleConformance.implementations[r]! {
    case .concrete(let m):
      let convert = FunctionReference(
        to: program[InitializerDecl.ID(m)!], usedIn: useScope, in: &module)
      let x0 = module.append(
        module.makeAllocStack(ir, anchoredAt: site),
        to: insertionBlock!)[0]
      frames.top.allocs.append(x0)
      let x1 = module.append(
        module.makeBorrow(.set, from: x0, anchoredAt: site),
        to: insertionBlock!)[0]
      module.append(
        module.makeCall(
          applying: .constant(convert), to: [x1, foreign], anchoredAt: site),
        to: insertionBlock!)
      let x3 = module.append(
        module.makeLoad(x0, anchoredAt: site),
        to: insertionBlock!)[0]
      return x3

    case .synthetic:
      fatalError("not implemented")
    }
  }

  /// Appends the IR to convert `o` to a FFI argument.
  private mutating func emitConvertToForeign(
    _ o: Operand,
    at site: SourceRange,
    into module: inout Module
  ) -> Operand {
    let t = module.type(of: o)
    precondition(t.isAddress)

    let useScope = frames.top.scope
    let foreignConvertible = program.ast.coreTrait("ForeignConvertible")!
    let foreignConvertibleConformance = program.conformance(
      of: t.ast, to: foreignConvertible, exposedTo: useScope)!
    let r = program.ast.requirements("foreign_value", in: foreignConvertible.decl)[0]

    // TODO: Handle cases where the foreign representation of `t` is not built-in

    switch foreignConvertibleConformance.implementations[r]! {
    case .concrete(let m):
      let convert = FunctionReference(
        to: program[FunctionDecl.ID(m)!], usedIn: useScope, in: &module)
      let x0 = module.append(
        module.makeBorrow(.let, from: o, anchoredAt: site),
        to: insertionBlock!)
      let x1 = module.append(
        module.makeCall(applying: .constant(convert), to: x0, anchoredAt: site),
        to: insertionBlock!)[0]
      return x1

    case .synthetic:
      fatalError("not implemented")
    }
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
    case CastExpr.self:
      return emitLValue(cast: .init(syntax)!, into: &module)
    case InoutExpr.self:
      return emitLValue(inoutExpr: .init(syntax)!, into: &module)
    case NameExpr.self:
      return emitLValue(name: NameExpr.Typed(syntax)!, into: &module)
    case TupleMemberExpr.self:
      return emitLValue(name: TupleMemberExpr.Typed(syntax)!, into: &module)
    default:
      return emitLValue(converting: syntax, into: &module)
    }
  }

  /// Inserts the IR for `rvalue` converted as a lvalue into `module` at the end of the current
  /// insertion block.
  private mutating func emitLValue(
    converting rvalue: AnyExprID.TypedNode,
    into module: inout Module
  ) -> Operand {
    let t = program.relations.canonical(rvalue.type)
    let s = emitAllocStack(for: t, at: rvalue.site, into: &module)
    emitInitialization(of: s, to: rvalue.id, into: &module)
    return s
  }

  /// Inserts the IR for converting `rvalue` as a lvalue at `site`.
  private mutating func emitLValue(
    converting rvalue: Operand,
    at site: SourceRange,
    into module: inout Module
  ) -> Operand {
    let t = module.type(of: rvalue).ast
    let s = emitAllocStack(for: t, at: site, into: &module)
    emitInitialization(of: s, to: rvalue, at: site, into: &module)
    return s
  }

  private mutating func emitLValue(
    cast syntax: CastExpr.Typed,
    into module: inout Module
  ) -> Operand {
    switch syntax.direction {
    case .pointerConversion:
      let source = emitRValue(syntax.left, into: &module)
      let target = RemoteType(program.relations.canonical(syntax.type))!
      return module.append(
        module.makePointerToAddress(source, to: target, anchoredAt: syntax.site),
        to: insertionBlock!)[0]

    default:
      fatalError("not implemented")
    }
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
    switch syntax.declaration {
    case .direct(let d, _):
      return emitLValue(directReferenceTo: program[d], into: &module)

    case .member(let d, _):
      let r = emitLValue(receiverOf: syntax, into: &module)
      return emitProperty(boundTo: r, declaredBy: program[d], into: &module, at: syntax.site)

    case .constructor:
      fatalError()

    case .builtinFunction, .builtinType:
      // Built-in functions and types are never used as l-value.
      unreachable()
    }
  }

  /// Inserts the IR denoting a direct reference to `d` at the end of the current insertion block.
  private func emitLValue(
    directReferenceTo d: AnyDeclID.TypedNode,
    into module: inout Module
  ) -> Operand {
    // Check if `d` is a local.
    if let s = frames[d] { return s }

    switch d.kind {
    case ProductTypeDecl.self:
      let t = MetatypeType(of: d.type)
      let g = module.addGlobal(MetatypeConstant(MetatypeType(d.type)!))
      let s = module.makeGlobalAddr(
        of: g, in: module.syntax.id, typed: ^MetatypeType(of: t), anchoredAt: d.site)
      return module.append(s, to: insertionBlock!)[0]

    default:
      fatalError("not implemented")
    }
  }

  /// Inserts the IR denoting the domain of `syntax` into `module` at the end of the current
  /// insertion block.
  private mutating func emitLValue(
    receiverOf syntax: NameExpr.Typed,
    into module: inout Module
  ) -> Operand {
    switch syntax.domain {
    case .none:
      return frames[receiver!]!
    case .implicit:
      fatalError("not implemented")
    case .expr(let e):
      return emitLValue(e, into: &module)
    }
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

  /// Returns the address of the member declared by `d` and bound to `receiver`, inserting IR
  /// anchored at `anchor` into `module`.
  private mutating func emitProperty(
    boundTo receiver: Operand,
    declaredBy d: AnyDeclID.TypedNode,
    into module: inout Module,
    at anchor: SourceRange
  ) -> Operand {
    switch d.kind {
    case SubscriptDecl.self:
      return emitComputedProperty(
        boundTo: receiver, declaredByBundle: .init(d)!, into: &module, at: anchor)

    case VarDecl.self:
      let l = AbstractTypeLayout(of: module.type(of: receiver).ast, definedIn: program)
      let i = l.offset(of: VarDecl.Typed(d)!.baseName)!
      return module.append(
        module.makeElementAddr(receiver, at: [i], anchoredAt: anchor),
        to: insertionBlock!)[0]

    default:
      fatalError("not implemented")
    }
  }

  /// Returns the address of the computed property declared by `d` and bound to `receiver`,
  /// inserting IR anchored at `anchor` into `module`.
  private mutating func emitComputedProperty(
    boundTo receiver: Operand,
    declaredByBundle d: SubscriptDecl.Typed,
    into module: inout Module,
    at anchor: SourceRange
  ) -> Operand {
    // TODO: Handle generics

    if let i = d.impls.uniqueElement {
      return emitComputedProperty(boundTo: receiver, declaredBy: i, into: &module, at: anchor)
    }

    let t = SubscriptType(program.relations.canonical(d.type))!
    let r = module.append(
      module.makeAccess(t.capabilities, from: receiver, anchoredAt: anchor),
      to: insertionBlock!)[0]

    var variants: [AccessEffect: Function.ID] = [:]
    for v in d.impls {
      variants[v.introducer.value] = module.demandSubscriptDeclaration(lowering: v)
    }

    let p = module.makeProjectBundle(
      applying: variants, of: d.id, typed: t, to: [r],
      anchoredAt: anchor)
    return module.append(p, to: insertionBlock!)[0]
  }

  /// Returns the address of the computed property declared by `d` and bound to `receiver`,
  /// inserting IR anchored at `anchor` into `module`.
  private mutating func emitComputedProperty(
    boundTo receiver: Operand,
    declaredBy d: SubscriptImpl.Typed,
    into module: inout Module,
    at anchor: SourceRange
  ) -> Operand {
    let t = SubscriptImplType(d.type)!
    let o = RemoteType(d.introducer.value, program.relations.canonical(t.output))
    let r = module.append(
      module.makeBorrow(o.access, from: receiver, anchoredAt: anchor),
      to: insertionBlock!)[0]

    let f = module.demandSubscriptDeclaration(lowering: d)
    return module.append(
      module.makeProject(o, applying: f, to: [r], anchoredAt: anchor),
      to: insertionBlock!)[0]
  }

  // MARK: Helpers

  /// Inserts a stack allocation for an object of type `t`.
  private mutating func emitAllocStack(
    for t: AnyType, at site: SourceRange, into module: inout Module
  ) -> Operand {
    let u = program.relations.canonical(t)
    let s = module.append(
      module.makeAllocStack(u, anchoredAt: site),
      to: insertionBlock!)[0]
    frames.top.allocs.append(s)
    return s
  }

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
  /// block into `module`.
  private mutating func emitInitialization(
    of storage: Operand,
    to value: AnyExprID,
    into module: inout Module
  ) {
    if let tuple = TupleExpr.ID(value) {
      emitInitialization(of: storage, to: tuple, into: &module)
    } else if let call = FunctionCallExpr.ID(value),
      let n = NameExpr.ID(program.ast[call].callee),
      case .constructor = program.referredDecls[n]!
    {
      emitInitializerCall(program[call], initializing: storage, into: &module)
    } else {
      let v = emitRValue(program[value], into: &module)
      // TODO: Replace by emitMove
      emitInitialization(of: storage, to: v, at: program.ast[value].site, into: &module)
    }
  }

  /// Inserts the IR for initializing `storage` with `value` at the end of the current insertion
  /// block into `module`.
  private mutating func emitInitialization(
    of storage: Operand,
    to value: TupleExpr.ID,
    into module: inout Module
  ) {
    for (i, e) in program.ast[value].elements.enumerated() {
      let s = module.append(
        module.makeElementAddr(storage, at: [i], anchoredAt: program.ast[e.value].site),
        to: insertionBlock!)[0]
      emitInitialization(of: s, to: e.value, into: &module)
    }
  }

  /// Inserts the IR for initializing `storage` with `value` at the end of the current insertion
  /// block, anchoring new instructions at `site` into `module`.
  private mutating func emitInitialization(
    of storage: Operand,
    to value: Operand,
    at site: SourceRange,
    into module: inout Module
  ) {
    let s = module.append(
      module.makeBorrow(.set, from: storage, anchoredAt: site),
      to: insertionBlock!)[0]
    module.append(
      module.makeStore(value, at: s, anchoredAt: site),
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
    let f = module.demandMoveOperatorDeclaration(access, from: c)
    let move = FunctionReference(to: f, usedIn: c.scope, in: module)

    let r = module.append(
      module.makeBorrow(access, from: storage, anchoredAt: anchor),
      to: insertionBlock!)[0]
    module.append(
      module.makeCall(applying: .constant(move), to: [r, value], anchoredAt: anchor),
      to: insertionBlock!)
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

}
