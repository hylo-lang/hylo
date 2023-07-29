import BigInt
import Core
import FrontEnd
import Utils

/// Val's IR emitter.
///
/// The emitter transforms well-formed, typed ASTs to a representation suitable for flow-sensitive
/// analysis. IR generated from the emitter may be incomplete and must go through mandatory passes
/// before it can be fed to code generation.
///
/// You create an instance by calling `Emitter.withInstance`, providing the module in which IR
/// should be incorporated and a diagnostic log. Then, the two main entry points are:
///
/// - `incorporateTopLevelDeclarations`: incorporates the top-level declarations of a module's
///   AST into its corresponding IR form.
/// - `incorporateSyntheticDeclarations`: generates the implementations of the synthetic
///   declarations that are notionally part of a module. This method should be called after
///   definitie deinitialization.
///
/// Other entry points may be used during IR passes (e.g., `emitDeinit`).
///
/// - Note: Unless documented otherwise, methods insert IR in `self.module` at `insertionPoint`.
struct Emitter {

  /// The diagnostics of lowering errors.
  private var diagnostics: DiagnosticSet = []

  /// The module into which new IR is inserted.
  private var module: Module!

  /// A stack of frames describing the variables and allocations of each traversed lexical scope.
  private var frames = Stack()

  /// Where new instructions are inserted.
  var insertionPoint: InsertionPoint?

  /// The program being lowered.
  private var program: TypedProgram {
    module.program
  }

  /// The AST of the program being lowered.
  private var ast: AST {
    program.ast
  }

  /// The basic block in which new instructions are currently inserted.
  private var insertionBlock: Block.ID? {
    insertionPoint?.block
  }

  /// The function containing the current insertion block.
  private var insertionFunction: Function.ID? {
    insertionBlock?.function
  }

  /// The scope corresponding to the current insertion block.
  private var insertionScope: AnyScopeID? {
    insertionBlock.map({ module[$0].scope })
  }

  /// The address of the return value in the current function, if any.
  private var returnValue: Operand? {
    guard
      let f = insertionFunction,
      let b = module.entry(of: f),
      !module[f].isSubscript
    else { return nil }
    return .parameter(b, module[f].inputs.count)
  }

  /// Reports the given diagnostic.
  private mutating func report(_ d: Diagnostic) {
    diagnostics.insert(d)
  }

  /// Appends a new basic block at the end of `self.insertionBlock.function`.
  private mutating func appendBlock() -> Block.ID {
    module.appendBlock(in: insertionScope!, to: insertionFunction!)
  }

  /// Inserts `newInstruction` into `self.module` at the end of `self.insertionPoint`.
  ///
  /// - Requires: `self.insertionPoint` refers to the end of a block.
  @discardableResult
  private mutating func insert<I: Instruction>(_ newInstruction: I) -> Operand? {
    let i = module.insert(newInstruction, at: insertionPoint!)
    return module.result(of: i)
  }

  // MARK: Entry points

  /// Calls `action` with an emitter configured to generate IR in `module` and accumulate
  /// diagnostics in `log`.
  static func withInstance<T>(
    insertingIn module: inout Module,
    reportingDiagnosticsTo log: inout DiagnosticSet,
    action: (inout Emitter) -> T
  ) -> T {
    var instance = Self()
    instance.module = module
    swap(&instance.diagnostics, &log)

    defer {
      module = instance.module.release()
      swap(&instance.diagnostics, &log)
    }

    return action(&instance)
  }

  /// Inserts the IR for the top-level declarations of `self.module`.
  mutating func incorporateTopLevelDeclarations() {
    for u in program.ast.topLevelDecls(module.id) {
      lower(topLevel: u)
    }
  }

  /// Inserts the IR for the synthesized declarations of `self.module`.
  mutating func incorporateSyntheticDeclarations() {
    for d in program.synthesizedDecls[module.id, default: []] {
      lower(synthesized: d)
    }

    var i = 0
    while i < module.synthesizedDecls.count {
      lower(synthesized: module.synthesizedDecls[i])
      i += 1
    }
  }

  // MARK: Declarations

  /// Inserts the IR for the top-level declaration `d`.
  ///
  /// - Requires: `d` is at module scope.
  private mutating func lower(topLevel d: AnyDeclID) {
    precondition(program.isAtModuleScope(d))
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
    case SubscriptDecl.self:
      lower(subscript: .init(d)!)
    case TraitDecl.self:
      lower(trait: .init(d)!)
    case TypeAliasDecl.self:
      break
    default:
      unexpected(d, in: ast)
    }
  }

  /// Inserts the IR for `d`.
  private mutating func lower(conformance d: ConformanceDecl.ID) {
    lower(members: ast[d].members)
  }

  /// Inserts the IR for `d`.
  private mutating func lower(extension d: ExtensionDecl.ID) {
    lower(members: ast[d].members)
  }

  /// Inserts the IR for `d`, returning the ID of the lowered function.
  @discardableResult
  private mutating func lower(function d: FunctionDecl.ID) -> Function.ID {
    withClearContext({ $0.lowerInClearContext(function: d) })
  }

  /// Inserts the IR for `d`, returning the ID of the lowered function.
  ///
  /// - Requires: `self` has a clear lowering context.
  private mutating func lowerInClearContext(function d: FunctionDecl.ID) -> Function.ID {
    let f = module.demandFunctionDeclaration(lowering: d)
    guard let b = ast[d].body else {
      if ast[d].isForeignInterface { lower(ffi: d) }
      return f
    }

    // Configure the emitter context.
    let entry = module.appendEntry(in: program.scopeContainingBody(of: d)!, to: f)
    let bodyFrame = outermostFrame(of: d, entering: entry)
    self.insertionPoint = .end(of: entry)

    // Emit the body.
    switch b {
    case .block(let s):
      lower(body: s, of: d, in: bodyFrame)

    case .expr(let e):
      pushing(bodyFrame, { $0.emitStore(value: e, to: $0.returnValue!) })
      insert(module.makeReturn(at: ast[e].site))
    }

    return f
  }

  /// Returns the frame enclosing the body of `d`, whose entry block is `entry`.
  private func outermostFrame(of d: FunctionDecl.ID, entering entry: Block.ID) -> Frame {
    var locals = DeclProperty<Operand>()

    let explicit = program[d].explicitCaptures
    let implicit = program[d].implicitCaptures

    // Exlicit captures appear first.
    for (i, c) in explicit.enumerated() {
      locals[c] = .parameter(entry, i)
    }

    // Implicit captures appear next.
    for (i, c) in implicit.enumerated() {
      locals[c.decl] = .parameter(entry, i + explicit.count)
    }

    // Function receiver appears next.
    var captureCount = explicit.count + implicit.count
    if let r = ast[d].receiver {
      locals[r] = .parameter(entry, captureCount)
      captureCount += 1
    }

    // Explicit parameters appear last.
    for (i, p) in ast[d].parameters.enumerated() {
      locals[p] = .parameter(entry, i + captureCount)
    }

    return Frame(locals: locals)
  }

  /// Inserts the IR for `b`, which is the body of `d` and is enclosed in frame `f`.
  private mutating func lower(body b: BraceStmt.ID, of d: FunctionDecl.ID, in f: Frame) {
    switch pushing(f, { $0.emit(braceStmt: b) }) {
    case .next:
      if !module[insertionFunction!].output.isVoidOrNever {
        let t = LambdaType(program[d].type)!.output
        report(.error(missingReturn: t, at: .empty(atEndOf: ast[b].site)))
      } else {
        emitStore(value: .void, to: returnValue!, at: .empty(atEndOf: ast[b].site))
        insert(module.makeReturn(at: .empty(atEndOf: ast[b].site)))
      }

    case .return(let s):
      insert(module.makeReturn(at: ast[s].site))

    default:
      fatalError("not implemented")
    }
  }

  /// Inserts the IR for calling `d`, which is a foreign function interface.
  private mutating func lower(ffi d: FunctionDecl.ID) {
    let f = module.demandFunctionDeclaration(lowering: d)

    // Configure the emitter context.
    let entry = module.appendEntry(in: d, to: f)

    self.insertionPoint = .end(of: entry)
    self.frames.push()
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    let site = ast[d].site

    // Convert Val arguments to their foreign representation. Note that the last parameter of the
    // entry is the address of the FFI's return value.
    var arguments: [Operand] = []
    for i in 0 ..< module[entry].inputs.count - 1 {
      let a = emitConvertToForeign(.parameter(entry, i), at: site)
      arguments.append(a)
    }

    // Emit the call to the foreign function.
    let returnType = module.functions[f]!.output
    let foreignResult = insert(
      module.makeCallFFI(
        returning: .object(returnType), applying: ast[d].foreignName!, to: arguments, at: site))!

    // Convert the result of the FFI to its Val representation and return it.
    switch returnType {
    case .never:
      insert(module.makeUnreachable(at: site))

    case .void:
      emitStore(value: .void, to: returnValue!, at: site)
      emitDeallocTopFrame(at: site)
      insert(module.makeReturn(at: site))

    default:
      let v = emitConvert(foreign: foreignResult, to: returnType, at: site)
      emitStore(value: v, to: returnValue!, at: site)
      emitDeallocTopFrame(at: site)
      insert(module.makeReturn(at: site))
    }
  }

  /// Inserts the IR for `d`.
  private mutating func lower(initializer d: InitializerDecl.ID) {
    // Nothing to do for memberwise initializer.
    if ast[d].isMemberwise { return }
    let f = module.demandInitializerDeclaration(lowering: d)

    // Create the function entry.
    let entry = module.appendEntry(in: ast[d].body!, to: f)

    // Configure the locals.
    var locals = DeclProperty<Operand>()
    locals[ast[d].receiver] = .parameter(entry, 0)
    for (i, p) in ast[d].parameters.enumerated() {
      locals[p] = .parameter(entry, i + 1)
    }

    // Emit the body.
    insertionPoint = .end(of: entry)
    switch pushing(Frame(locals: locals), { $0.emit(braceStmt: $0.ast[d].body!) }) {
    case .next:
      emitStore(value: .void, to: returnValue!, at: .empty(atEndOf: ast[d].site))
      insert(module.makeReturn(at: .empty(atEndOf: ast[d].site)))

    case .return(let s):
      insert(module.makeReturn(at: ast[s].site))

    default:
      fatalError("not implemented")
    }
  }

  /// Inserts the IR for `d`.
  private mutating func lower(subscript d: SubscriptDecl.ID) {
    for i in ast[d].impls {
      lower(subscriptImpl: i)
    }
  }

  /// Inserts the IR for `d`.
  private mutating func lower(subscriptImpl d: SubscriptImpl.ID) {
    let f = module.demandSubscriptDeclaration(lowering: d)
    guard let b = ast[d].body else { return }

    // Create the function entry.
    let entry = module.appendEntry(in: program.scopeContainingBody(of: d)!, to: f)

    // Configure the locals.
    var locals = DeclProperty<Operand>()

    let bundle = SubscriptDecl.ID(program[d].scope)!
    let explicit = program[bundle].explicitCaptures
    let implicit = program[bundle].implicitCaptures

    for (i, c) in explicit.enumerated() {
      locals[c] = .parameter(entry, i)
    }
    for (i, c) in implicit.enumerated() {
      locals[c.decl] = .parameter(entry, i + explicit.count)
    }

    var captureCount = explicit.count + implicit.count
    if let r = ast[d].receiver {
      locals[r] = .parameter(entry, captureCount)
      captureCount += 1
    }

    if let parameters = ast[bundle].parameters {
      for (i, p) in parameters.enumerated() {
        locals[p] = .parameter(entry, i + captureCount)
      }
    }

    // Emit the body.
    self.insertionPoint = .end(of: entry)
    switch b {
    case .block(let s):
      lower(body: s, of: d, in: Frame(locals: locals))

    case .expr(let e):
      pushing(Frame(locals: locals)) { (this) in
        let x0 = this.emitLValue(e)
        let x1 = this.insert(
          this.module.makeBorrow(this.ast[d].introducer.value, from: x0, at: this.ast[e].site))!
        this.insert(this.module.makeYield(this.ast[d].introducer.value, x1, at: this.ast[e].site))
      }
      insert(module.makeReturn(at: ast[e].site))
    }
  }

  /// Inserts the IR for `b`, which is the body of `d` and is enclosed in `bodyFrame`.
  private mutating func lower(body b: BraceStmt.ID, of d: SubscriptImpl.ID, in f: Frame) {
    switch pushing(f, { $0.emit(braceStmt: b) }) {
    case .next:
      insert(module.makeReturn(at: .empty(atEndOf: ast[b].site)))
    case .return(let s):
      insert(module.makeReturn(at: ast[s].site))
    default:
      fatalError("not implemented")
    }
  }

  /// Inserts the IR for `d`.
  private mutating func lower(namespace d: NamespaceDecl.ID) {
    for m in ast[d].members {
      lower(topLevel: m)
    }
  }

  /// Inserts the IR for `d`.
  private mutating func lower(product d: ProductTypeDecl.ID) {
    _ = module.addGlobal(MetatypeType(program[d].type)!)
    lower(members: ast[d].members)
  }

  /// Inserts the IR for `d`.
  private mutating func lower(trait d: TraitDecl.ID) {
    _ = module.addGlobal(TraitType(program[d].type)!)
  }

  /// Inserts the IR for given declaration `members`.
  private mutating func lower(members: [AnyDeclID]) {
    for m in members {
      switch m.kind {
      case FunctionDecl.self:
        lower(function: .init(m)!)
      case InitializerDecl.self:
        lower(initializer: .init(m)!)
      case SubscriptDecl.self:
        lower(subscript: .init(m)!)
      default:
        continue
      }
    }
  }

  /// Inserts the IR for `d`.
  ///
  /// - Requires: `d` is a global binding.
  private mutating func lower(globalBinding d: BindingDecl.ID) {
    fatalError("not implemented")
  }

  /// Inserts the IR for the local binding `d`.
  ///
  /// - Requires: `d` is a local binding.
  private mutating func lower(localBinding d: BindingDecl.ID) {
    switch program[d].pattern.introducer.value {
    case .var, .sinklet:
      lower(storedLocalBinding: d)
    case .let, .inout:
      lower(projectedLocalBinding: d)
    }
  }

  /// Inserts the IR for stored local binding `d`.
  ///
  /// - Requires: `d` is a local `var` or `sink let` binding.
  private mutating func lower(storedLocalBinding d: BindingDecl.ID) {
    precondition(program.isLocal(d))
    precondition(read(program[d].pattern.introducer.value, { ($0 == .var) || ($0 == .sinklet) }))

    // Allocate storage for all the names declared by `d` in a single aggregate.
    let storage = emitAllocStack(for: program[d].type, at: ast[d].site)

    // Declare all introduced names, initializing them if possible.
    let lhs = program[d].pattern.subpattern.id
    if let initializer = ast[d].initializer {
      emitInitStoredLocalBindings(
        in: lhs, referringTo: [], relativeTo: storage, consuming: initializer)
      return
    }

    for (path, name) in ast.names(in: lhs) {
      _ = emitDeclaration(of: name, referringTo: path, relativeTo: storage)
    }
  }

  /// Inserts the IR to declare and initialize the names in `lhs`, which refer to subobjects of
  /// `subfield` relative to `storage`, by consuming the value of `initializer`.
  private mutating func emitInitStoredLocalBindings(
    in lhs: AnyPatternID, referringTo subfield: RecordPath, relativeTo storage: Operand,
    consuming initializer: AnyExprID
  ) {
    ast.walking(pattern: lhs, expression: initializer) { (path, p, rhs) in
      switch p.kind {
      case NamePattern.self:
        emitInitStoredLocalBinding(
          NamePattern.ID(p)!, referringTo: subfield + path, relativeTo: storage,
          consuming: rhs)

      case TuplePattern.self:
        emitInitStoredLocalBindings(
          in: TuplePattern.ID(p)!, referringTo: subfield + path, relativeTo: storage,
          consuming: rhs)

      case WildcardPattern.self:
        let s = emitStore(value: rhs)
        emitDeinit(s, at: ast[p].site)

      default:
        unexpected(p, in: ast)
      }
    }
  }

  /// Inserts the IR to declare and initialize `name`, which refers to the given `subfield`
  /// relative to `storage`, by consuming the value of `initializer`.
  private mutating func emitInitStoredLocalBinding(
    _ name: NamePattern.ID, referringTo subfield: RecordPath, relativeTo storage: Operand,
    consuming initializer: AnyExprID
  ) {
    let lhsPart = emitDeclaration(of: name, referringTo: subfield, relativeTo: storage)
    let lhsPartType = module.type(of: lhsPart).ast
    let rhsPartType = program.relations.canonical(program[initializer].type)

    if program.relations.areEquivalent(lhsPartType, rhsPartType) {
      emitStore(value: initializer, to: lhsPart)
    } else if lhsPartType.base is UnionType {
      let x0 = insert(
        module.makeOpenUnion(
          lhsPart, as: rhsPartType, forInitialization: true, at: ast[name].site))!
      emitStore(value: initializer, to: x0)
      insert(module.makeCloseUnion(x0, at: ast[name].site))
    } else {
      fatalError("not implemented")
    }
  }

  /// Inserts the IR to declare and initialize the names in `lhs`, which refer to subobjects of
  /// `subfield` relative to `storage`, by consuming the value of `initializer`.
  private mutating func emitInitStoredLocalBindings(
    in lhs: TuplePattern.ID, referringTo subfield: RecordPath, relativeTo storage: Operand,
    consuming initializer: AnyExprID
  ) {
    let rhs = emitSubfieldView(storage, at: subfield, at: ast[lhs].site)
    emitStore(value: initializer, to: rhs)
    for (path, name) in ast.names(in: lhs) {
      _ = emitDeclaration(of: name, referringTo: subfield + path, relativeTo: storage)
    }
  }

  /// Inserts the IR to declare `name`, which refers to the given `subfield` relative to `storage`,
  /// returning that sub-location.
  private mutating func emitDeclaration(
    of name: NamePattern.ID, referringTo subfield: RecordPath, relativeTo storage: Operand
  ) -> Operand {
    let s = emitSubfieldView(storage, at: subfield, at: ast[name].site)
    frames[ast[name].decl] = s
    return s
  }

  /// Inserts the IR for projected local binding `d` .
  ///
  /// - Requires: `d` is a local `let` or `inout` binding.
  private mutating func lower(projectedLocalBinding d: BindingDecl.ID) {
    precondition(program.isLocal(d))

    let access: AccessEffect
    switch program[d].pattern.introducer.value {
    case .let:
      access = .let
    case .inout:
      access = .inout
    default:
      preconditionFailure()
    }

    // Borrowed binding requires an initializer.
    guard let initializer = ast[d].initializer else {
      report(.error(binding: access, requiresInitializerAt: program[d].pattern.introducer.site))
      for (_, name) in ast.names(in: program[d].pattern.subpattern) {
        let t = program.relations.canonical(program[program[name].decl].type)
        frames[ast[name].decl] = .constant(Poison(type: .address(t)))
      }
      return
    }

    let source = emitLValue(initializer)
    let isSink = module.isSink(source, in: insertionFunction!)

    for (path, name) in ast.names(in: program[d].pattern.subpattern) {
      var part = emitSubfieldView(source, at: path, at: program[name].decl.site)
      let partType = module.type(of: part).ast
      let partDecl = ast[name].decl

      let t = program.relations.canonical(program[partDecl].type)
      if !program.relations.areEquivalent(t, partType) {
        if let u = ExistentialType(t) {
          let box = emitExistential(u, borrowing: access, from: part, at: ast[partDecl].site)
          part = box
        }
      }

      if isSink {
        let b = module.makeAccess(
          [.sink, access], from: part, correspondingTo: partDecl, at: ast[partDecl].site)
        frames[partDecl] = insert(b)!
      } else {
        let b = module.makeBorrow(
          access, from: part, correspondingTo: partDecl, at: ast[partDecl].site)
        frames[partDecl] = insert(b)!
      }
    }
  }

  /// Returns the lowered conformances of `model` that are exposed to `useScope`.
  private mutating func loweredConformances(
    of model: AnyType, exposedTo useScope: AnyScopeID
  ) -> Set<IR.Conformance> {
    guard let conformances = program.relations.conformances[model] else { return [] }

    var result: Set<IR.Conformance> = []
    for concept in conformances.keys {
      let c = program.conformance(of: model, to: concept, exposedTo: useScope)!
      result.insert(loweredConformance(c))
    }
    return result
  }

  /// Returns the lowered form of `c`, generating function references in `useScope`.
  private mutating func loweredConformance(_ c: Core.Conformance) -> IR.Conformance {
    var implementations = IR.Conformance.ImplementationMap()
    for (r, i) in c.implementations.storage {
      switch i {
      case .concrete(let d):
        implementations[r] = loweredRequirementImplementation(d)

      case .synthetic(let d):
        lower(synthesized: d)
        implementations[r] = .function(.init(to: .init(d), in: module))
      }
    }

    return .init(concept: c.concept, implementations: implementations)
  }

  /// Returns the lowered form of the requirement implementation `d` in `useScope`.
  private mutating func loweredRequirementImplementation(
    _ d: AnyDeclID
  ) -> IR.Conformance.Implementation {
    switch d.kind {
    case FunctionDecl.self:
      let r = FunctionReference(to: FunctionDecl.ID(d)!, in: &module)
      return .function(r)

    case InitializerDecl.self:
      let r = FunctionReference(to: InitializerDecl.ID(d)!, in: &module)
      return .function(r)

    default:
      fatalError("not implemented")
    }
  }

  /// Synthesizes the implementation of `d`.
  private mutating func lower(synthesized d: SynthesizedFunctionDecl) {
    switch d.kind {
    case .deinitialize:
      return withClearContext({ $0.lower(syntheticDeinit: d) })
    case .moveInitialization:
      return withClearContext({ $0.lower(syntheticMoveInit: d) })
    case .moveAssignment:
      return withClearContext({ $0.lower(syntheticMoveAssign: d) })
    case .copy:
      fatalError("not implemented")
    }
  }

  /// Inserts the IR for `d`, which is a synthetic deinitializer.
  private mutating func lower(syntheticDeinit d: SynthesizedFunctionDecl) {
    let f = module.demandSyntheticDeclaration(lowering: d)
    if (module[f].entry != nil) || (program.module(containing: d.scope) != module.id) {
      return
    }

    let site = ast[module.id].site
    let entry = module.appendEntry(in: d.scope, to: f)
    insertionPoint = .end(of: entry)
    self.frames.push()
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    // The receiver is a sink parameter representing the object to deinitialize.
    let receiver = Operand.parameter(entry, 0)
    emitDeinitParts(of: receiver, at: site)

    emitStore(value: .void, to: returnValue!, at: site)
    emitDeallocTopFrame(at: site)
    insert(module.makeReturn(at: site))
  }

  /// Inserts the IR for `d`, which is a synthetic move initialization method.
  private mutating func lower(syntheticMoveInit d: SynthesizedFunctionDecl) {
    let f = module.demandSyntheticDeclaration(lowering: d)
    if (module[f].entry != nil) || (program.module(containing: d.scope) != module.id) {
      return
    }

    let site = ast[module.id].site
    let entry = module.appendEntry(in: d.scope, to: f)
    insertionPoint = .end(of: entry)
    self.frames.push()
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    let receiver = Operand.parameter(entry, 0)
    let argument = Operand.parameter(entry, 1)
    let object = module.type(of: receiver).ast

    if object.hasRecordLayout {
      emitMoveInitRecordParts(of: receiver, consuming: argument, at: site)
    } else if object.base is UnionType {
      emitMoveInitUnionPayload(of: receiver, consuming: argument, at: site)
    }

    emitStore(value: .void, to: returnValue!, at: site)
    emitDeallocTopFrame(at: site)
    insert(module.makeReturn(at: site))
  }

  /// Inserts the IR for initializing the stored parts of `receiver`, which stores a record,
  /// consuming `argument` at `site`.
  private mutating func emitMoveInitRecordParts(
    of receiver: Operand, consuming argument: Operand, at site: SourceRange
  ) {
    let layout = AbstractTypeLayout(of: module.type(of: receiver).ast, definedIn: program)

    // If the object is empty, simply mark it initialized.
    if layout.properties.isEmpty {
      insert(module.makeMarkState(receiver, initialized: true, at: site))
      emitDeinit(argument, at: site)
      return
    }

    // Otherwise, move initialize each property.
    for i in layout.properties.indices {
      let source = emitSubfieldView(argument, at: [i], at: site)
      let target = emitSubfieldView(receiver, at: [i], at: site)
      let part = insert(module.makeLoad(source, at: site))!
      emitStore(value: part, to: target, at: site)
    }
  }

  /// Inserts the IR for initializing the payload of `receiver`, which stores a union container,
  /// consuming `argument` at `site`.
  private mutating func emitMoveInitUnionPayload(
    of receiver: Operand, consuming argument: Operand, at site: SourceRange
  ) {
    let t = UnionType(module.type(of: receiver).ast)!

    // If union is empty, simply mark it initialized.
    if t.elements.isEmpty {
      insert(module.makeMarkState(receiver, initialized: true, at: site))
      emitDeinit(argument, at: site)
      return
    }

    // Trivial if the union has a single member.
    if let e = t.elements.uniqueElement {
      emitMoveInitUnionPayload(of: receiver, consuming: argument, containing: e, at: site)
      return
    }

    // Otherwise, use a switch to select the correct move-initialization.
    let elements = program.discriminatorToElement(in: t)
    var successors: [Block.ID] = []
    for _ in t.elements {
      successors.append(appendBlock())
    }

    let n = insert(module.makeUnionDiscriminator(argument, at: site))!
    insert(module.makeSwitch(on: n, toOneOf: successors, at: site))

    let tail = appendBlock()
    for i in 0 ..< elements.count {
      insertionPoint = .end(of: successors[i])
      emitMoveInitUnionPayload(
        of: receiver, consuming: argument, containing: elements[i], at: site)
      insert(module.makeBranch(to: tail, at: site))
    }

    insertionPoint = .end(of: tail)
  }

  /// Inserts the IR for initializing the payload of `receiver`, which stores a union containing
  /// a `payload`, consuming `argument` at `site`.
  ///
  /// - Requires: the type of `storage` is a union containing a `payload`.
  private mutating func emitMoveInitUnionPayload(
    of receiver: Operand, consuming argument: Operand, containing payload: AnyType,
    at site: SourceRange
  ) {
    // Deinitialize the receiver.
    let x0 = insert(module.makeOpenUnion(receiver, as: payload, at: site))!
    emitDeinit(x0, at: site)

    // Move the argument.
    let x1 = insert(module.makeOpenUnion(argument, as: payload, at: site))!
    let c = program.conformanceToMovable(of: payload, exposedTo: insertionScope!)!
    emitMove(.set, of: x1, to: x0, withConformanceToMovable: c, at: site)

    // Close the unions.
    insert(module.makeCloseUnion(x0, at: site))
    insert(module.makeCloseUnion(x1, at: site))
  }

  /// Inserts the IR for `d`, which is a synthetic move initialization method.
  private mutating func lower(syntheticMoveAssign d: SynthesizedFunctionDecl) {
    let f = module.demandSyntheticDeclaration(lowering: d)
    if (module[f].entry != nil) || (program.module(containing: d.scope) != module.id) {
      return
    }

    let site = ast[module.id].site
    let entry = module.appendEntry(in: d.scope, to: f)
    insertionPoint = .end(of: entry)
    self.frames.push()
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    let receiver = Operand.parameter(entry, 0)
    let argument = Operand.parameter(entry, 1)

    // Deinitialize the receiver.
    emitDeinit(receiver, at: site)

    // Apply the move-initializer.
    let c = program.conformanceToMovable(of: module.type(of: receiver).ast, exposedTo: d.scope)!
    let r = insert(module.makeLoad(argument, at: site))!
    emitMove(.set, of: r, to: receiver, withConformanceToMovable: c, at: site)

    emitStore(value: .void, to: returnValue!, at: site)
    emitDeallocTopFrame(at: site)
    insert(module.makeReturn(at: site))
  }

  // MARK: Statements

  /// The description of the next action a program should execute.
  private enum ControlFlow: Equatable {

    /// Move to the next statement.
    case next

    /// Return from the current function.
    case `return`(ReturnStmt.ID)

    /// Break from the innermost loop.
    case `break`

    /// Continue the innermost loop.
    case `continue`

  }

  /// Inserts IR for returning from current function, anchoring instructions to `s`.
  private mutating func emitControlFlow(return s: ReturnStmt.ID) {
    for f in frames.elements.reversed() {
      emitDeallocs(for: f, at: ast[s].site)
    }
    insert(module.makeReturn(at: ast[s].site))
  }

  /// Inserts the IR for `s`, returning its effect on control flow.
  private mutating func emit<T: StmtID>(stmt s: T) -> ControlFlow {
    switch s.kind {
    case AssignStmt.self:
      return emit(assignStmt: .init(s)!)
    case BraceStmt.self:
      return emit(braceStmt: .init(s)!)
    case ConditionalStmt.self:
      return emit(conditionalStmt: .init(s)!)
    case DeclStmt.self:
      return emit(declStmt: .init(s)!)
    case DiscardStmt.self:
      return emit(discardStmt: .init(s)!)
    case DoWhileStmt.self:
      return emit(doWhileStmt: .init(s)!)
    case ExprStmt.self:
      return emit(exprStmt: .init(s)!)
    case ReturnStmt.self:
      return emit(returnStmt: .init(s)!)
    case WhileStmt.self:
      return emit(whileStmt: .init(s)!)
    case YieldStmt.self:
      return emit(yieldStmt: .init(s)!)
    default:
      unexpected(s, in: ast)
    }
  }

  private mutating func emit(assignStmt s: AssignStmt.ID) -> ControlFlow {
    // The left operand of an assignment should always be marked for mutation, even if the
    // statement actually denotes initialization.
    guard ast[s].left.kind == InoutExpr.self else {
      report(
        .error(assignmentLHSRequiresMutationMarkerAt: .empty(at: program[s].left.site.first())))
      return .next
    }

    // The RHS is evaluated before the LHS.
    let x0 = emitStore(value: ast[s].right)
    let x1 = insert(module.makeLoad(x0, at: ast[s].site))!
    let x2 = emitLValue(ast[s].left)
    emitStore(value: x1, to: x2, at: ast[s].site)
    return .next
  }

  private mutating func emit(braceStmt s: BraceStmt.ID) -> ControlFlow {
    frames.push()
    defer {
      emitDeallocTopFrame(at: .empty(atEndOf: ast[s].site))
      frames.pop()
    }

    for i in ast[s].stmts.indices {
      let a = emit(stmt: ast[s].stmts[i])
      if a == .next { continue }

      // Exit the scope early if `i` was a control-flow statement, complaining if it wasn't the
      // last statement of the code block.
      if i != ast[s].stmts.count - 1 {
        report(.warning(unreachableStatement: ast[s].stmts[i + 1], in: ast))
      }
      return a
    }

    return .next
  }

  private mutating func emit(conditionalStmt s: ConditionalStmt.ID) -> ControlFlow {
    let (firstBranch, secondBranch) = emitTest(condition: ast[s].condition, in: AnyScopeID(s))
    let tail = appendBlock()

    insertionPoint = .end(of: firstBranch)
    switch emit(braceStmt: ast[s].success) {
    case .next:
      insert(module.makeBranch(to: tail, at: ast[s].site))
    case .return(let s):
      emitControlFlow(return: s)
    default:
      fatalError("not implemented")
    }

    insertionPoint = .end(of: secondBranch)
    guard let failure = ast[s].failure else {
      insert(module.makeBranch(to: tail, at: ast[s].site))
      insertionPoint = .end(of: tail)
      return .next
    }

    switch emit(stmt: failure) {
    case .next:
      insert(module.makeBranch(to: tail, at: ast[s].site))
    case .return(let s):
      emitControlFlow(return: s)
    default:
      fatalError("not implemented")
    }

    insertionPoint = .end(of: tail)
    return .next
  }

  private mutating func emit(declStmt s: DeclStmt.ID) -> ControlFlow {
    switch ast[s].decl.kind {
    case BindingDecl.self:
      lower(localBinding: .init(ast[s].decl)!)
    default:
      unexpected(ast[s].decl, in: ast)
    }
    return .next
  }

  private mutating func emit(discardStmt s: DiscardStmt.ID) -> ControlFlow {
    let v = emitStore(value: ast[s].expr)
    emitDeinit(v, at: ast[s].site)
    return .next
  }

  private mutating func emit(doWhileStmt s: DoWhileStmt.ID) -> ControlFlow {
    let loopBody = module.appendBlock(in: ast[s].body, to: insertionFunction!)
    let loopTail = module.appendBlock(in: ast[s].body, to: insertionFunction!)
    insert(module.makeBranch(to: loopBody, at: .empty(at: ast[s].site.first())))
    insertionPoint = .end(of: loopBody)

    // We're not using `emit(braceStmt:into:)` because we need to evaluate the loop condition
    // before exiting the scope.
    frames.push()

    let body = program[s].body.stmts
    for i in body.indices {
      let a = emit(stmt: body[i])
      if a == .next { continue }

      // Exit the scope early if `i` was a control-flow statement, complaining if it wasn't the
      // last statement of the code block.
      if i != body.count - 1 {
        report(.warning(unreachableStatement: body[i + 1], in: ast))
      }

      if case .return = a {
        return a
      } else {
        fatalError("not implemented")
      }
    }

    let c = emit(branchCondition: ast[s].condition)
    emitDeallocTopFrame(at: ast[s].site)
    frames.pop()

    insert(
      module.makeCondBranch(if: c, then: loopBody, else: loopTail, at: program[s].condition.site))
    insertionPoint = .end(of: loopTail)
    return .next
  }

  private mutating func emit(exprStmt s: ExprStmt.ID) -> ControlFlow {
    let v = emitStore(value: ast[s].expr)
    emitDeinit(v, at: ast[s].site)
    if !module.type(of: v).ast.isVoidOrNever {
      // TODO: complain about unused value
      fatalError("not implemented")
    }
    return .next
  }

  private mutating func emit(returnStmt s: ReturnStmt.ID) -> ControlFlow {
    if let e = ast[s].value {
      emitStore(value: e, to: returnValue!)
    } else {
      emitStore(value: .void, to: returnValue!, at: ast[s].site)
    }

    // The return instruction is emitted by the caller handling this control-flow effect.
    return .return(s)
  }

  private mutating func emit(whileStmt s: WhileStmt.ID) -> ControlFlow {
    // Enter the loop.
    let head = module.appendBlock(in: s, to: insertionFunction!)
    insert(module.makeBranch(to: head, at: .empty(at: ast[s].site.first())))

    // Test the conditions.
    insertionPoint = .end(of: head)
    let (body, exit) = emitTest(condition: ast[s].condition, in: AnyScopeID(s))

    // Execute the body.
    insertionPoint = .end(of: body)
    switch emit(stmt: ast[s].body) {
    case .next:
      insert(module.makeBranch(to: head, at: .empty(atEndOf: program[s].body.site)))
    case .return(let s):
      emitControlFlow(return: s)
    default:
      fatalError("not implemented")
    }

    // Exit.
    insertionPoint = .end(of: exit)
    return .next
  }

  private mutating func emit(yieldStmt s: YieldStmt.ID) -> ControlFlow {
    // TODO: Read mutability of current subscript

    let x0 = emitLValue(ast[s].value)
    let x1 = insert(module.makeBorrow(.let, from: x0, at: ast[s].site))!
    insert(module.makeYield(.let, x1, at: ast[s].site))
    return .next
  }

  // MARK: Values

  /// Inserts the IR for storing `value` to `storage`, anchoring new instructions at `site`.
  private mutating func emitStore(value: Operand, to storage: Operand, at site: SourceRange) {
    let t = module.type(of: storage).ast

    // Because `emitStore(value:to:at:)` is used to synthesize conformances to `Movable`, calling
    // the move-initializer of `Val.Void` would cause infinite recursion. Since the latter should
    // always be inlined anyway, we can emit a simple store in all cases.
    if t.isBuiltin || (t == .void) {
      let x0 = insert(module.makeBorrow(.set, from: storage, at: site))!
      insert(module.makeStore(value, at: x0, at: site))
    } else {
      let c = program.conformanceToMovable(of: t, exposedTo: insertionScope!)!
      emitMove(.set, of: value, to: storage, withConformanceToMovable: c, at: site)
    }
  }

  /// Inserts the IR for storing the value of `syntax` to a fresh stack allocation, returning the
  /// address of this allocation.
  @discardableResult
  private mutating func emitStore<T: ExprID>(value syntax: T) -> Operand {
    let s = emitAllocStack(for: program[syntax].type, at: ast[syntax].site)
    emitStore(value: syntax, to: s)
    return s
  }

  /// Inserts the IR for storing the value of `syntax` to `storage`.
  ///
  /// `storage` must be the address of some uninitialized memory block capable of storing the value
  /// of `syntax`.
  private mutating func emitStore<T: ExprID>(value syntax: T, to storage: Operand) {
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
      unexpected(syntax, in: ast)
    }
  }

  private mutating func emitStore(
    booleanLiteral e: BooleanLiteralExpr.ID, to storage: Operand
  ) {
    let x0 = emitSubfieldView(storage, at: [0], at: ast[e].site)
    let x1 = insert(module.makeBorrow(.set, from: x0, at: ast[e].site))!
    insert(module.makeStore(.i1(ast[e].value), at: x1, at: ast[e].site))
  }

  private mutating func emitStore(cast e: CastExpr.ID, to storage: Operand) {
    switch ast[e].direction {
    case .up:
      emitStore(upcast: e, to: storage)
    case .down:
      emitStore(downcast: e, to: storage)
    case .pointerConversion:
      unreachable("pointer to address conversion evalutes to a lvalue")
    }
  }

  private mutating func emitStore(upcast e: CastExpr.ID, to storage: Operand) {
    assert(ast[e].direction == .up)
    let target = MetatypeType(program[ast[e].right].type)!.instance

    // Store the LHS to `storage` if it already has the desired type.
    if program.relations.areEquivalent(program[ast[e].left].type, target) {
      emitStore(value: ast[e].left, to: storage)
      return
    }

    // Otherwise, wrap the LHS.
    fatalError("not implemented")
  }

  private mutating func emitStore(downcast e: CastExpr.ID, to storage: Operand) {
    assert(ast[e].direction == .down)
    let target = MetatypeType(program[ast[e].right].type)!.instance

    // Store the LHS to `storage` if it already has the desired type.
    if program.relations.areEquivalent(program[ast[e].left].type, target) {
      emitStore(value: ast[e].left, to: storage)
      return
    }

    // Otherwise, unpack or repack the LHS.
    let lhs = emitStore(value: ast[e].left)

    // TODO
    _ = lhs
    fatalError("not implementeds")
  }

  private mutating func emitStore(conditional e: ConditionalExpr.ID, to storage: Operand) {
    let (success, failure) = emitTest(condition: ast[e].condition, in: AnyScopeID(e))
    let tail = appendBlock()

    // Emit the success branch.
    insertionPoint = .end(of: success)
    pushing(Frame(), { $0.emitStore(value: $0.ast[e].success, to: storage) })
    insert(module.makeBranch(to: tail, at: ast[e].site))

    // Emit the failure branch.
    insertionPoint = .end(of: failure)
    pushing(Frame(), { $0.emitStore(value: $0.ast[e].failure, to: storage) })
    insert(module.makeBranch(to: tail, at: ast[e].site))

    insertionPoint = .end(of: tail)
  }

  private mutating func emitStore(floatLiteral e: FloatLiteralExpr.ID, to storage: Operand) {
    emitStore(numericLiteral: e, to: storage)
  }

  private mutating func emitStore(functionCall e: FunctionCallExpr.ID, to storage: Operand) {
    // Handle built-ins and constructor calls.
    if let n = NameExpr.ID(ast[e].callee) {
      switch program[n].referredDecl {
      case .builtinFunction(let f):
        let x0 = emit(apply: f, to: ast[e].arguments, at: ast[e].site)
        let x1 = insert(module.makeBorrow(.set, from: storage, at: ast[e].site))!
        insert(module.makeStore(x0, at: x1, at: ast[e].site))
        return

      case .constructor:
        emit(initializerCall: e, initializing: storage)
        return

      default:
        break
      }
    }

    // Explicit arguments are evaluated first, from left to right.
    let explicitArguments = emit(
      arguments: ast[e].arguments, to: ast[e].callee,
      synthesizingDefaultArgumentsAt: .empty(atEndOf: ast[e].site))

    // Callee and captures are evaluated next.
    let (callee, captures) = emit(functionCallee: ast[e].callee)
    let arguments = captures + explicitArguments

    // Call is evaluated last.
    let o = insert(module.makeBorrow(.set, from: storage, at: ast[e].site))!
    insert(module.makeCall(applying: callee, to: arguments, writingResultTo: o, at: ast[e].site))
  }

  private mutating func emitStore(
    integerLiteral e: IntegerLiteralExpr.ID, to storage: Operand
  ) {
    emitStore(numericLiteral: e, to: storage)
  }

  private mutating func emitStore(lambda e: LambdaExpr.ID, to storage: Operand) {
    let f = lower(function: ast[e].decl)
    let r = FunctionReference(
      to: f, parameterizedBy: module.parameterization(in: insertionFunction!), in: module)

    let x0 = insert(module.makePartialApply(wrapping: r, with: .void, at: ast[e].site))!
    let x1 = insert(module.makeBorrow(.set, from: storage, at: ast[e].site))!
    insert(module.makeStore(x0, at: x1, at: ast[e].site))
  }

  private mutating func emitStore(name e: NameExpr.ID, to storage: Operand) {
    let x0 = emitLValue(name: e)
    let x1 = insert(module.makeLoad(x0, at: ast[e].site))!
    emitStore(value: x1, to: storage, at: ast[e].site)
  }

  /// Writes the value of `e` to `storage`.
  ///
  /// - Parameters:
  ///   - site: The source range in which `e` is being evaluated. Defaults to `e.site`.
  private mutating func emitStore(
    pragma e: PragmaLiteralExpr.ID, to storage: Operand,
    at site: SourceRange? = nil
  ) {
    let anchor = site ?? ast[e].site
    switch ast[e].kind {
    case .file:
      emitStore(string: anchor.file.url.absoluteURL.path, to: storage, at: anchor)
    case .line:
      emitStore(int: anchor.first().line.number, to: storage, at: anchor)
    }
  }

  private mutating func emitStore(sequence e: SequenceExpr.ID, to storage: Operand) {
    emitStore(foldedSequence: program[e].folded, to: storage)
  }

  private mutating func emitStore(foldedSequence e: FoldedSequenceExpr, to storage: Operand) {
    switch e {
    case .infix(let callee, let lhs, let rhs):
      let t = program[callee.expr].type
      let calleeType = LambdaType(program.relations.canonical(t))!.lifted

      // Emit the operands, starting with RHS.
      let r = emit(infixOperand: rhs, passed: ParameterType(calleeType.inputs[1].type)!.access)
      let l = emit(infixOperand: lhs, passed: ParameterType(calleeType.inputs[0].type)!.access)

      // The callee must be a reference to member function.
      guard case .member(let d, _, _) = program[callee.expr].referredDecl else { unreachable() }
      let oper = Operand.constant(FunctionReference(to: FunctionDecl.ID(d)!, in: &module))

      // Emit the call.
      let site = ast.site(of: e)
      let x0 = insert(module.makeBorrow(.set, from: storage, at: site))!
      insert(module.makeCall(applying: oper, to: [l, r], writingResultTo: x0, at: site))

    case .leaf(let v):
      emitStore(value: v, to: storage)
    }
  }

  private mutating func emitStore(stringLiteral e: StringLiteralExpr.ID, to storage: Operand) {
    emitStore(string: ast[e].value, to: storage, at: ast[e].site)
  }

  private mutating func emitStore(tuple e: TupleExpr.ID, to storage: Operand) {
    if ast[e].elements.isEmpty {
      let t = program.relations.canonical(program[e].type)
      let x0 = insert(module.makeUnsafeCast(.void, to: t, at: ast[e].site))!
      let x1 = insert(module.makeBorrow(.set, from: storage, at: ast[e].site))!
      insert(module.makeStore(x0, at: x1, at: ast[e].site))
      return
    }

    for (i, element) in ast[e].elements.enumerated() {
      let xi = emitSubfieldView(storage, at: [i], at: ast[element.value].site)
      emitStore(value: element.value, to: xi)
    }
  }

  private mutating func emitStore(tupleMember e: TupleMemberExpr.ID, to storage: Operand) {
    let x0 = emitLValue(tupleMember: e)
    let x1 = insert(module.makeLoad(x0, at: ast[e].site))!
    emitStore(value: x1, to: storage, at: ast[e].site)
  }

  /// Writes the value of `literal` to `storage`.
  private mutating func emitStore<T: NumericLiteralExpr>(
    numericLiteral literal: T.ID, to storage: Operand
  ) {
    let literalType = program.relations.canonical(program[literal].type)

    switch literalType {
    case ast.coreType("Int")!:
      emitStore(integer: literal, signed: true, bitWidth: 64, to: storage)
    case ast.coreType("Int32")!:
      emitStore(integer: literal, signed: true, bitWidth: 32, to: storage)
    case ast.coreType("Int8")!:
      emitStore(integer: literal, signed: true, bitWidth: 8, to: storage)
    case ast.coreType("UInt")!:
      emitStore(integer: literal, signed: false, bitWidth: 64, to: storage)
    case ast.coreType("Float64")!:
      emitStore(floatingPoint: literal, to: storage, evaluatedBy: FloatingPointConstant.float64(_:))
    case ast.coreType("Float32")!:
      emitStore(floatingPoint: literal, to: storage, evaluatedBy: FloatingPointConstant.float32(_:))
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
    let syntax = ast[literal]
    let x0 = emitSubfieldView(storage, at: [0], at: syntax.site)
    let x1 = insert(module.makeBorrow(.set, from: x0, at: syntax.site))!
    let x2 = Operand.constant(evaluate(syntax.value))
    insert(module.makeStore(x2, at: x1, at: syntax.site))
  }

  /// Writes the value of `literal` to `storage`, knowing it is a core integer instance with given
  /// sign and width.
  private mutating func emitStore<T: NumericLiteralExpr>(
    integer literal: T.ID, signed: Bool, bitWidth: Int, to storage: Operand
  ) {
    let syntax = ast[literal]
    guard let bits = WideUInt(valLiteral: syntax.value, signed: signed, bitWidth: bitWidth) else {
      diagnostics.insert(
        .error(
          integerLiteral: syntax.value,
          overflowsWhenStoredInto: program[literal].type,
          at: syntax.site))
      return
    }

    let x0 = emitSubfieldView(storage, at: [0], at: syntax.site)
    let x1 = insert(module.makeBorrow(.set, from: x0, at: syntax.site))!
    let x2 = Operand.constant(IntegerConstant(bits))
    insert(module.makeStore(x2, at: x1, at: syntax.site))
  }

  /// Writes an instance of `Val.Int` with value `v` to `storage`, anchoring new instruction at
  /// `site`.
  ///
  /// - Requires: `storage` is the address of uninitialized memory of type `Val.Int`.
  private mutating func emitStore(int v: Int, to storage: Operand, at site: SourceRange) {
    let x0 = emitSubfieldView(storage, at: [0], at: site)
    let x1 = insert(module.makeBorrow(.set, from: x0, at: site))!
    insert(module.makeStore(.word(v), at: x1, at: site))
  }

  /// Writes an instance of `Val.String` with value `v` to `storage`, anchoring new instruction at
  /// `site`.
  ///
  /// - Requires: `storage` is the address of uninitialized memory of type `Val.String`.
  private mutating func emitStore(string v: String, to storage: Operand, at site: SourceRange) {
    var bytes = v.unescaped.data(using: .utf8)!
    let utf8 = PointerConstant(module.id, module.addGlobal(BufferConstant(bytes)))
    let size = bytes.count

    // Make sure the string is null-terminated.
    bytes.append(contentsOf: [0])

    let x0 = emitSubfieldView(storage, at: [0], at: site)
    emitStore(int: size, to: x0, at: site)

    let x1 = emitSubfieldView(storage, at: [1, 0], at: site)
    let x2 = insert(module.makeBorrow(.set, from: x1, at: site))!
    insert(module.makeStore(.constant(utf8), at: x2, at: site))
  }

  /// Inserts the IR for given constructor `call`, which initializes storage `r` by applying
  /// initializer `d` parameterized by `a`.
  ///
  /// - Parameters:
  ///   - call: The syntax of the call.
  ///   - s: The address of uninitialized storage typed by the receiver of `call`. This storage is
  ///     borrowed for initialization after evaluating `call`'s arguments and before the call.
  private mutating func emit(
    initializerCall call: FunctionCallExpr.ID, initializing s: Operand
  ) {
    let callee = NameExpr.ID(ast[call].callee)!
    guard case .constructor(let d, let a) = program[callee].referredDecl else {
      preconditionFailure()
    }

    // Handle memberwise constructor calls.
    if ast[d].isMemberwise {
      emit(memberwiseInitializerCall: call, initializing: s)
      return
    }

    // Arguments are evaluated first, from left to right.
    let arguments = emit(
      arguments: ast[call].arguments, to: ast[call].callee,
      synthesizingDefaultArgumentsAt: .empty(atEndOf: ast[call].site))

    // Receiver is captured next.
    let receiver = insert(module.makeBorrow(.set, from: s, at: ast[call].site))!

    // Call is evaluated last.
    let f = FunctionReference(to: d, parameterizedBy: a, in: &module)
    let x0 = emitAllocStack(for: .void, at: ast[call].site)
    let x1 = insert(module.makeBorrow(.set, from: x0, at: ast[call].site))!
    insert(
      module.makeCall(
        applying: .constant(f), to: [receiver] + arguments, writingResultTo: x1,
        at: ast[call].site))
  }

  /// Inserts the IR for given memberwise constructor `call`, which initializes `receiver`.
  ///
  /// - Parameters:
  ///   - call: The syntax of the call.
  ///   - receiver: The address of uninitialized storage typed by the receiver of `d`. This storage
  ///     is borrowed for initialization after evaluating `call`'s arguments.
  private mutating func emit(
    memberwiseInitializerCall call: FunctionCallExpr.ID, initializing receiver: Operand
  ) {
    let callee = LambdaType(program.relations.canonical(program[ast[call].callee].type))!

    if callee.inputs.isEmpty {
      let t = program.relations.canonical(program[call].type)
      let x0 = insert(module.makeUnsafeCast(.void, to: t, at: ast[call].site))!
      let x1 = insert(module.makeBorrow(.set, from: receiver, at: ast[call].site))!
      insert(module.makeStore(x0, at: x1, at: ast[call].site))
      return
    }

    for i in callee.inputs.indices {
      // TODO: Handle remote types
      let p = ParameterType(callee.inputs[i].type)!
      if p.bareType.base is RemoteType {
        fatalError("not implemented")
      }

      let s = emitSubfieldView(receiver, at: [i], at: ast[call].site)
      emitStore(value: ast[call].arguments[i].value, to: s)
    }
  }

  /// Inserts the IR for `arguments`, which is an argument passed to a function of type `callee`.
  ///
  /// - Parameters:
  ///   - syntheticSite: The site at which default pragma arguments are anchored.
  private mutating func emit(
    arguments: [LabeledArgument], to callee: AnyExprID,
    synthesizingDefaultArgumentsAt syntheticSite: SourceRange
  ) -> [Operand] {
    let calleeType = program.relations.canonical(program[callee].type).base as! CallableType
    let calleeDecl = NameExpr.ID(callee).flatMap({ program[$0].referredDecl.decl! })
    let defaults = calleeDecl.flatMap(ast.defaultArguments(of:))

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

      let v = emit(argument: a, to: ParameterType(p.type)!, at: syntheticSite)
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
    argument syntax: AnyExprID, to parameter: ParameterType, at site: SourceRange? = nil
  ) -> Operand {
    let argumentSite: SourceRange
    let storage: Operand

    if let e = PragmaLiteralExpr.ID(syntax) {
      argumentSite = site ?? ast[e].site
      storage = insert(module.makeAllocStack(program[e].type, at: argumentSite))!
      emitStore(pragma: e, to: storage, at: argumentSite)
    } else {
      argumentSite = ast[syntax].site
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
      let t = program[callee.expr].type
      let u = LambdaType(program.relations.canonical(t))!.lifted
      storage = emitAllocStack(for: u.output, at: ast.site(of: e))
      emitStore(foldedSequence: e, to: storage)

    case .leaf(let e):
      storage = emitLValue(e)
    }

    return emitAcquire(access, on: storage, at: ast.site(of: e))
  }

  /// Emits the IR of a call to `f` with given `arguments` at `site`.
  private mutating func emit(
    apply f: BuiltinFunction, to arguments: [LabeledArgument], at site: SourceRange
  ) -> Operand {
    switch f.name {
    case .llvm(let n):
      var a: [Operand] = []
      for e in arguments {
        let x0 = emitStore(value: e.value)
        let x1 = insert(module.makeLoad(x0, at: site))!
        a.append(x1)
      }
      return insert(module.makeLLVM(applying: n, to: a, at: site))!

    case .addressOf:
      let source = emitLValue(arguments[0].value)
      return insert(module.makeAddressToPointer(source, at: site))!
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
    functionCallee callee: AnyExprID
  ) -> (callee: Operand, captures: [Operand]) {
    switch callee.kind {
    case NameExpr.self:
      return emit(namedFunctionCallee: .init(callee)!)

    case InoutExpr.self:
      // TODO: Handle the mutation marker, somehow.
      return emit(functionCallee: ast[InoutExpr.ID(callee)!].subject)

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
    namedFunctionCallee callee: NameExpr.ID
  ) -> (callee: Operand, captures: [Operand]) {
    let calleeType = LambdaType(program.relations.canonical(program[callee].type))!

    switch program[callee].referredDecl {
    case .direct(let d, let a) where d.kind == FunctionDecl.self:
      // Callee is a direct reference to a function declaration.
      guard calleeType.environment == .void else {
        fatalError("not implemented")
      }

      let p = module.parameterization(in: insertionFunction!).appending(a)
      let r = FunctionReference(to: FunctionDecl.ID(d)!, parameterizedBy: p, in: &module)
      return (.constant(r), [])

    case .member(let d, let a, let s) where d.kind == FunctionDecl.self:
      // Callee is a member reference to a function or method.
      let r = FunctionReference(to: FunctionDecl.ID(d)!, parameterizedBy: a, in: &module)

      // The callee's receiver is the sole capture.
      let receiver = emitLValue(receiver: s, at: ast[callee].site)
      if let t = RemoteType(calleeType.captures[0].type) {
        let i = insert(module.makeBorrow(t.access, from: receiver, at: ast[callee].site))!
        return (Operand.constant(r), [i])
      } else {
        let i = insert(module.makeLoad(receiver, at: ast[callee].site))!
        return (Operand.constant(r), [i])
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
  private mutating func emit(lambdaCallee callee: AnyExprID) -> Operand {
    switch LambdaType(program[callee].type)!.receiverEffect {
    case .yielded:
      unreachable()
    case .set, .sink:
      fatalError("not implemented")

    case let k:
      let l = emitLValue(callee)
      let b = module.makeBorrow(k, from: l, at: ast[callee].site)
      return insert(b)!
    }
  }

  private mutating func emit(
    subscriptCallee callee: AnyExprID
  ) -> (callee: SubscriptBundleReference, captures: [Operand]) {
    // TODO: Handle captures
    switch callee.kind {
    case NameExpr.self:
      return emit(namedSubscriptCallee: .init(callee)!)

    case InoutExpr.self:
      // TODO: Handle the mutation marker, somehow.
      return emit(subscriptCallee: ast[InoutExpr.ID(callee)!].subject)

    default:
      fatalError("not implemented")
    }
  }

  private mutating func emit(
    namedSubscriptCallee callee: NameExpr.ID
  ) -> (callee: SubscriptBundleReference, captures: [Operand]) {
    switch program[callee].referredDecl {
    case .direct(let d, let a) where d.kind == SubscriptDecl.self:
      // Callee is a direct reference to a subscript declaration.
      let t = SubscriptType(program.relations.canonical(program[d].type))!
      guard t.environment == .void else {
        fatalError("not implemented")
      }

      let b = SubscriptBundleReference(to: SubscriptDecl.ID(d)!, parameterizedBy: a)
      return (b, [])

    case .member(let d, let a, let s) where d.kind == SubscriptDecl.self:
      // Callee is a member reference to a subscript declaration.
      let b = SubscriptBundleReference(to: SubscriptDecl.ID(d)!, parameterizedBy: a)

      // The callee's receiver is the sole capture.
      let receiver = emitLValue(receiver: s, at: ast[callee].site)
      let t = SubscriptType(program[d].type)!
      let i = insert(module.makeAccess(t.capabilities, from: receiver, at: ast[callee].site))!
      return (b, [i])

    case .builtinFunction, .builtinType:
      // There are no built-in subscripts.
      unreachable()

    default:
      fatalError("not implemented")
    }
  }

  /// Returns `(success: a, failure: b)` where `a` is the basic block reached if all items in
  /// `condition` hold and `b` is the basic block reached otherwise, creating new basic blocks
  /// in `scope`.
  private mutating func emitTest(
    condition: [ConditionItem], in scope: AnyScopeID
  ) -> (success: Block.ID, failure: Block.ID) {
    let f = insertionFunction!

    // Allocate storage for all the declarations in the condition before branching so that all
    // `dealloc_stack` are to dominated by their corresponding `alloc_stack`.
    var allocs: [Operand] = []
    for case .decl(let d) in condition {
      let a = insert(module.makeAllocStack(program[d].type, at: ast[d].site))!
      allocs.append(a)
    }

    let failure = module.appendBlock(in: scope, to: f)
    for (i, item) in condition.enumerated() {
      switch item {
      case .expr(let e):
        let test = pushing(Frame(), { $0.emit(branchCondition: e) })
        let next = module.appendBlock(in: scope, to: f)
        insert(module.makeCondBranch(if: test, then: next, else: failure, at: ast[e].site))
        insertionPoint = .end(of: next)

      case .decl(let d):
        let subject = emitLValue(ast[d].initializer!)

        if let t = UnionType(module.type(of: subject).ast) {
          let u = program.relations.canonical(program[d].type)
          let b = emitConditionalNarrowing(
            of: subject, typed: t, as: ast[d].pattern, typed: u,
            to: allocs[i],
            else: failure, in: scope)

          insertionPoint = .end(of: b)
          continue
        }

        fatalError("not implemented")
      }
    }

    return (success: insertionBlock!, failure: failure)
  }

  /// Returns a basic block in which the names in `pattern` have been declared and initialized by
  /// consuming the payload of `container` as an instance of `patternType`.
  private mutating func emitConditionalNarrowing(
    of container: Operand, typed containerType: UnionType,
    as pattern: BindingPattern.ID, typed patternType: AnyType,
    to storage: Operand,
    else failure: Block.ID, in scope: AnyScopeID
  ) -> Block.ID {
    // TODO: Implement narrowing to an arbitrary subtype.
    assert(containerType.elements.contains(patternType), "not implemented")
    let site = ast[pattern].site

    let i = program.discriminatorToElement(in: containerType).firstIndex(of: patternType)!
    let expected = IntegerConstant(i, bitWidth: 64)  // FIXME: should be width of 'word'
    let actual = insert(module.makeUnionDiscriminator(container, at: site))!

    let test = insert(
      module.makeLLVM(applying: .icmp(.eq, .word), to: [.constant(expected), actual], at: site))!
    let next = module.appendBlock(in: scope, to: insertionFunction!)
    insert(module.makeCondBranch(if: test, then: next, else: failure, at: site))

    insertionPoint = .end(of: next)
    let x0 = insert(module.makeOpenUnion(container, as: patternType, at: site))!
    let x1 = insert(module.makeLoad(x0, at: site))!
    insert(module.makeCloseUnion(x0, at: site))

    let c = program.conformanceToMovable(of: patternType, exposedTo: scope)!
    pushing(Frame()) { (this) in
      this.emitMove(.set, of: x1, to: storage, withConformanceToMovable: c, at: site)
    }

    for (path, name) in ast.names(in: pattern) {
      _ = emitDeclaration(of: name, referringTo: path, relativeTo: storage)
    }

    return next
  }

  /// Inserts the IR for branch condition `e`.
  ///
  /// - Requires: `e.type` is `Val.Bool`
  private mutating func emit(branchCondition e: AnyExprID) -> Operand {
    precondition(program.relations.canonical(program[e].type) == ast.coreType("Bool")!)
    let x0 = emitLValue(e)
    let x1 = emitSubfieldView(x0, at: [0], at: ast[e].site)
    let x2 = insert(module.makeLoad(x1, at: ast[e].site))!
    return x2
  }

  /// Inserts the IR for converting `foreign` to a value of type `ir`.
  private mutating func emitConvert(
    foreign: Operand,
    to ir: AnyType,
    at site: SourceRange
  ) -> Operand {
    precondition(module.type(of: foreign).isObject)

    let foreignConvertible = ast.coreTrait("ForeignConvertible")!
    let foreignConvertibleConformance = program.conformance(
      of: ir, to: foreignConvertible, exposedTo: insertionScope!)!
    let r = ast.requirements(
      Name(stem: "init", labels: ["foreign_value"]), in: foreignConvertible.decl)[0]

    switch foreignConvertibleConformance.implementations[r]! {
    case .concrete(let m):
      let convert = FunctionReference(to: InitializerDecl.ID(m)!, in: &module)
      let t = LambdaType(convert.type.ast)!.output

      let x0 = emitAllocStack(for: ir, at: site)
      let x1 = insert(module.makeBorrow(.set, from: x0, at: site))!
      let x2 = emitAllocStack(for: t, at: site)
      let x3 = insert(module.makeBorrow(.set, from: x2, at: site))!
      insert(
        module.makeCall(
          applying: .constant(convert), to: [x1, foreign], writingResultTo: x3, at: site))
      return insert(module.makeLoad(x0, at: site))!

    case .synthetic:
      fatalError("not implemented")
    }
  }

  /// Appends the IR to convert `o` to a FFI argument.
  private mutating func emitConvertToForeign(_ o: Operand, at site: SourceRange) -> Operand {
    let t = module.type(of: o)
    precondition(t.isAddress)

    let foreignConvertible = ast.coreTrait("ForeignConvertible")!
    let foreignConvertibleConformance = program.conformance(
      of: t.ast, to: foreignConvertible, exposedTo: insertionScope!)!
    let r = ast.requirements("foreign_value", in: foreignConvertible.decl)[0]

    // TODO: Handle cases where the foreign representation of `t` is not built-in

    switch foreignConvertibleConformance.implementations[r]! {
    case .concrete(let m):
      let convert = FunctionReference(to: FunctionDecl.ID(m)!, in: &module)
      let t = LambdaType(convert.type.ast)!.output

      let x0 = insert(module.makeBorrow(.let, from: o, at: site))!
      let x1 = emitAllocStack(for: t, at: site)
      let x2 = insert(module.makeBorrow(.set, from: x1, at: site))!
      insert(
        module.makeCall(applying: .constant(convert), to: [x0], writingResultTo: x2, at: site))
      return insert(module.makeLoad(x1, at: site))!

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
    let g = PointerConstant(module.id, module.addGlobal(witnessTable))

    let x0 = insert(module.makeBorrow(access, from: witness, at: site))!
    let x1 = insert(module.makeWrapExistentialAddr(x0, .constant(g), as: t, at: site))!
    return x1
  }

  /// Returns the witness table of `t` in `s`.
  private mutating func emitWitnessTable(of t: AnyType, usedIn s: AnyScopeID) -> WitnessTable {
    .init(for: t, conformingTo: loweredConformances(of: t, exposedTo: s), in: s)
  }

  // MARK: l-values

  /// Inserts the IR for the lvalue `syntax` block.
  private mutating func emitLValue(_ syntax: AnyExprID) -> Operand {
    switch syntax.kind {
    case CastExpr.self:
      return emitLValue(cast: .init(syntax)!)
    case InoutExpr.self:
      return emitLValue(inoutExpr: .init(syntax)!)
    case NameExpr.self:
      return emitLValue(name: .init(syntax)!)
    case SubscriptCallExpr.self:
      return emitLValue(subscriptCall: .init(syntax)!)
    case TupleMemberExpr.self:
      return emitLValue(tupleMember: .init(syntax)!)
    default:
      return emitStore(value: syntax)
    }
  }

  private mutating func emitLValue(cast e: CastExpr.ID) -> Operand {
    switch ast[e].direction {
    case .pointerConversion:
      let x0 = emitLValue(ast[e].left)
      let x1 = insert(module.makeLoad(x0, at: ast[e].site))!
      let target = RemoteType(program.relations.canonical(program[e].type))!
      return insert(module.makePointerToAddress(x1, to: target, at: ast[e].site))!

    default:
      fatalError("not implemented")
    }
  }

  private mutating func emitLValue(inoutExpr e: InoutExpr.ID) -> Operand {
    emitLValue(ast[e].subject)
  }

  private mutating func emitLValue(name e: NameExpr.ID) -> Operand {
    emitLValue(reference: program[e].referredDecl, at: ast[e].site)
  }

  private mutating func emitLValue(reference r: DeclReference, at site: SourceRange) -> Operand {
    switch r {
    case .direct(let d, _):
      return emitLValue(directReferenceTo: d, at: site)

    case .member(let d, let a, let s):
      let receiver = emitLValue(receiver: s, at: site)
      return emitProperty(boundTo: receiver, declaredBy: d, parameterizedBy: a, at: site)

    case .constructor:
      fatalError("not implemented")

    case .builtinModule, .builtinFunction, .builtinType, .intrinsicType:
      // Built-in symbols and intrinsic types are never used as l-value.
      unreachable()
    }
  }

  /// Returns the address of the `operation`'s receiver, which refers to a member declaration.
  private mutating func emitLValue(
    receiver r: DeclReference.Receiver, at site: SourceRange
  ) -> Operand {
    switch r {
    case .operand, .implicit:
      unreachable()
    case .explicit(let e):
      return emitLValue(e)
    case .elided(let s):
      return emitLValue(reference: s, at: site)
    }
  }

  /// Inserts the IR denoting a direct reference to `d`.
  private mutating func emitLValue(
    directReferenceTo d: AnyDeclID, at site: SourceRange
  ) -> Operand {
    // Handle local bindings.
    if let s = frames[d] {
      return s
    }

    // Handle global bindings.
    if d.kind == VarDecl.self {
      fatalError("not implemented")
    }

    // Handle references to type declarations.
    if let t = MetatypeType(program[d].type) {
      let s = emitAllocStack(for: ^t, at: site)
      insert(module.makeStore(.constant(t), at: s, at: site))
      return s
    }

    // Handle references to global functions.
    fatalError("not implemented")
  }

  private mutating func emitLValue(subscriptCall e: SubscriptCallExpr.ID) -> Operand {
    // Explicit arguments are evaluated first, from left to right.
    let explicitArguments = emit(
      arguments: ast[e].arguments, to: ast[e].callee,
      synthesizingDefaultArgumentsAt: .empty(atEndOf: ast[e].site))

    // Callee and captures are evaluated next.
    let (callee, captures) = emit(subscriptCallee: ast[e].callee)
    let arguments = captures + explicitArguments

    var variants: [AccessEffect: Function.ID] = [:]
    for v in ast[callee.bundle].impls {
      variants[ast[v].introducer.value] = module.demandSubscriptDeclaration(lowering: v)
    }

    // Projection is evaluated last.
    let t = SubscriptType(
      program.canonicalType(of: callee.bundle, parameterizedBy: callee.arguments))!
    return insert(
      module.makeProjectBundle(
        applying: variants, of: callee, typed: t, to: arguments, at: ast[e].site))!
  }

  private mutating func emitLValue(tupleMember e: TupleMemberExpr.ID) -> Operand {
    let base = emitLValue(ast[e].tuple)
    return emitSubfieldView(base, at: [ast[e].index.value], at: ast[e].index.site)
  }

  /// Returns the address of the member declared by `d`, parameterized by `a`, and bound to
  /// `receiver`, inserting IR anchored at `site`.
  private mutating func emitProperty(
    boundTo receiver: Operand, declaredBy d: AnyDeclID, parameterizedBy a: GenericArguments,
    at site: SourceRange
  ) -> Operand {
    switch d.kind {
    case SubscriptDecl.self:
      return emitComputedProperty(
        boundTo: receiver, declaredByBundle: .init(d)!, parameterizedBy: a, at: site)

    case VarDecl.self:
      let l = AbstractTypeLayout(of: module.type(of: receiver).ast, definedIn: program)
      let i = l.offset(of: ast[VarDecl.ID(d)!].baseName)!
      return emitSubfieldView(receiver, at: [i], at: site)

    default:
      fatalError("not implemented")
    }
  }

  /// Returns the projection the property declared by `d`, parameterized by `a`, and bound to
  /// `receiver`, inserting IR anchored at `site`.
  private mutating func emitComputedProperty(
    boundTo receiver: Operand, declaredByBundle d: SubscriptDecl.ID,
    parameterizedBy a: GenericArguments,
    at site: SourceRange
  ) -> Operand {
    if let i = ast[d].impls.uniqueElement {
      return emitComputedProperty(boundTo: receiver, declaredBy: i, parameterizedBy: a, at: site)
    }

    let t = SubscriptType(program.canonicalType(of: d, parameterizedBy: a))!
    let r = insert(module.makeAccess(t.capabilities, from: receiver, at: site))!

    var variants: [AccessEffect: Function.ID] = [:]
    for v in ast[d].impls {
      variants[ast[v].introducer.value] = module.demandSubscriptDeclaration(lowering: v)
    }

    return insert(
      module.makeProjectBundle(
        applying: variants, of: .init(to: d, parameterizedBy: a), typed: t, to: [r], at: site))!
  }

  /// Returns the projection of the property declared by `d`, parameterized by `a`, and bound to
  /// `receiver`, inserting IR anchored at `site`.
  private mutating func emitComputedProperty(
    boundTo receiver: Operand, declaredBy d: SubscriptImpl.ID, parameterizedBy a: GenericArguments,
    at site: SourceRange
  ) -> Operand {
    let t = SubscriptImplType(program.canonicalType(of: d, parameterizedBy: a))!
    let o = RemoteType(ast[d].introducer.value, program.relations.canonical(t.output))
    let r = insert(module.makeBorrow(o.access, from: receiver, at: site))!
    let f = module.demandSubscriptDeclaration(lowering: d)
    return insert(module.makeProject(o, applying: f, parameterizedBy: a, to: [r], at: site))!
  }

  // MARK: Deinitialization

  /// If `storage` is deinitializable in `self.insertionScope`, inserts the IR for deinitializing
  /// it, anchoring new instructions at `site`. Otherwise, reports a diagnostic.
  ///
  /// Let `T` be the type of `storage`, `storage` is deinitializable iff `T` has a deinitializer
  /// exposed to `self.insertionScope`.
  mutating func emitDeinit(_ storage: Operand, at site: SourceRange) {
    // Use custom conformance to `Deinitializable` if possible.
    let t = module.type(of: storage).ast
    if let c = module.program.conformanceToDeinitializable(of: t, exposedTo: insertionScope!) {
      emitDeinit(storage, withConformanceToDeinitializable: c, at: site)
      return
    }

    switch t.base {
    case is BuiltinType, is MetatypeType:
      insert(module.makeMarkState(storage, initialized: false, at: site))

    case let u as LambdaType:
      if program.relations.canonical(u.environment) == .void {
        insert(module.makeMarkState(storage, initialized: false, at: site))
      } else {
        fatalError("not implemented")
      }

    default:
      report(.error(nonDeinitializable: module.type(of: storage).ast, at: site))
    }
  }

  /// Inserts the IR for deinitializing `storage`, using `c` to identify the deinitializer to apply
  /// and anchoring new instructions at `site`.
  private mutating func emitDeinit(
    _ storage: Operand, withConformanceToDeinitializable c: Core.Conformance,
    at site: SourceRange
  ) {
    let d = module.demandDeinitDeclaration(from: c)
    let f = Operand.constant(FunctionReference(to: d, in: module))

    let x0 = insert(module.makeLoad(storage, at: site))!
    let x1 = insert(module.makeAllocStack(.void, at: site))!
    let x2 = insert(module.makeBorrow(.set, from: x1, at: site))!
    insert(module.makeCall(applying: f, to: [x0], writingResultTo: x2, at: site))
    insert(module.makeEndBorrow(x2, at: site))
    insert(module.makeMarkState(x1, initialized: false, at: site))
    insert(module.makeDeallocStack(for: x1, at: site))
  }

  /// If `storage` is deinitializable in `self.insertionScope`, inserts the IR for deinitializing
  /// it, anchoring new instructions at `site`. Otherwise, reports a diagnostic for each part that
  /// isn't deinitializable.
  private mutating func emitDeinitParts(of storage: Operand, at site: SourceRange) {
    let t = module.type(of: storage).ast

    if t.hasRecordLayout {
      emitDeinitRecordParts(of: storage, at: site)
    } else if t.base is UnionType {
      emitDeinitUnionPayload(of: storage, at: site)
    } else {
      report(.error(nonDeinitializable: module.type(of: storage).ast, at: site))
    }
  }

  /// If `storage`, which stores a record, is deinitializable in `self.insertionScope`, inserts
  /// the IR for deinitializing it, anchoring new instructions at `site`. Otherwise, reports a
  /// diagnostic for each part that isn't deinitializable.
  ///
  /// - Requires: the type of `storage` has a record layout.
  private mutating func emitDeinitRecordParts(of storage: Operand, at site: SourceRange) {
    let t = module.type(of: storage).ast
    precondition(t.hasRecordLayout)

    let layout = AbstractTypeLayout(of: t, definedIn: module.program)

    // If the object is empty, simply mark it uninitialized.
    if layout.properties.isEmpty {
      insert(module.makeMarkState(storage, initialized: false, at: site))
      return
    }

    // Otherwise, deinitialize each property.
    for i in layout.properties.indices {
      let x0 = insert(module.makeSubfieldView(of: storage, subfield: [i], at: site))!
      emitDeinit(x0, at: site)
    }
  }

  /// If `storage`, which stores a union. is deinitializable in `self.insertionScope`, inserts
  /// the IR for deinitializing it, anchoring new instructions at `site`. Otherwise, reports a
  /// diagnostic for each part that isn't deinitializable.
  ///
  /// - Requires: the type of `storage` is a union.
  private mutating func emitDeinitUnionPayload(of storage: Operand, at site: SourceRange) {
    let t = UnionType(module.type(of: storage).ast)!

    // If union is empty, simply mark it uninitialized.
    if t.elements.isEmpty {
      insert(module.makeMarkState(storage, initialized: false, at: site))
      return
    }

    // Trivial if the union has a single member.
    if let e = t.elements.uniqueElement {
      emitDeinitUnionPayload(of: storage, containing: e, at: site)
      return
    }

    // One successor per member in the union, ordered by their mangled representation.
    let elements = program.discriminatorToElement(in: t)
    var successors: [Block.ID] = []
    for _ in t.elements {
      successors.append(appendBlock())
    }

    let n = insert(module.makeUnionDiscriminator(storage, at: site))!
    insert(module.makeSwitch(on: n, toOneOf: successors, at: site))

    let tail = appendBlock()
    for i in 0 ..< elements.count {
      insertionPoint = .end(of: successors[i])
      emitDeinitUnionPayload(of: storage, containing: elements[i], at: site)
      insert(module.makeBranch(to: tail, at: site))
    }

    insertionPoint = .end(of: tail)
  }

  /// If `storage`, which stores a union container holding a `payload`, is deinitializable in
  /// `self.insertionScope`, inserts the IR for deinitializing it, anchoring new instructions at
  /// `site`. Otherwise, reports a diagnostic for each part that isn't deinitializable.
  private mutating func emitDeinitUnionPayload(
    of storage: Operand, containing payload: AnyType, at site: SourceRange
  ) {
    let x0 = insert(module.makeOpenUnion(storage, as: payload, at: site))!
    emitDeinit(x0, at: site)
    insert(module.makeCloseUnion(x0, at: site))
  }

  // MARK: Helpers

  /// Inserts the IR for aquiring `access` on `source` at `site`.
  private mutating func emitAcquire(
    _ access: AccessEffect, on source: Operand, at site: SourceRange
  ) -> Operand {
    switch access {
    case .let, .inout, .set:
      return insert(module.makeBorrow(access, from: source, at: site))!
    case .sink:
      return insert(module.makeLoad(source, at: site))!
    case .yielded:
      unreachable()
    }
  }

  /// Inserts a stack allocation for an object of type `t`.
  private mutating func emitAllocStack(
    for t: AnyType, at site: SourceRange
  ) -> Operand {
    let u = program.relations.canonical(t)
    let s = insert(module.makeAllocStack(u, at: site))!
    frames.top.allocs.append(s)
    return s
  }

  /// Appends the IR for computing the address of the given `subfield` of the record at
  /// `recordAddress` and returns the resulting address, anchoring new instructions at `site`.
  private mutating func emitSubfieldView(
    _ recordAddress: Operand, at subfield: RecordPath, at site: SourceRange
  ) -> Operand {
    if subfield.isEmpty { return recordAddress }
    return insert(module.makeSubfieldView(of: recordAddress, subfield: subfield, at: site))!
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
    withConformanceToMovable c: Core.Conformance,
    at site: SourceRange
  ) {
    let oper = module.demandMoveOperatorDeclaration(access, from: c)
    let move = Operand.constant(FunctionReference(to: oper, in: module))

    let x0 = insert(module.makeBorrow(access, from: storage, at: site))!
    let x1 = emitAllocStack(for: .void, at: site)
    let x2 = insert(module.makeBorrow(.set, from: x1, at: site))!
    insert(module.makeCall(applying: move, to: [x0, value], writingResultTo: x2, at: site))
  }

  /// Inserts the IR for deallocating each allocation in the top frame of `self.frames`, anchoring
  /// new instructions at `site`.
  private mutating func emitDeallocTopFrame(at site: SourceRange) {
    emitDeallocs(for: frames.top, at: site)
    frames.top.allocs.removeAll()
  }

  /// Inserts the IR for deallocating each allocation in `f`, anchoring new instructions at `site`.
  private mutating func emitDeallocs(for f: Frame, at site: SourceRange) {
    for a in f.allocs.reversed() {
      insert(module.makeDeallocStack(for: a, at: site))
    }
  }

  /// Emits the IR trapping iff `predicate`, which is an object of type `i1`, anchoring new
  /// instructions at `site`.
  private mutating func emitGuard(_ predicate: Operand, at site: SourceRange) {
    let failure = appendBlock()
    let success = appendBlock()
    insert(module.makeCondBranch(if: predicate, then: success, else: failure, at: site))

    insertionPoint = .end(of: failure)
    insert(module.makeUnreachable(at: site))
    insertionPoint = .end(of: success)
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
      emitDeallocTopFrame(at: .empty(atEndOf: ast[insertionScope!].site))
      frames.pop()
    }
    return action(&self)
  }

  /// Returns the result of calling `action` on a copy of `self` whose insertion block and frames
  /// are clear.
  private mutating func withClearContext<T>(_ action: (inout Self) throws -> T) rethrows -> T {
    var p: InsertionPoint? = nil
    var f = Stack()

    swap(&p, &insertionPoint)
    swap(&f, &frames)
    defer {
      swap(&p, &insertionPoint)
      swap(&f, &frames)
    }
    return try action(&self)
  }

}

extension Emitter {

  /// The local variables and allocations of a lexical scope.
  fileprivate struct Frame {

    /// A map from declaration of a local variable to its corresponding IR in the frame.
    var locals = DeclProperty<Operand>()

    /// The allocations in the frame, in FILO order.
    var allocs: [Operand] = []

  }

  /// A stack of frames.
  fileprivate struct Stack {

    /// The frames in the stack, ordered from bottom to top.
    private(set) var elements: [Frame] = []

    /// True iff the stack is empty.
    var isEmpty: Bool { elements.isEmpty }

    /// Accesses the top frame.
    ///
    /// - Requires: The stack is not empty.
    var top: Frame {
      get { elements[elements.count - 1] }
      _modify { yield &elements[elements.count - 1] }
    }

    /// Accesses the IR corresponding to `d`.
    ///
    /// - Requires: The stack is not empty.
    /// - Complexity: O(*n*) for read access where *n* is the number of frames in the stack. O(1)
    ///   for write access.
    subscript<T: DeclID>(d: T) -> Operand? {
      get {
        for frame in elements.reversed() {
          if let operand = frame.locals[d] { return operand }
        }
        return nil
      }
      set { top.locals[d] = newValue }
    }

    /// Pushes `newFrame` on the stack.
    mutating func push(_ newFrame: Frame = .init()) {
      elements.append(newFrame)
    }

    /// Pops the top frame.
    ///
    /// - Requires: The stack is not empty.
    @discardableResult
    mutating func pop() -> Frame {
      precondition(top.allocs.isEmpty, "stack leak")
      return elements.removeLast()
    }

  }

}

extension Diagnostic {

  static func error(assignmentLHSRequiresMutationMarkerAt site: SourceRange) -> Diagnostic {
    .error("left-hand side of assignment must be marked for mutation", at: site)
  }

  static func error(nonDeinitializable t: AnyType, at site: SourceRange) -> Diagnostic {
    .error("type '\(t)' is not deinitializable", at: site)
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

  static func error(missingReturn returnType: AnyType, at site: SourceRange) -> Diagnostic {
    .error("missing return in function expected to return '\(returnType)'", at: site)
  }

  static func warning(unreachableStatement s: AnyStmtID, in ast: AST) -> Diagnostic {
    .error("statement will never be executed", at: .empty(at: ast[s].site.first()))
  }

}
