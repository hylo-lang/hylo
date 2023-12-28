import BigInt
import Core
import FrontEnd
import Utils

/// Hylo's IR emitter.
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
/// - `incorporateSyntheticDeclarations`: generates the implementations of the synthesized
///   declarations that are notionally part of a module. This method is called after definite
///   deinitialization.
///
/// Other entry points may be used during IR passes (e.g., `emitDeinit`).
///
/// - Note: Unless documented otherwise, the methods of `Emitter` type insert IR in `self.module`
///   at `self.insertionPoint`, anchoring new instructions at the given source range, named `site`
///   in their parameter lists.
struct Emitter {

  /// The diagnostics of lowering errors.
  private var diagnostics: DiagnosticSet = []

  /// The module into which new IR is inserted.
  private var module: Module!

  /// A stack of frames describing the variables and allocations of each traversed lexical scope.
  private var frames = Stack()

  /// The loops in which control flow has currently entered.
  private var loops = LoopIDs()

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
    if let f = insertionFunction, let b = module.entry(of: f), !module[f].isSubscript {
      return .parameter(b, module[f].inputs.count)
    } else {
      return nil
    }
  }

  /// Reports the given diagnostic.
  private mutating func report(_ d: Diagnostic) {
    diagnostics.insert(d)
  }

  /// Appends a new basic block at the end of `self.insertionFunction`, defined in s.
  private mutating func appendBlock<T: ScopeID>(in s: T) -> Block.ID {
    module.appendBlock(in: s, to: insertionFunction!)
  }

  /// Appends a new basic block at the end of `self.insertionFunction`, defined in the same scope
  /// as `self.insertionBlock`.
  private mutating func appendBlock() -> Block.ID {
    appendBlock(in: insertionScope!)
  }

  /// Inserts `newInstruction` into `self.module` at the end of `self.insertionPoint`.
  ///
  /// - Requires: `self.insertionPoint` refers to the end of a block.
  @discardableResult
  private mutating func insert<I: Instruction>(_ newInstruction: I) -> Operand? {
    let i = module.insert(newInstruction, at: insertionPoint!)
    return module.result(of: i)
  }

  // MARK: Top-level entry points

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
    for u in program[module.id].decls {
      lower(topLevel: u)
    }
  }

  /// Inserts the IR for the synthesized declarations of `self.module`.
  mutating func incorporateSyntheticDeclarations() {
    for d in program.synthesizedDecls[module.id, default: []] {
      lower(synthetic: d)
    }

    // `lower(synthetic:)` may append additional declarations to `module.synthesizedDecls`.
    var i = 0
    while i < module.synthesizedDecls.count {
      lower(synthetic: module.synthesizedDecls[i])
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
    case VarDecl.self:
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
    let f = module.demandDeclaration(lowering: d)
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
      let returnType = LambdaType(program[d].type)!.output
      let returnSite = pushing(bodyFrame, { $0.lowerStatements(s, expecting: returnType) })
      insert(module.makeReturn(at: returnSite))

    case .expr(let e):
      pushing(bodyFrame, { $0.emitStore(value: e, to: $0.returnValue!) })
      insert(module.makeReturn(at: ast[e].site))
    }

    return f
  }

  /// Returns the frame enclosing the body of `d`, whose entry block is `entry`.
  private func outermostFrame(of d: FunctionDecl.ID, entering entry: Block.ID) -> Frame {
    var locals = DeclProperty<Operand>()

    for (i, c) in program.captures(of: d).enumerated() {
      locals[c] = .parameter(entry, i)
    }

    let captureCount = locals.count
    for (i, p) in ast[d].parameters.enumerated() {
      locals[p] = .parameter(entry, i + captureCount)
    }

    return Frame(locals: locals)
  }

  /// Inserts the IR for the statements of `b`, which is the body of a function returning instances
  /// of `returnType`, and returns the site of the return statement.
  private mutating func lowerStatements(
    _ b: BraceStmt.ID, expecting returnType: AnyType
  ) -> SourceRange {
    switch emit(braceStmt: b) {
    case .next:
      if canonical(returnType).isVoidOrNever {
        let anchor = SourceRange.empty(atEndOf: ast[b].site)
        insert(module.makeMarkState(returnValue!, initialized: true, at: anchor))
      }
      return ast[b].site

    case .return(let s):
      return ast[s].site

    default:
      UNIMPLEMENTED()
    }
  }

  /// Inserts the IR for calling `d`, which is a foreign function interface.
  private mutating func lower(ffi d: FunctionDecl.ID) {
    let f = module.demandDeclaration(lowering: d)

    // Configure the emitter context.
    let entry = module.appendEntry(in: d, to: f)

    self.insertionPoint = .end(of: entry)
    self.frames.push()
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    let site = ast[d].site

    // Convert Hylo arguments to their foreign representation. Note that the last parameter of the
    // entry is the address of the FFI's return value.
    var arguments: [Operand] = []
    for i in 0 ..< module[entry].inputs.count - 1 {
      let a = emitConvertToForeign(.parameter(entry, i), at: site)
      arguments.append(a)
    }

    // Return type must be foreign convertible unless it is `Void` or `Never`.
    let returnType = read(module.functions[f]!.output) { (t) in
      t.isVoidOrNever ? t : program.foreignRepresentation(of: t, exposedTo: insertionScope!)
    }

    // Emit the call to the foreign function.
    let foreignResult = insert(
      module.makeCallFFI(
        returning: .object(returnType), applying: ast[d].foreignName!, to: arguments, at: site))!

    // Convert the result of the FFI to its Hylo representation and return it.
    switch returnType {
    case .never:
      insert(module.makeUnreachable(at: site))

    case .void:
      insert(module.makeMarkState(returnValue!, initialized: true, at: site))
      emitDeallocTopFrame(at: site)
      insert(module.makeReturn(at: site))

    default:
      let v = emitConvert(foreign: foreignResult, to: module.functions[f]!.output, at: site)
      emitMove([.set], v, to: returnValue!, at: site)
      emitDeallocTopFrame(at: site)
      insert(module.makeReturn(at: site))
    }
  }

  /// Inserts the IR for `d`.
  private mutating func lower(initializer d: InitializerDecl.ID) {
    // Nothing to do for memberwise initializer.
    if ast[d].isMemberwise { return }
    let f = module.demandDeclaration(lowering: d)

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
    let bodyFrame = Frame(locals: locals)
    let returnSite = pushing(bodyFrame, { $0.lowerStatements($0.ast[d].body!, expecting: .void) })

    let receiverLayout = AbstractTypeLayout(of: program[d].receiver.type, definedIn: program)

    // If the object is empty, simply mark it initialized.
    if receiverLayout.properties.isEmpty {
      let receiver = entry.parameter(0)
      insert(module.makeMarkState(receiver, initialized: true, at: ast[d].site))
    }
    insert(module.makeReturn(at: returnSite))
  }

  /// Inserts the IR for `d`.
  private mutating func lower(method d: MethodDecl.ID) {
    for i in ast[d].impls {
      lower(methodImpl: i)
    }
  }

  /// Inserts the IR for `d`.
  private mutating func lower(methodImpl d: MethodImpl.ID) {
    let f = module.demandDeclaration(lowering: d)
    guard let b = ast[d].body else { return }

    // Create the function entry.
    let entry = module.appendEntry(in: program.scopeContainingBody(of: d)!, to: f)

    // Configure the locals.
    var locals = DeclProperty<Operand>()
    locals[ast[d].receiver] = .parameter(entry, 0)

    let bundle = MethodDecl.ID(program[d].scope)!
    for (i, p) in ast[bundle].parameters.enumerated() {
      locals[p] = .parameter(entry, i + 1)
    }

    let bodyFrame = Frame(locals: locals)

    // Emit the body.
    self.insertionPoint = .end(of: entry)
    switch b {
    case .block(let s):
      let returnType = LambdaType(program[d].type)!.output
      let returnSite = pushing(bodyFrame, { $0.lowerStatements(s, expecting: returnType) })
      insert(module.makeReturn(at: returnSite))

    case .expr(let e):
      pushing(bodyFrame, { $0.emitStore(value: e, to: $0.returnValue!) })
      insert(module.makeReturn(at: ast[e].site))
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
    let f = module.demandDeclaration(lowering: d)
    guard let b = ast[d].body else { return }

    // Create the function entry.
    let entry = module.appendEntry(in: program.scopeContainingBody(of: d)!, to: f)

    // Configure the locals.
    var locals = DeclProperty<Operand>()

    let bundle = SubscriptDecl.ID(program[d].scope)!
    let explicit = program[bundle].explicitCaptures
    let implicit = program[bundle].implicitCaptures

    // Explicit captures appear first.
    for (i, c) in explicit.enumerated() {
      locals[c] = .parameter(entry, i)
    }

    // Implicit captures appear next.
    for (i, c) in implicit.enumerated() {
      locals[c.decl] = .parameter(entry, i + explicit.count)
    }

    // Receiver appears next.
    var captureCount = explicit.count + implicit.count
    if let r = ast[d].receiver {
      locals[r] = .parameter(entry, captureCount)
      captureCount += 1
    }

    // Explicit parameters appear last.
    for (i, p) in ast[bundle].parameters.enumerated() {
      locals[p] = .parameter(entry, i + captureCount)
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
          this.module.makeAccess(this.ast[d].introducer.value, from: x0, at: this.ast[e].site))!
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
      UNIMPLEMENTED()
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
    module.addProductType(ProductType(d, ast: program.ast))
    lower(members: ast[d].members)
  }

  /// Inserts the IR for `d`.
  private mutating func lower(trait d: TraitDecl.ID) {
    module.addTrait(TraitType(d, ast: program.ast))
  }

  /// Inserts the IR for given declaration `members`.
  private mutating func lower(members: [AnyDeclID]) {
    for m in members {
      switch m.kind {
      case FunctionDecl.self:
        lower(function: .init(m)!)
      case InitializerDecl.self:
        lower(initializer: .init(m)!)
      case MethodDecl.self:
        lower(method: .init(m)!)
      case ProductTypeDecl.self:
        lower(product: .init(m)!)
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
    precondition(program.isGlobal(d))
    precondition(read(program[d].pattern.introducer.value, { ($0 == .let) || ($0 == .sinklet) }))

    let r = RemoteType(.set, program[d].type)
    let l = LambdaType(
      receiverEffect: .set, environment: ^TupleType(types: [^r]), inputs: [], output: .void)
    let f = SynthesizedFunctionDecl(.globalInitialization(d), typed: l, in: program[d].scope)
    let i = lower(globalBindingInitializer: f)
    let t = program.canonical(r.bareType, in: program[d].scope)
    let s = StaticStorage(t, identifiedBy: AnyDeclID(d), initializedWith: i)
    module.addStaticStorage(s)
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

    emitLocalDeclarations(introducedBy: lhs, referringTo: [], relativeTo: storage)
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
    let lhs = emitLocalDeclaration(of: name, referringTo: subfield, relativeTo: storage)
    emitStore(convertingIfNecessary: initializer, to: lhs)
  }

  /// Inserts the IR to declare and initialize the names in `lhs`, which refer to subobjects of
  /// `subfield` relative to `storage`, by consuming the value of `initializer`.
  private mutating func emitInitStoredLocalBindings(
    in lhs: TuplePattern.ID, referringTo subfield: RecordPath, relativeTo storage: Operand,
    consuming initializer: AnyExprID
  ) {
    let rhs = emitSubfieldView(storage, at: subfield, at: ast[lhs].site)
    emitStore(value: initializer, to: rhs)
    emitLocalDeclarations(introducedBy: lhs, referringTo: subfield, relativeTo: storage)
  }

  /// Inserts the IR to declare the names in `pattern`, which refer to parts of `subfield` relative
  /// to `storage`.
  private mutating func emitLocalDeclarations<T: PatternID>(
    introducedBy pattern: T,
    referringTo subfield: RecordPath, relativeTo storage: Operand
  ) {
    for (path, name) in ast.names(in: pattern) {
      _ = emitLocalDeclaration(of: name, referringTo: subfield + path, relativeTo: storage)
    }
  }

  /// Inserts the IR to declare `name`, which refers to `subfield` relative to `storage`, returning
  /// that sub-location.
  private mutating func emitLocalDeclaration(
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
    let access = AccessEffect(program[d].pattern.introducer.value)
    precondition(access == .let || access == .inout)
    precondition(program.isLocal(d))

    let initializer = ast[d].initializer!
    let source = emitLValue(initializer)
    let isSink = module.isSink(source)

    for (path, name) in ast.names(in: program[d].pattern.subpattern) {
      var part = emitSubfieldView(source, at: path, at: program[name].decl.site)
      let partDecl = ast[name].decl

      let t = canonical(program[partDecl].type)
      part = emitCoerce(part, to: t, at: ast[partDecl].site)

      if isSink {
        let b = module.makeAccess(
          [.sink, access], from: part, correspondingTo: partDecl, at: ast[partDecl].site)
        frames[partDecl] = insert(b)!
      } else {
        let b = module.makeAccess(
          access, from: part, correspondingTo: partDecl, at: ast[partDecl].site)
        frames[partDecl] = insert(b)!
      }
    }
  }

  // MARK: Synthetic declarations

  /// Synthesizes the implementation of `d`.
  private mutating func lower(synthetic d: SynthesizedFunctionDecl) {
    switch d.kind {
    case .deinitialize:
      return withClearContext({ $0.lower(syntheticDeinit: d) })
    case .moveInitialization:
      return withClearContext({ $0.lower(syntheticMoveInit: d) })
    case .moveAssignment:
      return withClearContext({ $0.lower(syntheticMoveAssign: d) })
    case .copy:
      return withClearContext({ $0.lower(syntheticCopy: d) })
    case .globalInitialization:
      return withClearContext({ $0.lower(globalBindingInitializer: d) })
    case .autoclosure:
      // nothing do to here; expansion is done at the caller side.
      break
    }
  }

  /// Inserts the IR for `d`, which is a synthetic deinitializer.
  private mutating func lower(syntheticDeinit d: SynthesizedFunctionDecl) {
    let f = module.demandDeclaration(lowering: d)
    if !shouldEmitBody(of: d, loweredTo: f) { return }

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

    insert(module.makeMarkState(returnValue!, initialized: true, at: site))
    emitDeallocTopFrame(at: site)
    insert(module.makeReturn(at: site))
  }

  /// Inserts the IR for `d`, which is a synthetic move initialization method.
  private mutating func lower(syntheticMoveInit d: SynthesizedFunctionDecl) {
    let f = module.demandDeclaration(lowering: d)
    if !shouldEmitBody(of: d, loweredTo: f) { return }

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

    insert(module.makeMarkState(returnValue!, initialized: true, at: site))
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
      emitMove([.set], source, to: target, at: site)
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

    let n = emitUnionDiscriminator(argument, at: site)
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
    let x0 = insert(
      module.makeOpenUnion(receiver, as: payload, forInitialization: true, at: site))!
    emitDeinit(x0, at: site)

    // Move the argument.
    let x1 = insert(module.makeOpenUnion(argument, as: payload, at: site))!
    emitMove([.set], x1, to: x0, at: site)

    // Close the unions.
    insert(module.makeCloseUnion(x0, at: site))
    insert(module.makeCloseUnion(x1, at: site))
  }

  /// Inserts the IR for `d`, which is a synthetic move initialization method.
  private mutating func lower(syntheticMoveAssign d: SynthesizedFunctionDecl) {
    let f = module.demandDeclaration(lowering: d)
    if !shouldEmitBody(of: d, loweredTo: f) { return }

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
    emitMove([.set], argument, to: receiver, at: site)
    insert(module.makeMarkState(returnValue!, initialized: true, at: site))
    emitDeallocTopFrame(at: site)
    insert(module.makeReturn(at: site))
  }

  /// Inserts the IR for `d`, which is a synthetic copy method.
  private mutating func lower(syntheticCopy d: SynthesizedFunctionDecl) {
    let f = module.demandDeclaration(lowering: d)
    if !shouldEmitBody(of: d, loweredTo: f) { return }

    let site = ast[module.id].site
    let entry = module.appendEntry(in: d.scope, to: f)
    insertionPoint = .end(of: entry)
    self.frames.push()
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    let source = Operand.parameter(entry, 0)
    let target = Operand.parameter(entry, 1)
    let object = module.type(of: source).ast

    if object.hasRecordLayout {
      emitCopyRecordParts(from: source, to: target, at: site)
    } else if object.base is UnionType {
      emitCopyUnionPayload(from: source, to: target, at: site)
    }

    emitDeallocTopFrame(at: site)
    insert(module.makeReturn(at: site))
  }

  /// Inserts the IR for copying the stored parts of `source`, which stores a record, to `target`
  /// at `site`.
  private mutating func emitCopyRecordParts(
    from source: Operand, to target: Operand, at site: SourceRange
  ) {
    let layout = AbstractTypeLayout(of: module.type(of: source).ast, definedIn: program)

    // If the object is empty, simply mark the target as initialized.
    if layout.properties.isEmpty {
      insert(module.makeMarkState(target, initialized: true, at: site))
      return
    }

    // Otherwise, copy each property.
    for i in layout.properties.indices {
      let s = emitSubfieldView(source, at: [i], at: site)
      let t = emitSubfieldView(target, at: [i], at: site)
      emitCopy(s, to: t, at: site)
    }
  }

  /// Inserts the IR for copying `source`, which stores a union container, to `target` at `site`.
  private mutating func emitCopyUnionPayload(
    from source: Operand, to target: Operand, at site: SourceRange
  ) {
    let t = UnionType(module.type(of: source).ast)!

    // If union is empty, simply mark the target as initialized.
    if t.elements.isEmpty {
      insert(module.makeMarkState(target, initialized: true, at: site))
      return
    }

    // Trivial if the union has a single member.
    if let e = t.elements.uniqueElement {
      emitCopyUnionPayload(from: source, containing: e, to: target, at: site)
      return
    }

    // Otherwise, use a switch to select the correct move-initialization.
    let elements = program.discriminatorToElement(in: t)
    var successors: [Block.ID] = []
    for _ in t.elements {
      successors.append(appendBlock())
    }

    let n = emitUnionDiscriminator(source, at: site)
    insert(module.makeSwitch(on: n, toOneOf: successors, at: site))

    let tail = appendBlock()
    for i in 0 ..< elements.count {
      insertionPoint = .end(of: successors[i])
      emitCopyUnionPayload(from: source, containing: elements[i], to: target, at: site)
      insert(module.makeBranch(to: tail, at: site))
    }

    insertionPoint = .end(of: tail)
  }

  /// Inserts the IR for copying `source`, which stores a union containing a `payload`, to `target`
  /// at `site`.
  private mutating func emitCopyUnionPayload(
    from source: Operand, containing payload: AnyType, to target: Operand, at site: SourceRange
  ) {
    let x0 = insert(module.makeOpenUnion(source, as: payload, at: site))!
    let x1 = insert(module.makeOpenUnion(target, as: payload, forInitialization: true, at: site))!
    emitCopy(x0, to: x1, at: site)
    insert(module.makeCloseUnion(x0, at: site))
    insert(module.makeCloseUnion(x1, at: site))
  }

  /// Inserts the IR for lowering `d`, which is a global binding initializer, returning the ID of
  /// the lowered function.
  @discardableResult
  private mutating func lower(globalBindingInitializer d: SynthesizedFunctionDecl) -> Function.ID {
    let f = module.demandDeclaration(lowering: d)
    if !shouldEmitBody(of: d, loweredTo: f) {
      return f
    }

    let entry = module.appendEntry(in: d.scope, to: f)
    insertionPoint = .end(of: entry)
    self.frames.push()
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    let storage = Operand.parameter(entry, 0)
    guard case .globalInitialization(let binding) = d.kind else { unreachable() }
    let initializer = program[binding].initializer!

    emitInitStoredLocalBindings(
      in: program[binding].pattern.subpattern, referringTo: [], relativeTo: storage,
      consuming: initializer)
    insert(module.makeMarkState(returnValue!, initialized: true, at: program[initializer].site))
    emitDeallocTopFrame(at: program[initializer].site)
    insert(module.makeReturn(at: program[initializer].site))

    return f
  }

  private mutating func lower(syntheticAutoclosure d: SynthesizedFunctionDecl) -> Function.ID {
    guard case .autoclosure(let argument) = d.kind else { unreachable() }
    let f = module.demandDeclaration(lowering: d)
    let entry = module.appendEntry(in: d.scope, to: f)

    insertionPoint = .end(of: entry)
    self.frames.push()
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    // Emit the body.
    emitStore(value: argument, to: returnValue!)
    insert(module.makeReturn(at: ast[argument].site))

    return f
  }

  /// Returns `true` if the body of `d`, which has been lowered to `f` in `self`, has yet to be
  /// generated in `self`.
  private func shouldEmitBody(of d: SynthesizedFunctionDecl, loweredTo f: Function.ID) -> Bool {
    (module[f].entry == nil) && (program.module(containing: d.scope) == module.id)
  }

  // MARK: Statements

  /// The description of the next action a program should execute.
  private enum ControlFlow: Equatable {

    /// Move to the next statement.
    case next

    /// Return from the current function.
    case `return`(ReturnStmt.ID)

    /// Break from the innermost loop.
    case `break`(BreakStmt.ID)

    /// Continue the innermost loop.
    case `continue`(ContinueStmt.ID)

  }

  /// Inserts IR for handling the given control flow, applying `handleNext` to generate the IR
  /// corresponding to `.next`.
  private mutating func emitControlFlow(
    _ f: ControlFlow, handlingNextWith handleNext: (inout Self) -> Void
  ) {
    switch f {
    case .next:
      handleNext(&self)
    case .return(let s):
      emitControlFlow(return: s)
    case .break(let s):
      emitControlFlow(break: s)
    default:
      UNIMPLEMENTED()
    }
  }

  /// Inserts IR for returning from current function, anchoring instructions at `s`.
  private mutating func emitControlFlow(return s: ReturnStmt.ID) {
    for f in frames.elements.reversed() {
      emitDeallocs(for: f, at: ast[s].site)
    }
    insert(module.makeReturn(at: ast[s].site))
  }

  /// Inserts IR for breaking from innermost loop, anchoring instructions at `s`.
  private mutating func emitControlFlow(break s: BreakStmt.ID) {
    let innermost = loops.last!
    for f in frames.elements[frames.depth...].reversed() {
      emitDeallocs(for: f, at: ast[s].site)
    }
    insert(module.makeBranch(to: innermost.exit, at: ast[s].site))
  }

  /// Inserts the IR for `s`, returning its effect on control flow.
  private mutating func emit<T: StmtID>(stmt s: T) -> ControlFlow {
    switch s.kind {
    case AssignStmt.self:
      return emit(assignStmt: .init(s)!)
    case BraceStmt.self:
      return emit(braceStmt: .init(s)!)
    case BreakStmt.self:
      return emit(breakStmt: .init(s)!)
    case ConditionalCompilationStmt.self:
      return emit(condCompilationStmt: .init(s)!)
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
    case ForStmt.self:
      return emit(forStmt: .init(s)!)
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
    // The LHS should must be marked for mutation even if the statement denotes initialization.
    guard program[s].left.kind == InoutExpr.self else {
      let p = program[s].left.site.first()
      report(.error(assignmentLHSRequiresMutationMarkerAt: .empty(at: p)))
      return .next
    }

    // The RHS is evaluated first, stored into some local storage, and moved to the LHS. Implicit
    // conversion is necessary if the RHS is subtype of the LHS.
    let rhs = emitAllocStack(for: program[s].left.type, at: ast[s].site)
    emitStore(convertingIfNecessary: ast[s].right, to: rhs)
    let lhs = emitLValue(ast[s].left)
    emitMove([.inout, .set], rhs, to: lhs, at: ast[s].site)

    return .next
  }

  private mutating func emit(braceStmt s: BraceStmt.ID) -> ControlFlow {
    frames.push()
    defer {
      emitDeallocTopFrame(at: .empty(atEndOf: ast[s].site))
      frames.pop()
    }

    return emit(stmtList: ast[s].stmts)
  }

  private mutating func emit(stmtList stmts: [AnyStmtID]) -> ControlFlow {
    for i in stmts.indices {
      let a = emit(stmt: stmts[i])
      if a == .next { continue }

      // Exit the scope early if `i` was a control-flow statement, complaining if it wasn't the
      // last statement of the code block.
      if i != stmts.count - 1 {
        report(.warning(unreachableStatement: stmts[i + 1], in: ast))
      }
      return a
    }

    return .next
  }

  private mutating func emit(breakStmt s: BreakStmt.ID) -> ControlFlow {
    return .break(s)
  }

  private mutating func emit(condCompilationStmt s: ConditionalCompilationStmt.ID) -> ControlFlow {
    return emit(stmtList: ast[s].expansion(for: ast.compiler))
  }

  private mutating func emit(conditionalStmt s: ConditionalStmt.ID) -> ControlFlow {
    let (firstBranch, secondBranch) = emitTest(condition: ast[s].condition, in: AnyScopeID(s))
    let tail = appendBlock()

    insertionPoint = .end(of: firstBranch)
    let f1 = emit(braceStmt: ast[s].success)
    emitControlFlow(f1) { (me) in
      me.insert(me.module.makeBranch(to: tail, at: me.ast[s].site))
    }

    insertionPoint = .end(of: secondBranch)
    guard let failure = ast[s].failure else {
      insert(module.makeBranch(to: tail, at: ast[s].site))
      insertionPoint = .end(of: tail)
      return .next
    }

    let f2 = emit(stmt: failure.value)
    emitControlFlow(f2) { (me) in
      me.insert(me.module.makeBranch(to: tail, at: me.ast[s].site))
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
    let body = appendBlock(in: ast[s].body)
    let exit = appendBlock(in: ast[s].body)
    loops.append(LoopID(depth: frames.depth, exit: exit))
    defer { loops.removeLast() }

    insert(module.makeBranch(to: body, at: .empty(at: ast[s].site.first())))
    insertionPoint = .end(of: body)

    // We're not using `emit(braceStmt:into:)` because we need to evaluate the loop condition
    // before exiting the scope.
    frames.push()

    let statements = program[s].body.stmts
    for i in statements.indices {
      let flow = emit(stmt: statements[i])
      if flow == .next { continue }
      emitControlFlow(flow, handlingNextWith: { _ in unreachable() })

      // Exit the scope early if `i` was a control-flow statement, complaining if it wasn't the
      // last statement of the code block.
      if i != statements.count - 1 {
        report(.warning(unreachableStatement: statements[i + 1], in: ast))
      }

      return flow
    }

    let condition = ast[s].condition.value
    let c = emit(branchCondition: condition)
    emitDeallocTopFrame(at: ast[s].site)
    frames.pop()

    insert(module.makeCondBranch(if: c, then: body, else: exit, at: ast[condition].site))
    insertionPoint = .end(of: exit)
    return .next
  }

  private mutating func emit(exprStmt s: ExprStmt.ID) -> ControlFlow {
    let v = emitStore(value: ast[s].expr)
    emitDeinit(v, at: ast[s].site)
    return .next
  }

  /// Inserts the IR for loop `s`, returning its effect on control flow.
  private mutating func emit(forStmt s: ForStmt.ID) -> ControlFlow {
    if program.ast.isConsuming(s) {
      UNIMPLEMENTED("consuming for loops")
    } else {
      return emit(nonConsumingForStmt: s)
    }
  }

  /// Inserts the IR for non-consuming loop `s`, returning its effect on control flow.
  private mutating func emit(nonConsumingForStmt s: ForStmt.ID) -> ControlFlow {
    let domainType = program[program[s].domain.value].type

    let collection = ast.core.collection
    let collectionConformance = program.conformance(
      of: domainType, to: collection.type, exposedTo: program[s].scope)!
    let collectionWitness = CollectionWitness(collectionConformance, in: &module)

    let equatable = ast.core.equatable
    let equatableConformance = program.conformance(
      of: collectionWitness.position, to: equatable.type, exposedTo: program[s].scope)!
    let equal = module.reference(toImplementationOf: equatable.equal, for: equatableConformance)

    let introducer = program[s].introducerSite

    // The collection on which the loop iterates.
    let domain = emitLValue(program[s].domain.value)
    // The storage allocated for the result of the exit condition.
    let quit = emitAllocStack(for: ^ast.coreType("Bool")!, at: introducer)
    // The start and end positions of the collection; the former is updated with each iteration.
    let (currentPosition, endPosition) = emitPositions(
      forIteratingOver: domain, usingWitness: collectionWitness, at: introducer)

    // The "head" of the loop; tests for the exit condition.
    let head = appendBlock(in: s)
    // The "lens" of the loop; tests for narrowing conditions and applies filters.
    let enter = appendBlock(in: s)
    // The "tail" of the loop; increments the index and jumps back to the head.
    let tail = appendBlock(in: s)
    // The remainder of the program, after the loop.
    let exit = appendBlock()

    loops.append(LoopID(depth: frames.depth, exit: exit))
    defer { loops.removeLast() }

    insert(module.makeBranch(to: head, at: introducer))

    insertionPoint = .end(of: head)
    let x0 = insert(module.makeAccess(.let, from: currentPosition, at: introducer))!
    let x1 = insert(module.makeAccess(.let, from: endPosition, at: introducer))!
    emitApply(.constant(equal), to: [x0, x1], writingResultTo: quit, at: introducer)
    insert(module.makeEndAccess(x1, at: introducer))
    insert(module.makeEndAccess(x0, at: introducer))
    let x2 = emitLoadBuiltinBool(quit, at: introducer)
    insert(module.makeCondBranch(if: x2, then: exit, else: enter, at: introducer))

    insertionPoint = .end(of: enter)
    let x6 = insert(module.makeAccess(.let, from: domain, at: introducer))!
    let x7 = insert(module.makeAccess(.let, from: currentPosition, at: introducer))!
    let x8 = insert(
      module.makeProject(
        .init(.let, collectionWitness.element), applying: collectionWitness.access,
        specializedBy: collectionConformance.arguments, to: [x6, x7], at: introducer))!

    if module.type(of: x8).ast != collectionWitness.element {
      UNIMPLEMENTED("narrowing projections #1099")
    }

    emitLocalDeclarations(
      introducedBy: program[s].binding.pattern, referringTo: [], relativeTo: x8)

    // TODO: Filter

    let flow = emit(braceStmt: ast[s].body)
    emitControlFlow(flow) { (me) in
      me.insert(me.module.makeBranch(to: tail, at: .empty(atEndOf: me.program[s].body.site)))
    }

    insertionPoint = .end(of: tail)
    let x3 = insert(module.makeAllocStack(collectionWitness.position, at: introducer))!
    let x4 = insert(module.makeAccess(.let, from: domain, at: introducer))!
    let x5 = insert(module.makeAccess(.let, from: currentPosition, at: introducer))!
    emitApply(collectionWitness.positionAfter, to: [x4, x5], writingResultTo: x3, at: introducer)
    insert(module.makeEndAccess(x4, at: introducer))
    insert(module.makeEndAccess(x5, at: introducer))
    emitMove([.inout], x3, to: currentPosition, at: introducer)
    insert(module.makeDeallocStack(for: x3, at: introducer))
    insert(module.makeBranch(to: head, at: introducer))

    insertionPoint = .end(of: exit)
    return .next
  }

  /// Inserts the IR for initializing the start and end positions of `domain`, whose conformance to
  /// the collection trait is witnessed by `witness`, returning these positions.
  private mutating func emitPositions(
    forIteratingOver domain: Operand,
    usingWitness witness: CollectionWitness, at site: SourceRange
  ) -> (startIndex: Operand, endIndex: Operand) {
    let start = emitAllocStack(for: witness.position, at: site)
    let end = emitAllocStack(for: witness.position, at: site)

    let x0 = insert(module.makeAccess(.let, from: domain, at: site))!
    emitApply(witness.startPosition, to: [x0], writingResultTo: start, at: site)
    emitApply(witness.endPosition, to: [x0], writingResultTo: end, at: site)
    insert(module.makeEndAccess(x0, at: site))

    return (startIndex: start, endIndex: end)
  }

  /// Inserts the IR for `s`, returning its effect on control flow.
  private mutating func emit(returnStmt s: ReturnStmt.ID) -> ControlFlow {
    if module[insertionFunction!].isSubscript {
      report(.error(returnInSubscript: s, in: ast))
      return .next
    }

    if let e = ast[s].value {
      emitStore(value: e, to: returnValue!)
    } else {
      insert(module.makeMarkState(returnValue!, initialized: true, at: ast[s].site))
    }

    // The return instruction is emitted by the caller handling this control-flow effect.
    return .return(s)
  }

  private mutating func emit(whileStmt s: WhileStmt.ID) -> ControlFlow {
    // Enter the loop.
    let head = appendBlock(in: s)
    insert(module.makeBranch(to: head, at: .empty(at: ast[s].site.first())))

    // Test the conditions.
    insertionPoint = .end(of: head)
    let (body, exit) = emitTest(condition: ast[s].condition, in: AnyScopeID(s))

    // Add the current loop to the emitter context.
    loops.append(LoopID(depth: frames.depth, exit: exit))
    defer { loops.removeLast() }

    // Execute the body.
    insertionPoint = .end(of: body)
    let flow = emit(braceStmt: ast[s].body)
    emitControlFlow(flow) { (me) in
      me.insert(me.module.makeBranch(to: head, at: .empty(atEndOf: me.program[s].body.site)))
    }

    // Exit.
    insertionPoint = .end(of: exit)
    return .next
  }

  private mutating func emit(yieldStmt s: YieldStmt.ID) -> ControlFlow {
    // TODO: Read mutability of current subscript

    let x0 = emitLValue(ast[s].value)
    let x1 = insert(module.makeAccess(.let, from: x0, at: ast[s].site))!
    insert(module.makeYield(.let, x1, at: ast[s].site))
    return .next
  }

  // MARK: Values

  /// Inserts the IR for initializing `storage` by storing `value` to it.
  private mutating func emitInitialize(
    storage: Operand, to value: Operand, at site: SourceRange
  ) {
    let x0 = insert(module.makeAccess(.set, from: storage, at: site))!
    insert(module.makeStore(value, at: x0, at: site))
    insert(module.makeEndAccess(x0, at: site))
  }

  /// Inserts the IR for storing the value of `e` to a fresh stack allocation, returning the
  /// address of this allocation.
  @discardableResult
  private mutating func emitStore<T: ExprID>(value e: T) -> Operand {
    let s = emitAllocStack(for: program[e].type, at: ast[e].site)
    emitStore(value: e, to: s)
    return s
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  ///
  /// - Requires: `storage` is the address of some uninitialized memory block capable of storing
  ///   the value of `e`.
  private mutating func emitStore<T: ExprID>(value e: T, to storage: Operand) {
    switch e.kind {
    case BooleanLiteralExpr.self:
      emitStore(BooleanLiteralExpr.ID(e)!, to: storage)
    case BufferLiteralExpr.self:
      emitStore(BufferLiteralExpr.ID(e)!, to: storage)
    case CastExpr.self:
      emitStore(CastExpr.ID(e)!, to: storage)
    case ConditionalExpr.self:
      emitStore(ConditionalExpr.ID(e)!, to: storage)
    case FloatLiteralExpr.self:
      emitStore(FloatLiteralExpr.ID(e)!, to: storage)
    case FunctionCallExpr.self:
      emitStore(FunctionCallExpr.ID(e)!, to: storage)
    case IntegerLiteralExpr.self:
      emitStore(IntegerLiteralExpr.ID(e)!, to: storage)
    case LambdaExpr.self:
      emitStore(LambdaExpr.ID(e)!, to: storage)
    case NameExpr.self:
      emitStore(NameExpr.ID(e)!, to: storage)
    case PragmaLiteralExpr.self:
      emitStore(PragmaLiteralExpr.ID(e)!, to: storage)
    case RemoteExpr.self:
      emitStore(RemoteExpr.ID(e)!, to: storage)
    case SequenceExpr.self:
      emitStore(SequenceExpr.ID(e)!, to: storage)
    case SubscriptCallExpr.self:
      emitStore(SubscriptCallExpr.ID(e)!, to: storage)
    case StringLiteralExpr.self:
      emitStore(StringLiteralExpr.ID(e)!, to: storage)
    case TupleExpr.self:
      emitStore(TupleExpr.ID(e)!, to: storage)
    case TupleMemberExpr.self:
      emitStore(TupleMemberExpr.ID(e)!, to: storage)
    default:
      unexpected(e, in: ast)
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: BooleanLiteralExpr.ID, to storage: Operand) {
    let x0 = emitSubfieldView(storage, at: [0], at: ast[e].site)
    let x1 = insert(module.makeAccess(.set, from: x0, at: ast[e].site))!
    insert(module.makeStore(.i1(ast[e].value), at: x1, at: ast[e].site))
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: BufferLiteralExpr.ID, to storage: Operand) {
    if program[e].elements.isEmpty {
      insert(module.makeMarkState(storage, initialized: true, at: program[e].site))
      return
    }

    // The elements of a buffer literal have the same type.
    for (i, v) in program[e].elements.enumerated() {
      let x0 = insert(module.makeAdvanced(storage, byStrides: i, at: program[v].site))!
      emitStore(value: v, to: x0)
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: CastExpr.ID, to storage: Operand) {
    switch ast[e].direction {
    case .up:
      emitStore(upcast: e, to: storage)
    case .down:
      emitStore(downcast: e, to: storage)
    case .pointerConversion:
      emitStore(pointerConversion: e, to: storage)
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(upcast e: CastExpr.ID, to storage: Operand) {
    assert(ast[e].direction == .up)
    let target = canonical(program[e].type)
    let source = canonical(program[ast[e].left].type)

    // `A ~> A`
    if program.areEquivalent(source, target, in: program[e].scope) {
      emitStore(value: ast[e].left, to: storage)
      return
    }

    // `A ~> Union<A, B>`
    if let u = UnionType(target), u.elements.contains(source) {
      let x0 = insert(
        module.makeOpenUnion(storage, as: source, forInitialization: true, at: program[e].site))!
      emitStore(value: ast[e].left, to: x0)
      insert(module.makeCloseUnion(x0, at: program[e].site))
      return
    }

    // Otherwise, wrap the LHS.
    UNIMPLEMENTED("unimplemented conversion from '\(source)' to '\(target)'")
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(downcast e: CastExpr.ID, to storage: Operand) {
    assert(ast[e].direction == .down)
    let target = program[ast[e].right].type

    // Store the LHS to `storage` if it already has the desired type.
    if program.areEquivalent(program[ast[e].left].type, target, in: program[e].scope) {
      emitStore(value: ast[e].left, to: storage)
      return
    }

    // Otherwise, unpack or repack the LHS.
    let lhs = emitStore(value: ast[e].left)

    // TODO
    _ = lhs
    UNIMPLEMENTED()
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(pointerConversion e: CastExpr.ID, to storage: Operand) {
    let x0 = emitLValue(pointerConversion: e)

    // Consuming a pointee requires a conformance to `Movable`.
    let target = RemoteType(canonical(program[e].type))!
    let movable = program.ast.core.movable.type
    if !program.conforms(target.bareType, to: movable, in: insertionScope!) {
      report(.error(module.type(of: x0).ast, doesNotConformTo: movable, at: ast[e].site))
      return
    }

    emitMove([.inout, .set], x0, to: storage, at: ast[e].site)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: ConditionalExpr.ID, to storage: Operand) {
    let (success, failure) = emitTest(condition: ast[e].condition, in: AnyScopeID(e))
    let tail = appendBlock()

    // Emit the success branch.
    insertionPoint = .end(of: success)
    pushing(Frame(), { $0.emitStore(value: $0.ast[e].success, to: storage) })
    insert(module.makeBranch(to: tail, at: ast[e].site))

    // Emit the failure branch.
    insertionPoint = .end(of: failure)
    pushing(Frame(), { $0.emitStore(value: $0.ast[e].failure.value, to: storage) })
    insert(module.makeBranch(to: tail, at: ast[e].site))

    insertionPoint = .end(of: tail)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: FloatLiteralExpr.ID, to storage: Operand) {
    emitStore(numericLiteral: e, to: storage)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: FunctionCallExpr.ID, to storage: Operand) {
    // Handle built-ins and constructor calls.
    if let n = NameExpr.ID(ast[e].callee) {
      switch program[n].referredDecl {
      case .builtinFunction(let f):
        let x0 = emit(apply: f, to: ast[e].arguments, at: ast[e].site)
        let x1 = insert(module.makeAccess(.set, from: storage, at: ast[e].site))!
        insert(module.makeStore(x0, at: x1, at: ast[e].site))
        return

      case .constructor:
        emit(constructorCall: e, initializing: storage)
        return

      default:
        break
      }
    }

    // Explicit arguments are evaluated first, from left to right.
    let explicitArguments = emitArguments(
      to: ast[e].callee, in: CallID(e),
      usingExplicit: ast[e].arguments, synthesizingDefaultAt: .empty(atEndOf: ast[e].site))

    // Callee and captures are evaluated next.
    let (callee, captures) = emit(functionCallee: ast[e].callee)
    let arguments = captures + explicitArguments

    // Call is evaluated last.
    switch callee {
    case .direct(let r):
      emitApply(.constant(r), to: arguments, writingResultTo: storage, at: ast[e].site)
    case .lambda(let r):
      emitApply(r, to: arguments, writingResultTo: storage, at: ast[e].site)
    case .bundle(let r):
      emitApply(r, to: arguments, writingResultTo: storage, at: ast[e].site)
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: IntegerLiteralExpr.ID, to storage: Operand) {
    emitStore(numericLiteral: e, to: storage)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: LambdaExpr.ID, to storage: Operand) {
    let callee = lower(function: ast[e].decl)
    let r = FunctionReference(
      to: callee, in: module,
      specializedBy: module.specialization(in: insertionFunction!), in: insertionScope!)

    let site = ast[e].site
    let x0 = insert(module.makeAddressToPointer(.constant(r), at: site))!
    let x1 = emitSubfieldView(storage, at: [0], at: site)
    emitInitialize(storage: x1, to: x0, at: ast[e].site)

    let lambda = LambdaType(program.canonical(program[e].type, in: insertionScope!))!
    if lambda.environment == .void { return }

    var i = 1
    for b in program[e].decl.explicitCaptures {
      // TODO: See #878
      guard program[b].pattern.subpattern.kind == NamePattern.self else { UNIMPLEMENTED() }
      let y0 = emitSubfieldView(storage, at: [i], at: site)
      emitStore(value: program[b].initializer!, to: y0)
      i += 1
    }

    for c in program[e].decl.implicitCaptures {
      let y0 = emitLValue(directReferenceTo: c.decl, at: site)
      let y1 = insert(module.makeAccess(c.type.access, from: y0, at: site))!
      let y2 = emitSubfieldView(storage, at: [i], at: site)
      emitStore(access: y1, to: y2, at: site)
      i += 1
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: NameExpr.ID, to storage: Operand) {
    let x0 = emitLValue(e)
    emitMove([.inout, .set], x0, to: storage, at: ast[e].site)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  ///
  /// - Parameters:
  ///   - site: The source range in which `e` is being evaluated. Defaults to `e.site`.
  private mutating func emitStore(
    _ e: PragmaLiteralExpr.ID, to storage: Operand,
    at site: SourceRange? = nil
  ) {
    let anchor = site ?? ast[e].site
    switch ast[e].kind {
    case .file:
      emitStore(string: anchor.file.url.absoluteURL.fileSystemPath, to: storage, at: anchor)
    case .line:
      emitStore(int: anchor.first().line.number, to: storage, at: anchor)
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: RemoteExpr.ID, to storage: Operand) {
    let t = RemoteType(program[e].type)!
    let s = program[e].site

    let x0 = emitLValue(program[e].operand)
    let x1 = insert(module.makeAccess(t.access, from: x0, at: s))!
    emitStore(access: x1, to: storage, at: s)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: SequenceExpr.ID, to storage: Operand) {
    emitStore(program[e].folded, to: storage)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: FoldedSequenceExpr, to storage: Operand) {
    switch e {
    case .infix(let callee, let lhs, let rhs):
      let t = program[callee.expr].type
      let calleeType = LambdaType(canonical(t))!.lifted

      // Emit the operands, starting with RHS.
      let r = emit(infixOperand: rhs, passed: ParameterType(calleeType.inputs[1].type)!.access)
      let l = emit(infixOperand: lhs, passed: ParameterType(calleeType.inputs[0].type)!.access)

      // The callee must be a reference to member function.
      guard case .member(let d, let a, _) = program[callee.expr].referredDecl else {
        unreachable()
      }

      let o = FunctionReference(to: d, in: &module, specializedBy: a, in: insertionScope!)
      let f = Operand.constant(o)

      // Emit the call.
      let site = ast.site(of: e)
      let result = insert(module.makeAccess(.set, from: storage, at: site))!
      insert(module.makeCall(applying: f, to: [l, r], writingResultTo: result, at: site))

    case .leaf(let v):
      emitStore(value: v, to: storage)
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: SubscriptCallExpr.ID, to storage: Operand) {
    let (callee, arguments) = emitOperands(e)
    guard ast.implementation(.sink, of: callee.bundle) != nil else {
      let n = ast.name(of: callee.bundle)
      report(.error(n, hasNoSinkImplementationAt: program[e].callee.site))
      return
    }

    _ = arguments
    UNIMPLEMENTED("sink subscript")
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: StringLiteralExpr.ID, to storage: Operand) {
    emitStore(string: ast[e].value, to: storage, at: ast[e].site)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: TupleExpr.ID, to storage: Operand) {
    if ast[e].elements.isEmpty {
      insert(module.makeMarkState(storage, initialized: true, at: ast[e].site))
      return
    }

    for (i, element) in ast[e].elements.enumerated() {
      let xi = emitSubfieldView(storage, at: [i], at: ast[element.value].site)
      emitStore(value: element.value, to: xi)
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: TupleMemberExpr.ID, to storage: Operand) {
    let x0 = emitLValue(e)
    emitMove([.inout, .set], x0, to: storage, at: ast[e].site)
  }

  /// Inserts the IR to store the value of `e` to `storage`, converting it to the type of `storage`
  /// if necessary.
  ///
  /// The type comparison is performed in the scope of `e`.
  private mutating func emitStore<T: ExprID>(
    convertingIfNecessary e: T,
    to storage: Operand
  ) {
    let lhsType = module.type(of: storage).ast
    let rhsType = canonical(program[e].type)

    if program.areEquivalent(lhsType, rhsType, in: program[e].scope) {
      emitStore(value: e, to: storage)
    } else if lhsType.base is UnionType {
      let x0 = insert(
        module.makeOpenUnion(
          storage, as: rhsType, forInitialization: true, at: ast[e].site))!
      emitStore(value: e, to: x0)
      insert(module.makeCloseUnion(x0, at: ast[e].site))
    } else {
      UNIMPLEMENTED()
    }
  }

  /// Writes the value of `literal` to `storage`.
  private mutating func emitStore<T: NumericLiteralExpr>(
    numericLiteral literal: T.ID, to storage: Operand
  ) {
    let literalType = canonical(program[literal].type)

    switch literalType {
    case ast.coreType("Int")!:
      emitStore(integer: literal, signed: true, bitWidth: 64, to: storage)
    case ast.coreType("Int32")!:
      emitStore(integer: literal, signed: true, bitWidth: 32, to: storage)
    case ast.coreType("Int8")!:
      emitStore(integer: literal, signed: true, bitWidth: 8, to: storage)
    case ast.coreType("UInt")!:
      emitStore(integer: literal, signed: false, bitWidth: 64, to: storage)
    case ast.coreType("UInt8")!:
      emitStore(integer: literal, signed: false, bitWidth: 8, to: storage)
    case ast.coreType("Float64")!:
      emitStore(floatingPoint: literal, to: storage, evaluatedBy: FloatingPointConstant.float64(_:))
    case ast.coreType("Float32")!:
      emitStore(floatingPoint: literal, to: storage, evaluatedBy: FloatingPointConstant.float32(_:))
    default:
      UNIMPLEMENTED()
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
    let x1 = insert(module.makeAccess(.set, from: x0, at: syntax.site))!
    let x2 = Operand.constant(evaluate(syntax.value))
    insert(module.makeStore(x2, at: x1, at: syntax.site))
  }

  /// Writes the value of `literal` to `storage`, knowing it is a core integer instance with given
  /// sign and width.
  private mutating func emitStore<T: NumericLiteralExpr>(
    integer literal: T.ID, signed: Bool, bitWidth: Int, to storage: Operand
  ) {
    let syntax = ast[literal]
    guard let bits = WideUInt(hyloLiteral: syntax.value, signed: signed, bitWidth: bitWidth) else {
      diagnostics.insert(
        .error(
          integerLiteral: syntax.value,
          overflowsWhenStoredInto: program[literal].type,
          at: syntax.site))
      return
    }

    let x0 = emitSubfieldView(storage, at: [0], at: syntax.site)
    let x1 = insert(module.makeAccess(.set, from: x0, at: syntax.site))!
    let x2 = Operand.constant(IntegerConstant(bits))
    insert(module.makeStore(x2, at: x1, at: syntax.site))
  }

  /// Writes an instance of `Hylo.Int` with value `v` to `storage`.
  ///
  /// - Requires: `storage` is the address of uninitialized memory of type `Hylo.Int`.
  mutating func emitStore(int v: Int, to storage: Operand, at site: SourceRange) {
    let x0 = emitSubfieldView(storage, at: [0], at: site)
    let x1 = insert(module.makeAccess(.set, from: x0, at: site))!
    insert(module.makeStore(.word(v), at: x1, at: site))
    insert(module.makeEndAccess(x1, at: site))
  }

  /// Writes an instance of `Hylo.String` with value `v` to `storage`.
  ///
  /// - Requires: `storage` is the address of uninitialized memory of type `Hylo.String`.
  private mutating func emitStore(string v: String, to storage: Operand, at site: SourceRange) {
    let bytes = v.unescaped.data(using: .utf8)!

    let x0 = emitSubfieldView(storage, at: [0], at: site)
    emitStore(int: bytes.count, to: x0, at: site)

    let x1 = emitSubfieldView(storage, at: [1, 0], at: site)
    let x2 = insert(module.makeConstantString(utf8: bytes, at: site))!
    emitInitialize(storage: x1, to: x2, at: site)
  }

  /// Inserts the IR for storing `a`, which is an `access`, to `storage`.
  ///
  /// - Parameter storage: an address derived from an `alloc_stack` that is outlived by the
  ///   provenances of `a`.
  private mutating func emitStore(
    access a: Operand, to storage: Operand, at site: SourceRange
  ) {
    guard let s = module.provenances(storage).uniqueElement, module[s] is AllocStack else {
      report(.error(cannotCaptureAccessAt: site))
      return
    }

    let x0 = insert(module.makeAccess(.set, from: storage, at: site))!
    insert(module.makeCapture(a, in: x0, at: site))
    insert(module.makeEndAccess(x0, at: site))
    frames.top.setMayHoldCaptures(s)
  }

  /// Inserts the IR for calling `callee` on `arguments`, storing the result to `storage`.
  private mutating func emitApply(
    _ callee: Operand, to arguments: [Operand],
    writingResultTo storage: Operand, at site: SourceRange
  ) {
    let o = insert(module.makeAccess(.set, from: storage, at: site))!
    insert(module.makeCall(applying: callee, to: arguments, writingResultTo: o, at: site))
    insert(module.makeEndAccess(o, at: site))
  }

  /// Inserts the IR for calling `callee` on `arguments`, storing the result to `storage`.
  private mutating func emitApply(
    _ callee: BundleReference<MethodDecl>, to arguments: [Operand],
    writingResultTo storage: Operand, at site: SourceRange
  ) {
    let o = insert(module.makeAccess(.set, from: storage, at: site))!
    insert(
      module.makeCallBundle(
        applying: .init(callee, in: insertionScope!), to: arguments, writingResultTo: o, at: site))
    insert(module.makeEndAccess(o, at: site))
  }

  /// Inserts the IR for given constructor `call`, which initializes storage `r` by applying
  /// initializer `d` parameterized by `a`.
  ///
  /// - Parameters:
  ///   - call: The syntax of the call.
  ///   - s: The address of uninitialized storage typed by the receiver of `call`. This storage is
  ///     borrowed for initialization after evaluating `call`'s arguments and before the call.
  private mutating func emit(constructorCall call: FunctionCallExpr.ID, initializing s: Operand) {
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
    let arguments = emitArguments(
      to: ast[call].callee, in: CallID(call),
      usingExplicit: ast[call].arguments, synthesizingDefaultAt: .empty(atEndOf: ast[call].site))

    // Receiver is captured next.
    let receiver = insert(module.makeAccess(.set, from: s, at: ast[call].site))!

    // Call is evaluated last.
    let f = Operand.constant(
      FunctionReference(to: AnyDeclID(d), in: &module, specializedBy: a, in: insertionScope!))
    let x0 = emitAllocStack(for: .void, at: ast[call].site)
    let x1 = insert(module.makeAccess(.set, from: x0, at: ast[call].site))!

    let s = module.makeCall(
      applying: f, to: [receiver] + arguments, writingResultTo: x1, at: ast[call].site)
    insert(s)
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
    let callee = LambdaType(canonical(program[ast[call].callee].type))!

    if callee.inputs.isEmpty {
      insert(module.makeMarkState(receiver, initialized: true, at: ast[call].site))
      return
    }

    for i in callee.inputs.indices {
      // TODO: Handle remote types
      let p = ParameterType(callee.inputs[i].type)!
      if p.bareType.base is RemoteType {
        UNIMPLEMENTED()
      }

      let s = emitSubfieldView(receiver, at: [i], at: ast[call].site)
      emitStore(value: ast[call].arguments[i].value, to: s)
    }
  }

  /// Inserts the IR preparing the run-time arguments passed to `callee` in `call`, lowering
  /// `arguments` and synthesizing default values at `syntheticSite`.
  ///
  /// Information about argument resolution is read from `program.callOperands`. Arguments passed
  /// explicitly have a corresponding expression in `arguments`. If default arguments are used,
  /// `callee` is a name expression referring to a callable declaration.
  private mutating func emitArguments(
    to callee: AnyExprID, in call: CallID,
    usingExplicit arguments: [LabeledArgument],
    synthesizingDefaultAt syntheticSite: SourceRange
  ) -> [Operand] {
    let parameters = (canonical(program[callee].type).base as! CallableType).inputs
    let inputs = program.callOperands[call]!
    assert(parameters.count == inputs.count)

    // Nothing to do if the callee has no parameter.
    if parameters.isEmpty { return [] }

    // Parameter declarations are accessible iff `callee` is a direct reference to a callable.
    let parameterDecls = NameExpr.ID(callee).flatMap { (n) in
      program.ast.runtimeParameters(of: program[n].referredDecl.decl!)
    }

    var result: [Operand] = []
    for i in inputs.indices {
      let p = ParameterType(parameters[i].type)!

      switch inputs[i] {
      case .explicit(let n):
        let a = arguments[n].value
        result.append(emit(argument: a, to: p, at: syntheticSite))

      case .defaulted:
        let a = program[parameterDecls![i]].defaultValue!
        result.append(emit(argument: a, to: p, at: syntheticSite))

      case .implicit(let d):
        let s = emitLValue(directReferenceTo: d, at: syntheticSite)
        result.append(insert(module.makeAccess(p.access, from: s, at: syntheticSite))!)
      }
    }

    return result
  }

  /// Inserts the IR generating the operands of the subscript call `e`.
  private mutating func emitOperands(
    _ e: SubscriptCallExpr.ID
  ) -> (callee: BundleReference<SubscriptDecl>, arguments: [Operand]) {
    // Explicit arguments are evaluated first, from left to right.
    let explicitArguments = emitArguments(
      to: ast[e].callee, in: CallID(e),
      usingExplicit: ast[e].arguments, synthesizingDefaultAt: .empty(atEndOf: ast[e].site))

    // Callee and captures are evaluated next.
    let (callee, captures) = emit(subscriptCallee: ast[e].callee)

    return (callee, captures + explicitArguments)
  }

  /// Inserts the IR for the argument `e` passed to a parameter of type `parameter`.
  ///
  /// - Parameters:
  ///   - site: The source range in which `e` is being evaluated if it's a pragma literals.
  ///     Defaults to `e.site`.
  private mutating func emit(
    argument e: AnyExprID, to parameter: ParameterType, at site: SourceRange? = nil
  ) -> Operand {

    if parameter.isAutoclosure {
      return emit(autoclosureFor: e, to: parameter, at: site)
    }

    let argumentSite: SourceRange
    let storage: Operand

    if let a = PragmaLiteralExpr.ID(e) {
      argumentSite = site ?? ast[a].site
      storage = insert(module.makeAllocStack(program[a].type, at: argumentSite))!
      emitStore(a, to: storage, at: argumentSite)
    } else {
      argumentSite = ast[e].site
      storage = emitLValue(e)
    }

    let s = emitCoerce(storage, to: parameter.bareType, at: argumentSite)
    return insert(module.makeAccess(parameter.access, from: s, at: argumentSite))!
  }

  private mutating func emit(
    autoclosureFor argument: AnyExprID, to parameter: ParameterType, at site: SourceRange? = nil
  ) -> Operand {
    // Emit synthesized function declaration.
    let f = SynthesizedFunctionDecl(
      .autoclosure(argument), typed: parameter.bareType.base as! LambdaType,
      in: program[argument].scope)
    let callee = withClearContext({ $0.lower(syntheticAutoclosure: f) })

    // Emit the IR code to reference tha function declaration.
    let r = FunctionReference(
      to: callee, in: module,
      specializedBy: module.specialization(in: insertionFunction!), in: insertionScope!)

    let site = ast[argument].site
    let s1 = insert(module.makeAddressToPointer(.constant(r), at: site))!
    let s2 = emitAllocStack(for: parameter.bareType, at: site)
    emitInitialize(storage: s2, to: s1, at: site)
    return insert(module.makeAccess(parameter.access, from: s2, at: site))!
  }

  /// Inserts the IR for infix operand `e` passed with convention `access`.
  private mutating func emit(
    infixOperand e: FoldedSequenceExpr, passed access: AccessEffect
  ) -> Operand {
    let storage: Operand

    switch e {
    case .infix(let callee, _, _):
      let t = LambdaType(canonical(program[callee.expr].type))!.lifted
      storage = emitAllocStack(for: t.output, at: ast.site(of: e))
      emitStore(e, to: storage)

    case .leaf(let e):
      storage = emitLValue(e)
    }

    return insert(module.makeAccess(access, from: storage, at: ast.site(of: e)))!
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
        let x1 = insert(module.makeAccess(.sink, from: x0, at: site))!
        let x2 = insert(module.makeLoad(x1, at: site))!
        a.append(x2)
        insert(module.makeEndAccess(x1, at: site))
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
  ) -> (callee: Callee, captures: [Operand]) {
    switch callee.kind {
    case NameExpr.self:
      return emit(namedFunctionCallee: .init(callee)!)

    case InoutExpr.self:
      // TODO: Handle the mutation marker, somehow.
      return emit(functionCallee: ast[InoutExpr.ID(callee)!].subject)

    default:
      let f = emit(lambdaCallee: callee)
      return (.lambda(f), [])
    }
  }

  /// Inserts the IR for given `callee` and returns `(c, a)`, where `c` is the callee's value and
  /// `a` are arguments to lifted parameters.
  private mutating func emit(
    namedFunctionCallee callee: NameExpr.ID
  ) -> (callee: Callee, captures: [Operand]) {
    switch program[callee].referredDecl {
    case .direct(let d, let a) where d.isCallable:
      // Callee is a direct reference to an arrow declaration.
      guard LambdaType(canonical(program[callee].type))!.environment == .void else {
        UNIMPLEMENTED("Generate IR for calls to local functions with captures #1088")
      }
      let f = FunctionReference(to: d, in: &module, specializedBy: a, in: insertionScope!)
      return (.direct(f), [])

    case .member(let d, _, _) where d.isCallable:
      return emitMemberFunctionCallee(callee)

    case .builtinFunction, .builtinType:
      // Calls to built-ins should have been handled already.
      unreachable()

    default:
      // Callee is a lambda.
      let f = emit(lambdaCallee: .init(callee))
      return (.lambda(f), [])
    }
  }

  /// Inserts the IR evaluating `callee`, which refers to a member function, returning `(c, [r])`
  /// where `c` is the callee's value and `r` is the receiver of the call.
  private mutating func emitMemberFunctionCallee(
    _ callee: NameExpr.ID
  ) -> (callee: Callee, captures: [Operand]) {
    guard case .member(let d, let a, let s) = program[callee].referredDecl else {
      unreachable()
    }

    let receiver = emitLValue(receiver: s, at: ast[callee].site)
    let receiverType = module.type(of: receiver).ast
    let scopeOfUse = program[callee].scope

    let functionToCall: Callee

    // Check if `d`'s implementation is synthethic.
    if program.isRequirement(d) && !program.isSkolem(receiverType) {
      let t = program.traitDeclaring(d)!
      let c = program.conformance(of: receiverType, to: t, exposedTo: scopeOfUse)!
      let irFunction = module.demandDeclaration(lowering: c.implementations[d]!)
      functionToCall = .direct(
        FunctionReference(to: irFunction, in: module, specializedBy: a, in: scopeOfUse))
    } else {
      functionToCall = Callee(d, specializedBy: a, in: &module, usedIn: insertionScope!)
    }

    let k = receiverCapabilities(program[callee].type)
    let c = insert(module.makeAccess(k, from: receiver, at: ast[callee].site))!
    return (functionToCall, [c])
  }

  /// Inserts the IR for given `callee` and returns its value.
  ///
  /// - Requires: `callee` has a lambda type.
  private mutating func emit(lambdaCallee callee: AnyExprID) -> Operand {
    switch LambdaType(program[callee].type)!.receiverEffect {
    case .yielded:
      unreachable()
    case .set, .sink:
      UNIMPLEMENTED()

    case let k:
      let l = emitLValue(callee)
      let b = module.makeAccess(k, from: l, at: ast[callee].site)
      return insert(b)!
    }
  }

  private mutating func emit(
    subscriptCallee callee: AnyExprID
  ) -> (callee: BundleReference<SubscriptDecl>, captures: [Operand]) {
    // TODO: Handle captures
    switch callee.kind {
    case NameExpr.self:
      return emit(namedSubscriptCallee: .init(callee)!)

    case InoutExpr.self:
      // TODO: Handle the mutation marker, somehow.
      return emit(subscriptCallee: ast[InoutExpr.ID(callee)!].subject)

    default:
      UNIMPLEMENTED()
    }
  }

  private mutating func emit(
    namedSubscriptCallee callee: NameExpr.ID
  ) -> (callee: BundleReference<SubscriptDecl>, captures: [Operand]) {
    switch program[callee].referredDecl {
    case .direct(let d, let a) where d.kind == SubscriptDecl.self:
      // Callee is a direct reference to a subscript declaration.
      let t = SubscriptType(canonical(program[d].type))!
      guard t.environment == .void else {
        UNIMPLEMENTED()
      }

      let b = BundleReference(to: SubscriptDecl.ID(d)!, specializedBy: a)
      return (b, [])

    case .member(let d, let a, let s) where d.kind == SubscriptDecl.self:
      // Callee is a member reference to a subscript declaration.
      let b = BundleReference(to: SubscriptDecl.ID(d)!, specializedBy: a)

      // The callee's receiver is the sole capture.
      let receiver = emitLValue(receiver: s, at: ast[callee].site)
      let t = SubscriptType(program[d].type)!
      let i = insert(module.makeAccess(t.capabilities, from: receiver, at: ast[callee].site))!
      return (b, [i])

    case .builtinFunction, .builtinType:
      // There are no built-in subscripts.
      unreachable()

    default:
      UNIMPLEMENTED()
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
        let patternType = canonical(program[d].type)
        let next = emitConditionalNarrowing(
          subject, as: ast[d].pattern, typed: patternType, to: allocs[i],
          else: failure, in: scope)
        insertionPoint = .end(of: next)
      }
    }

    return (success: insertionBlock!, failure: failure)
  }

  /// Returns a basic block in which the names in `pattern` have been declared and initialized.
  ///
  /// This method emits IR to:
  ///
  /// - check whether the value in `subject` is an instance of `patternType`;
  /// - if it isn't, jump to `failure`;
  /// - if it is, jump to a new basic block *b*, coerce the contents of `subject` into `storage`,
  ///   applying consuming coercions as necessary, and define the bindings declared in `pattern`.
  ///
  /// If `subject` always matches `patternType`, the narrowing is irrefutable and `failure` is
  /// unreachable in the generated IR.
  ///
  /// The return value is the new basic block *b*, which is defined in `scope`. The emitter context
  /// is updated to associate the bindings declared in `pattern` to their address in `storage`.
  private mutating func emitConditionalNarrowing(
    _ subject: Operand,
    as pattern: BindingPattern.ID, typed patternType: AnyType,
    to storage: Operand,
    else failure: Block.ID, in scope: AnyScopeID
  ) -> Block.ID {
    switch module.type(of: subject).ast.base {
    case let t as UnionType:
      return emitConditionalNarrowing(
        subject, typed: t, as: pattern, typed: patternType, to: storage,
        else: failure, in: scope)
    default:
      break
    }

    UNIMPLEMENTED()
  }

  /// Returns a basic block in which the names in `pattern` have been declared and initialized.
  ///
  /// This method method implements conditional narrowing for union types.
  private mutating func emitConditionalNarrowing(
    _ subject: Operand, typed subjectType: UnionType,
    as pattern: BindingPattern.ID, typed patternType: AnyType,
    to storage: Operand,
    else failure: Block.ID, in scope: AnyScopeID
  ) -> Block.ID {
    // TODO: Implement narrowing to an arbitrary subtype.
    guard subjectType.elements.contains(patternType) else { UNIMPLEMENTED() }
    let site = ast[pattern].site

    let i = program.discriminatorToElement(in: subjectType).firstIndex(of: patternType)!
    let expected = IntegerConstant(i, bitWidth: 64)  // FIXME: should be width of 'word'
    let actual = emitUnionDiscriminator(subject, at: site)

    let test = insert(
      module.makeLLVM(applying: .icmp(.eq, .word), to: [.constant(expected), actual], at: site))!
    let next = appendBlock(in: scope)
    insert(module.makeCondBranch(if: test, then: next, else: failure, at: site))

    insertionPoint = .end(of: next)
    let x0 = insert(module.makeOpenUnion(subject, as: patternType, at: site))!
    pushing(Frame()) { (this) in
      this.emitMove([.set], x0, to: storage, at: site)
    }
    insert(module.makeCloseUnion(x0, at: site))

    emitLocalDeclarations(introducedBy: pattern, referringTo: [], relativeTo: storage)

    return next
  }

  /// Inserts the IR for branch condition `e`.
  ///
  /// - Requires: `e.type` is `Hylo.Bool`
  private mutating func emit(branchCondition e: AnyExprID) -> Operand {
    precondition(canonical(program[e].type) == ast.coreType("Bool")!)
    let wrapper = emitLValue(e)
    return emitLoadBuiltinBool(wrapper, at: ast[e].site)
  }

  /// Inserts the IR for extracting the built-in value stored in an instance of `Hylo.Bool`.
  private mutating func emitLoadBuiltinBool(_ wrapper: Operand, at site: SourceRange) -> Operand {
    precondition(module.type(of: wrapper) == .address(ast.coreType("Bool")!))
    let x0 = emitSubfieldView(wrapper, at: [0], at: site)
    let x1 = insert(module.makeAccess(.sink, from: x0, at: site))!
    let x2 = insert(module.makeLoad(x1, at: site))!
    insert(module.makeEndAccess(x1, at: site))
    return x2
  }

  /// Inserts the IR for coercing `source` to an address of type `target`.
  ///
  /// `source` is returned unchanged if it stores an instance of `target`. Otherwise, the IR
  /// producing an address of type `target` is inserted, consuming `source` if necessary.
  private mutating func emitCoerce(
    _ source: Operand, to target: AnyType, at site: SourceRange
  ) -> Operand {
    let lhs = module.type(of: source).ast
    let rhs = program.canonical(target, in: insertionScope!)

    if program.areEquivalent(lhs, rhs, in: insertionScope!) {
      return source
    }

    if lhs.base is RemoteType {
      let s = insert(module.makeOpenCapture(source, at: site))!
      return emitCoerce(s, to: rhs, at: site)
    }

    switch rhs.base {
    case let t as ExistentialType:
      return _emitCoerce(source, to: t, at: site)
    case let t as LambdaType:
      return _emitCoerce(source, to: t, at: site)
    case let t as UnionType:
      return _emitCoerce(source, to: t, at: site)
    default:
      unexpectedCoercion(from: lhs, to: rhs)
    }
  }

  /// Inserts the IR for coercing `source` to an address of type `target`.
  ///
  /// - Requires: `target` is canonical.
  private mutating func _emitCoerce(
    _ source: Operand, to target: ExistentialType, at site: SourceRange
  ) -> Operand {
    let t = module.type(of: source).ast
    if t.base is ExistentialType {
      return source
    }

    return emitExistential(target, wrapping: source, at: site)
  }

  /// Inserts the IR for coercing `source` to an address of type `target`.
  ///
  /// - Requires: `target` is canonical.
  private mutating func _emitCoerce(
    _ source: Operand, to target: LambdaType, at site: SourceRange
  ) -> Operand {
    let t = module.type(of: source).ast
    guard let lhs = LambdaType(t) else {
      unexpectedCoercion(from: t, to: ^target)
    }

    // TODO: Handle variance
    let rhs = LambdaType(
      receiverEffect: lhs.receiverEffect,
      environment: target.environment,
      inputs: target.inputs,
      output: target.output)

    if !program.areEquivalent(^lhs, ^rhs, in: insertionScope!) {
      unexpectedCoercion(from: t, to: ^target)
    }

    // If we're here, then `t` and `u` only differ on their effects.
    return source
  }

  /// Inserts the IR for coercing `source` to an address of type `target`.
  ///
  /// - Requires: `target` is canonical.
  private mutating func _emitCoerce(
    _ source: Operand, to target: UnionType, at site: SourceRange
  ) -> Operand {
    let lhs = module.type(of: source).ast

    let x0 = emitAllocStack(for: ^target, at: site)
    let x1 = insert(module.makeOpenUnion(x0, as: lhs, forInitialization: true, at: site))!
    emitMove([.set], source, to: x1, at: site)
    insert(module.makeCloseUnion(x1, at: site))
    return x0
  }

  /// Traps on this execution path becauses of un unexpected coercion from `lhs` to `rhs`.
  private func unexpectedCoercion(
    from lhs: AnyType, to rhs: AnyType, file: StaticString = #file, line: UInt = #line
  ) -> Never {
    fatalError("unexpected coercion from '\(lhs)' to \(rhs)", file: file, line: line)
  }

  /// Inserts the IR for converting `foreign` to a value of type `ir`.
  private mutating func emitConvert(
    foreign: Operand, to ir: AnyType, at site: SourceRange
  ) -> Operand {
    precondition(module.type(of: foreign).isObject)

    let foreignConvertible = ast.core.foreignConvertible.type
    let foreignConvertibleConformance = program.conformance(
      of: ir, to: foreignConvertible, exposedTo: insertionScope!)!
    let r = ast.requirements(
      Name(stem: "init", labels: ["foreign_value"]), in: foreignConvertible.decl)[0]

    // TODO: Handle cases where the foreign representation of `t` is not built-in.

    // Store the foreign representation in memory to call the converter.
    let source = emitAllocStack(for: module.type(of: foreign).ast, at: site)
    emitInitialize(storage: source, to: foreign, at: site)

    switch foreignConvertibleConformance.implementations[r]! {
    case .concrete(let m):
      let convert = module.demandDeclaration(lowering: m)!
      let f = module.reference(to: convert, implementedFor: foreignConvertibleConformance)

      let x0 = emitAllocStack(for: ir, at: site)
      let x1 = insert(module.makeAccess(.set, from: x0, at: site))!
      let x2 = emitAllocStack(for: LambdaType(f.type.ast)!.output, at: site)
      let x3 = insert(module.makeAccess(.set, from: x2, at: site))!
      let x4 = insert(module.makeAccess(.sink, from: source, at: site))!

      let s = module.makeCall(applying: .constant(f), to: [x1, x4], writingResultTo: x3, at: site)
      insert(s)

      insert(module.makeEndAccess(x4, at: site))
      insert(module.makeEndAccess(x3, at: site))
      insert(module.makeEndAccess(x1, at: site))
      return x0

    case .synthetic:
      UNIMPLEMENTED()
    }
  }

  /// Appends the IR to convert `o` to a FFI argument.
  ///
  /// The returned operand is the result of a `load` instruction.
  private mutating func emitConvertToForeign(_ o: Operand, at site: SourceRange) -> Operand {
    let t = module.type(of: o)
    precondition(t.isAddress)

    let foreignConvertible = ast.core.foreignConvertible.type
    let foreignConvertibleConformance = program.conformance(
      of: t.ast, to: foreignConvertible, exposedTo: insertionScope!)!
    let r = ast.requirements("foreign_value", in: foreignConvertible.decl)[0]

    // TODO: Handle cases where the foreign representation of `t` is not built-in.

    switch foreignConvertibleConformance.implementations[r]! {
    case .concrete(let m):
      let convert = module.demandDeclaration(lowering: m)!
      let f = module.reference(to: convert, implementedFor: foreignConvertibleConformance)

      let x0 = insert(module.makeAccess(.let, from: o, at: site))!
      let x1 = emitAllocStack(for: LambdaType(f.type.ast)!.output, at: site)
      let x2 = insert(module.makeAccess(.set, from: x1, at: site))!
      insert(module.makeCall(applying: .constant(f), to: [x0], writingResultTo: x2, at: site))
      insert(module.makeEndAccess(x2, at: site))
      insert(module.makeEndAccess(x0, at: site))

      let x3 = insert(module.makeAccess(.sink, from: x1, at: site))!
      let x4 = insert(module.makeLoad(x3, at: site))!
      insert(module.makeEndAccess(x3, at: site))
      return x4

    case .synthetic:
      UNIMPLEMENTED()
    }
  }

  /// Returns an existential container of type `t` wrappring `witness`.
  private mutating func emitExistential(
    _ t: ExistentialType, wrapping witness: Operand, at site: SourceRange
  ) -> Operand {
    let w = module.type(of: witness).ast
    let table = Operand.constant(module.demandWitnessTable(w, in: insertionScope!))
    return insert(module.makeWrapExistentialAddr(witness, table, as: t, at: site))!
  }

  // MARK: l-values

  /// Inserts the IR for lvalue `e`.
  private mutating func emitLValue(_ e: AnyExprID) -> Operand {
    switch e.kind {
    case CastExpr.self:
      return emitLValue(CastExpr.ID(e)!)
    case InoutExpr.self:
      return emitLValue(InoutExpr.ID(e)!)
    case NameExpr.self:
      return emitLValue(NameExpr.ID(e)!)
    case SubscriptCallExpr.self:
      return emitLValue(SubscriptCallExpr.ID(e)!)
    case TupleMemberExpr.self:
      return emitLValue(TupleMemberExpr.ID(e)!)
    default:
      return emitStore(value: e)
    }
  }

  /// Inserts the IR for lvalue `e`.
  private mutating func emitLValue(_ e: CastExpr.ID) -> Operand {
    switch ast[e].direction {
    case .up:
      return emitLValue(upcast: e)
    case .pointerConversion:
      return emitLValue(pointerConversion: e)
    default:
      UNIMPLEMENTED("lvalue lowering for cast expressions #1049")
    }
  }

  /// Inserts the IR for lvalue `e`.
  private mutating func emitLValue(upcast e: CastExpr.ID) -> Operand {
    switch ast[e].left.kind {
    case FloatLiteralExpr.self:
      return emitStore(value: ast[e].left)
    case IntegerLiteralExpr.self:
      return emitStore(value: ast[e].left)
    default:
      UNIMPLEMENTED("lvalue lowering for cast expressions #1049")
    }
  }

  /// Inserts the IR for lvalue `e`.
  private mutating func emitLValue(pointerConversion e: CastExpr.ID) -> Operand {
    let x0 = emitLValue(ast[e].left)
    let x1 = insert(module.makeAccess(.sink, from: x0, at: ast[e].site))!
    let x2 = insert(module.makeLoad(x1, at: ast[e].site))!
    insert(module.makeEndAccess(x1, at: ast[e].site))

    let target = RemoteType(canonical(program[e].type))!
    return insert(module.makePointerToAddress(x2, to: target, at: ast[e].site))!
  }

  /// Inserts the IR for lvalue `e`.
  private mutating func emitLValue(_ e: InoutExpr.ID) -> Operand {
    emitLValue(ast[e].subject)
  }

  /// Inserts the IR for lvalue `e`.
  private mutating func emitLValue(_ e: NameExpr.ID) -> Operand {
    emitLValue(reference: program[e].referredDecl, at: ast[e].site)
  }

  /// Inserts the IR for lvalue `e`.
  private mutating func emitLValue(_ e: SubscriptCallExpr.ID) -> Operand {
    let (callee, arguments) = emitOperands(e)
    let s = module.makeProjectBundle(
      applying: .init(callee, in: insertionScope!), to: arguments, at: ast[e].site)
    return insert(s)!
  }

  /// Inserts the IR for lvalue `e`.
  private mutating func emitLValue(_ e: TupleMemberExpr.ID) -> Operand {
    let base = emitLValue(ast[e].tuple)
    return emitSubfieldView(base, at: [ast[e].index.value], at: ast[e].index.site)
  }

  /// Inserts the IR for `r` used a lvalue at `site`.
  private mutating func emitLValue(reference r: DeclReference, at site: SourceRange) -> Operand {
    switch r {
    case .direct(let d, _):
      return emitLValue(directReferenceTo: d, at: site)

    case .member(let d, let a, let s):
      let receiver = emitLValue(receiver: s, at: site)
      return emitProperty(boundTo: receiver, declaredBy: d, specializedBy: a, at: site)

    case .constructor:
      UNIMPLEMENTED()

    case .builtinModule, .builtinFunction, .builtinType, .compilerKnownType:
      // Built-in symbols and compiler-known types are never used as l-value.
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

    // Handle references to type declarations.
    if let t = MetatypeType(program[d].type) {
      let s = emitAllocStack(for: ^t, at: site)
      emitInitialize(storage: s, to: .constant(t), at: site)
      return s
    }

    assert(program.isGlobal(d), "unhandled local declaration")

    switch d.kind {
    case GenericParameterDecl.self:
      return insert(module.makeGenericParameter(passedTo: .init(d)!, at: site))!

    case VarDecl.self:
      let (root, subfied) = program.subfieldRelativeToRoot(of: .init(d)!)
      let s = insert(module.makeGlobalAddr(of: root, at: site))!
      return emitSubfieldView(s, at: subfied, at: site)

    default:
      unexpected(d, in: program.ast)
    }
  }

  /// Inserts IR to return the address of the member declared by `d`, bound to `receiver`, and
  /// specialized by `specialization`.
  private mutating func emitProperty(
    boundTo receiver: Operand, declaredBy d: AnyDeclID,
    specializedBy specialization: GenericArguments,
    at site: SourceRange
  ) -> Operand {
    switch d.kind {
    case SubscriptDecl.self:
      return emitComputedProperty(
        boundTo: receiver, declaredByBundle: .init(d)!, specializedBy: specialization, at: site)

    case VarDecl.self:
      let l = AbstractTypeLayout(of: module.type(of: receiver).ast, definedIn: program)
      let i = l.offset(of: ast[VarDecl.ID(d)!].baseName)!
      return emitSubfieldView(receiver, at: [i], at: site)

    default:
      UNIMPLEMENTED()
    }
  }

  /// Returns the projection the property declared by `d`, bound to `receiver`, and specialized by
  /// `specialization`.
  private mutating func emitComputedProperty(
    boundTo receiver: Operand, declaredByBundle d: SubscriptDecl.ID,
    specializedBy specialization: GenericArguments,
    at site: SourceRange
  ) -> Operand {
    if let i = ast[d].impls.uniqueElement {
      return emitComputedProperty(
        boundTo: receiver, declaredBy: i, specializedBy: specialization, at: site)
    }

    let callee = BundleReference(to: d, specializedBy: specialization)
    let t = SubscriptType(canonicalType(of: d, specializedBy: specialization))!
    let r = insert(module.makeAccess(t.capabilities, from: receiver, at: site))!

    let s = module.makeProjectBundle(
      applying: .init(callee, in: insertionScope!), to: [r], at: site)
    return insert(s)!
  }

  /// Returns the projection of the property declared by `d`, bound to `receiver`, and specialized
  /// by `specialization`.
  private mutating func emitComputedProperty(
    boundTo receiver: Operand, declaredBy d: SubscriptImpl.ID,
    specializedBy specialization: GenericArguments,
    at site: SourceRange
  ) -> Operand {
    let t = SubscriptImplType(canonicalType(of: d, specializedBy: specialization))!
    let o = RemoteType(ast[d].introducer.value, t.output)
    let r = insert(module.makeAccess(o.access, from: receiver, at: site))!
    let f = module.demandDeclaration(lowering: d)

    let s = module.makeProject(
      o, applying: f, specializedBy: specialization,
      to: [r], at: site)
    return insert(s)!
  }

  // MARK: Move

  /// Replaces `i`, which is a `move` instruction with move-assignment of `semantics == .inout` or
  /// move-initialization if `semantics == .set`, returning the identity of the first instruction
  /// taking the place of `i`.
  ///
  /// After the call, `insertionPoint` set to `nil`.
  mutating func replaceMove(_ i: InstructionID, with semantics: AccessEffect) -> InstructionID {
    let s = module[i] as! Move
    let predecessor = module.instruction(before: i)

    insertionPoint = .before(i)
    emitMove(semantics, s.object, to: s.target, withMovableConformance: s.movable, at: s.site)
    module.removeInstruction(i)

    if let p = predecessor {
      return module.instruction(after: p)!
    } else {
      let b = insertionBlock!
      return .init(b, module[b].instructions.firstAddress!)
    }
  }

  /// Inserts IR for move-initializing/assigning `storage` with `value` at `site`.
  ///
  /// The type of `value` must a built-in or conform to `Movable` in `insertionScope`.
  ///
  /// The value of `semantics` defines the type of move to emit:
  /// - `[.set]` emits move-initialization.
  /// - `[.inout]` emits move-assignment.
  /// - `[.inout, .set]` emits a `move` instruction that will is later replaced during definite
  ///   initialization analysis by either move-assignment if `storage` is found initialized or
  ///   by move-initialization otherwise.
  private mutating func emitMove(
    _ semantics: AccessEffectSet, _ value: Operand, to storage: Operand, at site: SourceRange
  ) {
    precondition(!semantics.isEmpty && semantics.isSubset(of: [.set, .inout]))
    let model = module.type(of: value).ast
    precondition(model == module.type(of: storage).ast)

    // Built-in types are handled as a special case.
    if model.isBuiltin {
      emitMoveBuiltIn(value, to: storage, at: site)
      return
    }

    // Use memcpy of `source` is trivially movable.
    if program.isTriviallyMovable(model, in: insertionScope!) {
      let x0 = insert(module.makeAccess(.sink, from: value, at: site))!
      let x1 = insert(module.makeAccess(.set, from: storage, at: site))!
      insert(module.makeMemoryCopy(x0, x1, at: site))
      insert(module.makeEndAccess(x1, at: site))
      insert(module.makeMarkState(x0, initialized: false, at: site))
      insert(module.makeEndAccess(x0, at: site))
      return
    }

    // Other types must be movable.
    guard
      let movable = program.conformance(
        of: model, to: program.ast.core.movable.type, exposedTo: insertionScope!)
    else { preconditionFailure("expected '\(model)' to be 'Movable'") }

    // Insert a call to the approriate move implementation if its semantics is unambiguous.
    // Otherwise, insert a call to the method bundle.
    if let k = semantics.uniqueElement {
      emitMove(k, value, to: storage, withMovableConformance: movable, at: site)
    } else {
      insert(module.makeMove(value, to: storage, usingConformance: movable, at: site))
    }
  }

  /// Inserts IR for move-initializing/assigning `storage` with built-in `value` at `site`.
  private mutating func emitMoveBuiltIn(
    _ value: Operand, to storage: Operand, at site: SourceRange
  ) {
    // Built-in are always stored.
    let x0 = insert(module.makeAccess(.set, from: storage, at: site))!
    let x1 = insert(module.makeAccess(.sink, from: value, at: site))!
    let x2 = insert(module.makeLoad(x1, at: site))!
    insert(module.makeStore(x2, at: x0, at: site))
    insert(module.makeEndAccess(x1, at: site))
    insert(module.makeEndAccess(x0, at: site))
  }

  /// Inserts IR for move-initializing/assigning `storage` with `value` at `site` using `movable`
  /// to locate the implementations of these operations.
  ///
  /// The value of `semantics` defines the type of move to emit:
  /// - `.set` emits move-initialization.
  /// - `.inout` emits move-assignment.
  ///
  /// - Requires: `storage` does not have a built-in type.
  private mutating func emitMove(
    _ semantics: AccessEffect, _ value: Operand, to storage: Operand,
    withMovableConformance movable: Core.Conformance, at site: SourceRange
  ) {
    let d = module.demandTakeValueDeclaration(semantics, definedBy: movable)
    let f = module.reference(to: d, implementedFor: movable)

    let x0 = insert(module.makeAllocStack(.void, at: site))!
    let x1 = insert(module.makeAccess(.set, from: x0, at: site))!
    let x2 = insert(module.makeAccess(semantics, from: storage, at: site))!
    let x3 = insert(module.makeAccess(.sink, from: value, at: site))!
    insert(module.makeCall(applying: .constant(f), to: [x2, x3], writingResultTo: x1, at: site))
    insert(module.makeEndAccess(x3, at: site))
    insert(module.makeEndAccess(x2, at: site))
    insert(module.makeEndAccess(x1, at: site))
    insert(module.makeDeallocStack(for: x0, at: site))
  }

  // MARK: Copy

  /// Inserts IR for copying `source` to `target` at `site`.
  private mutating func emitCopy(
    _ source: Operand, to target: Operand, at site: SourceRange
  ) {
    let model = module.type(of: source).ast
    precondition(model == module.type(of: target).ast)

    // Built-in types are handled as a special case.
    if model.isBuiltin {
      emitMoveBuiltIn(source, to: target, at: site)
      return
    }

    // Other types must be copyable.
    guard
      let copyable = program.conformance(
        of: model, to: program.ast.core.copyable.type, exposedTo: insertionScope!)
    else { preconditionFailure("expected '\(model)' to be 'Copyable'") }
    emitCopy(source, to: target, withCopyableConformance: copyable, at: site)
  }

  /// Inserts IR for copying `source` to `target` at `site` using `copyable` to locate the
  /// implementation of the copy operation.
  private mutating func emitCopy(
    _ source: Operand, to target: Operand,
    withCopyableConformance copyable: Core.Conformance, at site: SourceRange
  ) {
    let d = module.demandCopyDeclaration(definedBy: copyable)
    let f = module.reference(to: d, implementedFor: copyable)

    let x0 = insert(module.makeAccess(.let, from: source, at: site))!
    let x1 = insert(module.makeAccess(.set, from: target, at: site))!
    insert(module.makeCall(applying: .constant(f), to: [x0], writingResultTo: x1, at: site))
    insert(module.makeEndAccess(x1, at: site))
    insert(module.makeEndAccess(x0, at: site))
  }

  // MARK: Deinitialization

  /// If `storage` is deinitializable in `self.insertionScope`, inserts the IR for deinitializing
  /// it, or reports a diagnostic otherwise.
  ///
  /// Let `T` be the type of `storage`, `storage` is deinitializable iff `T` has a deinitializer
  /// exposed to `self.insertionScope`.
  mutating func emitDeinit(_ storage: Operand, at site: SourceRange) {
    let model = module.type(of: storage).ast

    // Use a no-op if the object is trivially deinitializable.
    if program.isTriviallyDeinitializable(model, in: insertionScope!) {
      insert(module.makeMarkState(storage, initialized: false, at: site))
      return
    }

    // Use custom conformance to `Deinitializable` if possible.
    let concept = program.ast.core.deinitializable.type
    if let c = module.program.conformance(of: model, to: concept, exposedTo: insertionScope!) {
      emitDeinit(storage, withDeinitializableConformance: c, at: site)
      return
    }

    // Object is not deinitializable.
    report(.error(module.type(of: storage).ast, doesNotConformTo: concept, at: site))
  }

  /// Inserts the IR for deinitializing `storage`, using `deinitializable` to identify the locate
  /// the deinitializer to apply.
  private mutating func emitDeinit(
    _ storage: Operand, withDeinitializableConformance deinitializable: Core.Conformance,
    at site: SourceRange
  ) {
    let d = module.demandDeinitDeclaration(from: deinitializable)
    let f = module.reference(to: d, implementedFor: deinitializable)

    let x0 = insert(module.makeAllocStack(.void, at: site))!
    let x1 = insert(module.makeAccess(.set, from: x0, at: site))!
    let x2 = insert(module.makeAccess(.sink, from: storage, at: site))!
    insert(module.makeCall(applying: .constant(f), to: [x2], writingResultTo: x1, at: site))
    insert(module.makeEndAccess(x2, at: site))
    insert(module.makeEndAccess(x1, at: site))
    insert(module.makeMarkState(x0, initialized: false, at: site))
    insert(module.makeDeallocStack(for: x0, at: site))
  }

  /// If `storage` is deinitializable in `self.insertionScope`, inserts the IR for deinitializing
  /// it; reports a diagnostic for each part that isn't deinitializable otherwise.
  private mutating func emitDeinitParts(of storage: Operand, at site: SourceRange) {
    let t = module.type(of: storage).ast

    if program.isTriviallyDeinitializable(t, in: insertionScope!) {
      insert(module.makeMarkState(storage, initialized: false, at: site))
    } else if t.base is UnionType {
      emitDeinitUnionPayload(of: storage, at: site)
    } else if t.hasRecordLayout {
      emitDeinitRecordParts(of: storage, at: site)
    } else {
      report(.error(t, doesNotConformTo: ast.core.deinitializable.type, at: site))
    }
  }

  /// If `storage`, which stores a record, is deinitializable in `self.insertionScope`, inserts
  /// the IR for deinitializing it; reports a diagnostic for each part that isn't
  /// deinitializable otherwise.
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
      let x0 = emitSubfieldView(storage, at: [i], at: site)
      emitDeinit(x0, at: site)
    }
  }

  /// If `storage`, which stores a union, is deinitializable in `self.insertionScope`, inserts
  /// the IR for deinitializing it; reports a diagnostic for each part that isn't
  /// deinitializable otherwise.
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

    let n = emitUnionDiscriminator(storage, at: site)
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
  /// `self.insertionScope`, inserts the IR for deinitializing it; reports a diagnostic for each
  /// part that isn't deinitializable otherwise.
  private mutating func emitDeinitUnionPayload(
    of storage: Operand, containing payload: AnyType, at site: SourceRange
  ) {
    let x0 = insert(module.makeOpenUnion(storage, as: payload, at: site))!
    emitDeinit(x0, at: site)
    insert(module.makeCloseUnion(x0, at: site))
  }

  // MARK: Helpers

  /// Returns the canonical form of `t` in the current insertion scope.
  private func canonical(_ t: AnyType) -> AnyType {
    program.canonical(t, in: insertionScope!)
  }

  /// Returns the type of `d` specialized with `specialization` in the current insertion scope.
  private func canonicalType<T: Decl>(
    of d: T.ID, specializedBy specialization: GenericArguments
  ) -> AnyType {
    let t = program.specialize(program[d].type, for: specialization, in: insertionScope!)
    return canonical(t)
  }

  /// Returns the capabilities required by the receiver of `callee`, which is the type of a member
  /// function or method bundle.
  private func receiverCapabilities(_ callee: AnyType) -> AccessEffectSet {
    switch canonical(callee).base {
    case let t as LambdaType:
      return [RemoteType(t.captures[0].type)?.access ?? .sink]
    case let t as MethodType:
      return t.capabilities
    default:
      unreachable()
    }
  }

  /// Inserts a stack allocation for an object of type `t`.
  private mutating func emitAllocStack(
    for t: AnyType, at site: SourceRange
  ) -> Operand {
    let s = insert(module.makeAllocStack(canonical(t), at: site))!
    frames.top.allocs.append((source: s, mayHoldCaptures: false))
    return s
  }

  /// Inserts the IR for deallocating each allocation in the top frame of `self.frames`.
  private mutating func emitDeallocTopFrame(at site: SourceRange) {
    emitDeallocs(for: frames.top, at: site)
    frames.top.allocs.removeAll()
  }

  /// Inserts the IR for deallocating each allocation in `f`.
  private mutating func emitDeallocs(for f: Frame, at site: SourceRange) {
    for a in f.allocs.reversed() {
      if a.mayHoldCaptures {
        insert(module.makeReleaseCapture(a.source, at: site))
      }
      insert(module.makeDeallocStack(for: a.source, at: site))
    }
  }

  /// Appends the IR for computing the address of the given `subfield` of the record at
  /// `recordAddress` and returns the resulting address.
  mutating func emitSubfieldView(
    _ recordAddress: Operand, at subfield: RecordPath, at site: SourceRange
  ) -> Operand {
    if subfield.isEmpty { return recordAddress }
    let s = module.makeSubfieldView(of: recordAddress, subfield: subfield, at: site)
    return insert(s)!
  }

  /// Emits the IR trapping iff `predicate`, which is an object of type `i1`.
  private mutating func emitGuard(_ predicate: Operand, at site: SourceRange) {
    let failure = appendBlock()
    let success = appendBlock()
    insert(module.makeCondBranch(if: predicate, then: success, else: failure, at: site))

    insertionPoint = .end(of: failure)
    insert(module.makeUnreachable(at: site))
    insertionPoint = .end(of: success)
  }

  /// Emits the IR for copying the union discriminator of `container`, which is the address of
  /// a union container.
  private mutating func emitUnionDiscriminator(
    _ container: Operand, at site: SourceRange
  ) -> Operand {
    let x0 = insert(module.makeAccess(.let, from: container, at: site))!
    let x1 = insert(module.makeUnionDiscriminator(x0, at: site))!
    insert(module.makeEndAccess(x0, at: site))
    return x1
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
    var l = LoopIDs()

    swap(&p, &insertionPoint)
    swap(&f, &frames)
    swap(&l, &loops)
    defer {
      swap(&p, &insertionPoint)
      swap(&f, &frames)
      swap(&l, &loops)
    }
    return try action(&self)
  }

}

extension Emitter {

  /// The local variables and allocations of a lexical scope.
  fileprivate struct Frame {

    /// A map from declaration of a local variable to its corresponding IR in the frame.
    var locals = DeclProperty<Operand>()

    /// The allocations in the frame, in FILO order, paired with a flag that's `true` iff they may
    /// hold captured accesses.
    var allocs: [(source: Operand, mayHoldCaptures: Bool)] = []

    /// Sets the `mayHoldCaptures` on the allocation corresponding to `source`.
    mutating func setMayHoldCaptures(_ source: Operand) {
      let i = allocs.firstIndex(where: { $0.source == source })!
      allocs[i].mayHoldCaptures = true
    }

  }

  /// A stack of frames.
  fileprivate struct Stack {

    /// The frames in the stack, ordered from bottom to top.
    private(set) var elements: [Frame] = []

    /// `true` iff the stack is empty.
    var isEmpty: Bool { elements.isEmpty }

    /// The depth of the stack.
    var depth: Int { elements.count }

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

  /// The identifier of a loop lexically enclosing newly generated IR.
  fileprivate struct LoopID {

    /// The innermost frame enclosing the loop in the emitter context.
    let depth: Int

    /// The block to which control flow jumps when it exits the loop.
    let exit: Block.ID

  }

  /// A stack of loop identifiers.
  fileprivate typealias LoopIDs = [LoopID]

}

extension Diagnostic {

  fileprivate static func error(
    assignmentLHSRequiresMutationMarkerAt site: SourceRange
  ) -> Diagnostic {
    .error("left-hand side of assignment must be marked for mutation", at: site)
  }

  fileprivate static func error(cannotCaptureAccessAt site: SourceRange) -> Diagnostic {
    .error("cannot capture access", at: site)
  }

  fileprivate static func error(
    integerLiteral s: String, overflowsWhenStoredInto t: AnyType,
    at site: SourceRange
  ) -> Diagnostic {
    .error("integer literal '\(s)' overflows when stored into '\(t)'", at: site)
  }

  fileprivate static func error(returnInSubscript s: ReturnStmt.ID, in ast: AST) -> Diagnostic {
    .error("return statement in subscript", at: ast[s].introducerSite)
  }

  fileprivate static func error(
    _ n: Name, hasNoSinkImplementationAt site: SourceRange
  ) -> Diagnostic {
    .error("cannot use '\(n)' to form a rvalue: subscript has no 'sink' implementation", at: site)
  }

  fileprivate static func warning(unreachableStatement s: AnyStmtID, in ast: AST) -> Diagnostic {
    .error("statement will never be executed", at: .empty(at: ast[s].site.first()))
  }

}
