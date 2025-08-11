import BigInt
import FrontEnd
import Utils
import Foundation

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
  // Emitter coding convention:
  //
  // - The `currentSource` property is state set by the `lowering()` family of
  //   functions.
  //
  // - Methods of the form `_snake_case_name` emit a `snake_case_name`
  //   instruction associated with the current `currentSource`.
  //
  // - Methods of the form `_emitXXX(optionalLabel: something ...)`
  //   should only be entered in the context of a call to
  //   `lowering(something) { ... }`.  They may be documented
  //   entirely in terms of a returned operand value, omitting the
  //   fact that they emit the code to compute that operand.  You can
  //   think of them as compound pseudo-instruction generators like
  //   those described in the previous bullet.
  //
  // - `_emitMemberFunctionCallee` and its helpers are currently an
  //   exception to the previous bullet; for some reason they don't
  //   take the `callee` as the first argument.

  /// The diagnostics of lowering errors.
  private var diagnostics: DiagnosticSet = []

  /// The module into which new IR is inserted.
  private var module: Module!

  /// A stack of frames describing the variables and allocations of each traversed lexical scope.
  private var frames = Stack()

  /// The loops in which control flow has currently entered.
  private var loops = LoopIDs()

  /// For each block, the state of `frames` where the block was first entered.
  private var stackOnEntry: [ Block.AbsoluteID: Stack ] = [:]

  /// Where new instructions are inserted.
  var insertionPoint: InsertionPoint?

  /// The function containing the current insertion block.
  var insertionFunction: Function.ID?

  /// The source code associated with instructions to be inserted.
  var currentSource: SourceRange

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

  /// The scope corresponding to the current insertion block.
  private var insertionScope: AnyScopeID? {
    insertionBlock.map({ module[$0, in: insertionFunction!].scope })
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
  private mutating func appendBlock<T: ScopeID>(in s: T) -> Block.AbsoluteID {
    module.appendBlock(in: s, to: insertionFunction!)
  }

  /// Appends a new basic block at the end of `self.insertionFunction`, defined in the same scope
  /// as `self.insertionBlock`.
  private mutating func appendBlock() -> Block.AbsoluteID {
    appendBlock(in: insertionScope!)
  }

  /// Inserts `newInstruction` into `self.module` at the end of `self.insertionPoint`.
  ///
  /// - Requires: `self.insertionPoint` refers to the end of a block.
  @discardableResult
  private mutating func insert<I: Instruction>(_ newInstruction: I) -> Operand? {
    let i = module.insert(newInstruction, at: insertionPoint!, in: insertionFunction!)
    return module.result(of: i, in: insertionFunction!)
  }

  // MARK: Top-level entry points

  /// Calls `action` with an emitter configured to generate IR in `module` and accumulate
  /// diagnostics in `log`.
  static func withInstance<T>(
    insertingIn module: inout Module,
    reportingDiagnosticsTo log: inout DiagnosticSet,
    action: (inout Emitter) -> T
  ) -> T {
    var instance = Self(currentSource: module.program[module.id].site)
    instance.module = consume module
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
    insertionFunction = f

    // Configure the emitter context.
    let entry = module.appendEntry(in: program.scopeContainingBody(of: d)!, to: f)
    let bodyFrame = outermostFrame(of: d, entering: entry)
    self.insertionPoint = .end(of: Block.ID(entry))

    // Emit the body.
    switch b {
    case .block(let s):
      let returnType = ArrowType(program[d].type)!.output
      let returnSite = within(bodyFrame, { $0.siteOfReturn(lowering: s, whichReturns: returnType) })
      lowering(at: returnSite) { $0._return() }

    case .expr(let e):
      within(bodyFrame, { $0.emitStore(value: e, to: $0.returnValue!) })
      lowering(e) { $0._return() }
    }

    return f
  }

  /// Returns the frame enclosing the body of `d`, whose entry block is `entry`.
  private func outermostFrame(of d: FunctionDecl.ID, entering entry: Block.AbsoluteID) -> Frame {
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
  @discardableResult
  private mutating func siteOfReturn(
    lowering b: BraceStmt.ID, whichReturns returnType: AnyType
  ) -> SourceRange {
    switch emit(braceStmt: b) {
    case .next:
      if canonical(returnType).isVoidOrNever {
        lowering(after: b) {
          $0._mark_state(.initialized, $0.returnValue!)
        }
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
    insertionFunction = f

    // Configure the emitter context.
    let entry = module.appendEntry(in: d, to: f)

    self.insertionPoint = .end(of: Block.ID(entry))
    self.frames.push()
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    lowering(d) { me in

      // Convert Hylo arguments to their foreign representation. Note that the last parameter of the
      // entry is the address of the FFI's return value.
      let arguments = me.module[entry].inputs.indices.dropLast().map {
        me._emitConvertToForeign(.parameter(entry, $0))
      }

      // Return type must be foreign convertible unless it is `Void` or `Never`.
      let t = me.module.functions[f]!.output
      let returnType = t.isVoidOrNever ? t
        : me.program.foreignRepresentation(of: t, exposedTo: me.insertionScope!)

      let foreignResult = me._call_ffi(me.program[d].attributes.foreignName!, on: arguments, returning: returnType)

      // Convert the result of the FFI to its Hylo representation and return it.
      switch returnType {
      case .never:
        me._unreachable()
        return

      case .void:
        me._mark_state(.initialized, me.returnValue!)

      default:
        let v = me._emitConvert(foreign: foreignResult, to: me.module.functions[f]!.output)
        me._emitMove(.set, v, to: me.returnValue!)
      }
      me._emitDeallocTopFrame()
      me._return()
    }
  }

  /// Inserts the IR for `d`.
  private mutating func lower(initializer d: InitializerDecl.ID) {
    // Nothing to do for memberwise initializer.
    if ast[d].isMemberwise { return }
    let f = module.demandDeclaration(lowering: d)
    insertionFunction = f

    // Create the function entry.
    let entry = module.appendEntry(in: ast[d].body!, to: f)

    // Configure the locals.
    var locals = DeclProperty<Operand>()
    locals[ast[d].receiver] = .parameter(entry, 0)
    for (i, p) in ast[d].parameters.enumerated() {
      locals[p] = .parameter(entry, i + 1)
    }

    // Emit the body.
    insertionPoint = .end(of: Block.ID(entry))
    let returnSite = within(Frame(locals: locals)) {
      $0.siteOfReturn(lowering: $0.ast[d].body!, whichReturns: .void)
    }

    // If the object is empty, simply mark it initialized.
    let r = module.type(of: .parameter(entry, 0), in: f).ast
    let l = AbstractTypeLayout(of: r, definedIn: program)

    if l.properties.isEmpty {
      lowering(d) {
        $0._mark_state(.initialized, entry.parameter(0))
      }
    }

    lowering(at: returnSite) { $0._return() }
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
    insertionFunction = f

    // Create the function entry.
    let entry = module.appendEntry(in: program.scopeContainingBody(of: d)!, to: f)

    // Configure the locals.
    var parameters = DeclProperty<Operand>()
    parameters[ast[d].receiver] = .parameter(entry, 0)

    let bundle = MethodDecl.ID(program[d].scope)!
    for (i, p) in ast[bundle].parameters.enumerated() {
      parameters[p] = .parameter(entry, i + 1)
    }

    // Emit the body.
    self.insertionPoint = .end(of: Block.ID(entry))
    switch b {
    case .block(let s):
      let returnSite = within(Frame(locals: parameters)) {
        $0.siteOfReturn(lowering: s, whichReturns: ArrowType($0.program[d].type)!.output)
      }
      lowering(at: returnSite) { $0._return() }

    case .expr(let e):
      within(Frame(locals: parameters)) {
        $0.emitStore(value: e, to: $0.returnValue!)
      }
      lowering(e) { $0._return() }
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
    insertionFunction = f

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
    self.insertionPoint = .end(of: Block.ID(entry))
    switch b {
    case .block(let s):
      lower(body: s, of: d, in: Frame(locals: locals))

    case .expr(let e):
      lowering(e) { this in
        this.within(Frame(locals: locals)) { (this) in
          let x0 = this.emitLValue(e)
          let x1 = this._access([this.ast[d].introducer.value], from: x0)
          this._yield(this.ast[d].introducer.value, x1)
        }
        this._return()
      }
    }
  }

  /// Inserts the IR for `b`, which is the body of `d` and is enclosed in `bodyFrame`.
  private mutating func lower(body b: BraceStmt.ID, of d: SubscriptImpl.ID, in f: Frame) {
    switch within(f, { $0.emit(braceStmt: b) }) {
    case .next:
      lowering(after: b) { $0._return() }
    case .return(let s):
      lowering(s) { $0._return() }
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

    let r = RemoteType(.set, program.canonical(typeOf: d))
    let l = ArrowType(
      receiverEffect: .set, environment: ^TupleType(types: [^r]), inputs: [], output: .void)
    let f = SynthesizedFunctionDecl(
      .globalInitialization(d), typed: l, parameterizedBy: [], in: program[d].scope)
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
    let storage = lowering(d) {
      // Allocate storage for all the names declared by `d` in a single aggregate.
      $0._alloc_stack($0.program[d].type)
    }
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
        lowering(p) {
          $0._emitDeinit(s)
        }

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
    lowering(lhs) { me in
      let rhs = me._subfield_view(storage, at: subfield)
      me.emitStore(value: initializer, to: rhs)
      me.emitLocalDeclarations(introducedBy: lhs, referringTo: subfield, relativeTo: storage)
    }
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
    lowering(name) { me in
      let s = me._subfield_view(storage, at: subfield)
      me.frames[me.ast[name].decl] = s
      return s
    }
  }

  /// Inserts the IR for projected local binding `d` .
  ///
  /// - Requires: `d` is a local `let` or `inout` binding.
  private mutating func lower(projectedLocalBinding d: BindingDecl.ID) {
    precondition(program.isLocal(d))
    let source = emitLValue(ast[d].initializer!)
    assignProjections(of: source, to: program[d].pattern)
  }

  /// Assigns the bindings declared in `d` to their corresponding projection of `rhs`.
  private mutating func assignProjections(of rhs: Operand, to d: BindingPattern.ID) {
    precondition(!program[d].introducer.value.isConsuming)
    let k = AccessEffect(program[d].introducer.value)
    let request: AccessEffectSet = module.isSink(rhs, in: insertionFunction!) ? [k, .sink] : [k]

    for (path, name) in ast.names(in: program[d].subpattern) {
      var part = lowering(program[name].decl) {
        $0._subfield_view(rhs, at: path)
      }
      let partDecl = ast[name].decl

      let bindingType = canonical(program[partDecl].type)
      lowering(partDecl) { me in
        part = me._emitCoerce(part, to: bindingType)
        me.frames[partDecl] = me._access(request, from: part, correspondingTo: partDecl)
      }
    }
  }

  // MARK: Synthetic declarations

  /// Synthesizes the implementation of `d`.
  mutating func lower(synthetic d: SynthesizedFunctionDecl) {
    switch d.kind {
    case .deinitialize:
      lower(syntheticDeinit: d)
    case .moveInitialization:
      lower(syntheticMoveInit: d)
    case .moveAssignment:
      lower(syntheticMoveAssign: d)
    case .copy:
      lower(syntheticCopy: d)
    case .equal:
      lower(syntheticEqual: d)
    case .globalInitialization:
      lower(globalBindingInitializer: d)
    case .autoclosure:
      // nothing do to here; expansion is done at the caller side.
      break
    }
  }

  /// Inserts the IR for `d`, which is a synthetic deinitializer.
  private mutating func lower(syntheticDeinit d: SynthesizedFunctionDecl) {
    withPrologue(of: d) { (me, entry) in
      // The receiver is a sink parameter representing the object to deinitialize.
      let receiver = Operand.parameter(entry, 0)
      me._emitDeinitParts(of: receiver)

      me._mark_state(.initialized, me.returnValue)
      me._emitDeallocTopFrame()
      me._return()
    }
  }

  /// Inserts the IR for `d`, which is a synthetic move initialization method.
  private mutating func lower(syntheticMoveInit d: SynthesizedFunctionDecl) {
    withPrologue(of: d) { (me, entry) in
      let receiver = Operand.parameter(entry, 0)
      let argument = Operand.parameter(entry, 1)
      let object = me.module.type(of: receiver, in: me.insertionFunction!).ast

      if object.hasRecordLayout {
        me._emitMoveInitRecordParts(of: receiver, consuming: argument)
      } else if object.base is UnionType {
        me._emitMoveInitUnionPayload(of: receiver, consuming: argument)
      }

      me._mark_state(.initialized, me.returnValue)
      me._emitDeallocTopFrame()
      me._return()
    }
  }

  /// Inserts the IR for initializing the stored parts of `receiver`, which stores a record,
  /// consuming `argument`.
  private mutating func _emitMoveInitRecordParts(
    of receiver: Operand, consuming argument: Operand
  ) {
    let layout = AbstractTypeLayout(of: module.type(of: receiver, in: insertionFunction!).ast, definedIn: program)

    // If the object is empty, simply mark it initialized.
    if layout.properties.isEmpty {
      // Note: we must not call the argument's deinitializer since we're notionally moving its
      // value to the receiver rather than destroying it.
      _mark_state(.initialized, receiver)
      _mark_state(.uninitialized, argument)
      return
    }

    // Otherwise, move initialize each property.
    for i in layout.properties.indices {
      let source = _subfield_view(argument, at: [i])
      let target = _subfield_view(receiver, at: [i])
      _emitMove(.set, source, to: target)
    }
  }

  /// Inserts the IR for initializing the payload of `receiver`, which stores a union container,
  /// consuming `argument`.
  private mutating func _emitMoveInitUnionPayload(
    of receiver: Operand, consuming argument: Operand
  ) {
    let t = UnionType(module.type(of: receiver, in: insertionFunction!).ast)!

    // If union is empty, simply mark it initialized.
    if t.elements.isEmpty {
      _mark_state(.initialized, receiver)
      _emitDeinit(argument)
      return
    }

    // Trivial if the union has a single member.
    if let e = t.elements.uniqueElement {
      _emitMoveInitUnionPayload(of: receiver, consuming: argument, containing: e)
      return
    }

    // Otherwise, use a switch to select the correct move-initialization.
    let targets = UnionSwitch.Targets(
      t.elements.map({ (e) in (key: e, value: appendBlock()) }),
      uniquingKeysWith: { (a, _) in a })
    _emitUnionSwitch(on: argument, toOneOf: targets)

    let tail = appendBlock()
    for (u, b) in targets {
      insertionPoint = .end(of: Block.ID(b))
      _emitMoveInitUnionPayload(of: receiver, consuming: argument, containing: u)
      _branch(to: tail)
    }

    insertionPoint = .end(of: Block.ID(tail))
  }

  /// Inserts the IR for initializing the payload of `receiver`, which stores a union containing
  /// a `payload`, consuming `argument`.
  ///
  /// - Requires: the type of `storage` is a union containing a `payload`.
  private mutating func _emitMoveInitUnionPayload(
    of receiver: Operand, consuming argument: Operand, containing payload: AnyType
  ) {
    // Move the argument.
    let x0 = _open_union(receiver, as: payload, .forInitialization)
    let x1 = _open_union(argument, as: payload)
    _emitMove(.set, x1, to: x0)

    // Close the unions.
    _close_union(x0)
    _close_union(x1)
  }

  /// Inserts the IR for `d`, which is a synthetic move initialization method.
  private mutating func lower(syntheticMoveAssign d: SynthesizedFunctionDecl) {
    withPrologue(of: d) { (me, entry) in
      let receiver = Operand.parameter(entry, 0)
      let argument = Operand.parameter(entry, 1)

      // Deinitialize the receiver.
      me._emitDeinit(receiver)

      // Apply the move-initializer.
      me._emitMove(.set, argument, to: receiver)
      me._mark_state(.initialized, me.returnValue)
      me._emitDeallocTopFrame()
      me._return()
    }
  }

  /// Inserts the IR for `d`, which is a synthetic copy method.
  private mutating func lower(syntheticCopy d: SynthesizedFunctionDecl) {
    withPrologue(of: d) { (me, entry) in
      let source = Operand.parameter(entry, 0)
      let target = Operand.parameter(entry, 1)
      let object = me.module.type(of: source, in: me.insertionFunction!).ast

      if object.hasRecordLayout {
        me._emitCopyRecordParts(from: source, to: target)
      } else if object.base is UnionType {
        me._emitCopyUnionPayload(from: source, to: target)
      }

      me._emitDeallocTopFrame()
      me._return()
    }
  }

  /// Inserts the ID for `d`, which is an equality operator.
  private mutating func lower(syntheticEqual d: SynthesizedFunctionDecl) {
    withPrologue(of: d) { (me, entry) in
      let lhs = Operand.parameter(entry, 0)
      let rhs = Operand.parameter(entry, 1)
      let t = me.module.type(of: lhs, in: me.insertionFunction!).ast

      if t.hasRecordLayout {
        me._emitStorePartsEquality(lhs, rhs, to: me.returnValue!)
      } else if t.base is UnionType {
        me._emitStoreUnionPayloadEquality(lhs, rhs, to: me.returnValue!)
      } else {
        UNIMPLEMENTED("synthetic equality for type '\(t)'")
      }

      me._emitDeallocTopFrame()
      me._return()
    }
  }

  /// Declares `d` in the current module and returns its corresponding
  /// identifier, calling `action` to generate its implementation if
  /// it should be emitted the current module.
  @discardableResult
  private mutating func withPrologue(
    of d: SynthesizedFunctionDecl,
    _ action: (inout Self, _ entry: Block.AbsoluteID) -> Void
  ) -> Function.ID {
    withClearContext { (me) in
      let f = me.module.demandDeclaration(lowering: d)
      if me.shouldEmitBody(of: d, loweredTo: f) {
        me.insertionFunction = f
        let entry = me.module.appendEntry(in: d.scope, to: f)
        me.insertionPoint = .end(of: Block.ID(entry))
        me.frames.push()

        let savedSite = me.currentSource
        defer { me.currentSource = savedSite }
        me.lowering(me.module.id) { me in
          action(&me, entry)
        }
        me.frames.pop()
        assert(me.frames.isEmpty)
      }
      return f
    }
  }

  /// Inserts the IR for copying the stored parts of `source`, which stores a record, to `target`.
  private mutating func _emitCopyRecordParts(
    from source: Operand, to target: Operand
  ) {
    let layout = AbstractTypeLayout(of: module.type(of: source, in: insertionFunction!).ast, definedIn: program)

    // If the object is empty, simply mark the target as initialized.
    if layout.properties.isEmpty {
      _mark_state(.initialized, target)
      return
    }

    // Otherwise, copy each property.
    for i in layout.properties.indices {
      let s = _subfield_view(source, at: [i])
      let t = _subfield_view(target, at: [i])
      _emitCopy(s, to: t)
    }
  }

  /// Inserts the IR for copying `source`, which stores a union container, to `target`.
  private mutating func _emitCopyUnionPayload(
    from source: Operand, to target: Operand
  ) {
    let t = UnionType(module.type(of: source, in: insertionFunction!).ast)!

    // If union is empty, simply mark the target as initialized.
    if t.elements.isEmpty {
      _mark_state(.initialized, target)
      return
    }

    // Trivial if the union has a single member.
    if let e = t.elements.uniqueElement {
      _emitCopyUnionPayload(from: source, containing: e, to: target)
      return
    }

    // Otherwise, use a switch to select the correct copy method.
    let targets = UnionSwitch.Targets(
      t.elements.map({ (e) in (key: e, value: appendBlock()) }),
      uniquingKeysWith: { (a, _) in a })
    _emitUnionSwitch(on: source, toOneOf: targets)

    let tail = appendBlock()
    for (u, b) in targets {
      insertionPoint = .end(of: Block.ID(b))
      _emitCopyUnionPayload(from: source, containing: u, to: target)
      _branch(to: tail)
    }

    insertionPoint = .end(of: Block.ID(tail))
  }

  /// Inserts the IR for copying `source`, which stores a union
  /// containing a `payload`, to `target`.
  private mutating func _emitCopyUnionPayload(
    from source: Operand, containing payload: AnyType, to target: Operand
  ) {
    let x0 = _open_union(source, as: payload)
    let x1 = _open_union(target, as: payload, .forInitialization)
    _emitCopy(x0, to: x1)
    _close_union(x0)
    _close_union(x1)
  }

  /// Inserts the IR for lowering `d`, which is a global binding initializer, returning the ID of
  /// the lowered function.
  @discardableResult
  private mutating func lower(globalBindingInitializer d: SynthesizedFunctionDecl) -> Function.ID {
    withPrologue(of: d) { (me, entry) in
      let storage = Operand.parameter(entry, 0)
      guard case .globalInitialization(let binding) = d.kind else { unreachable() }

      let initializer = me.program[binding].initializer!
      me.lowering(initializer) { me in
        me.emitInitStoredLocalBindings(
          in: me.program[binding].pattern.subpattern, referringTo: [], relativeTo: storage,
          consuming: initializer)
        me._mark_state(.initialized, me.returnValue)
        me._emitDeallocTopFrame()
        me._return()
      }
    }
  }

  private mutating func lower(syntheticAutoclosure d: SynthesizedFunctionDecl) -> Function.ID {
    guard case .autoclosure(let argument) = d.kind else { unreachable() }
    let f = module.demandDeclaration(lowering: d)
    insertionFunction = f
    let entry = module.appendEntry(in: d.scope, to: f)

    insertionPoint = .end(of: Block.ID(entry))
    self.frames.push()
    defer {
      self.frames.pop()
      assert(self.frames.isEmpty)
    }

    // Emit the body.
    emitStore(value: argument, to: returnValue!)
    lowering(argument) { $0._return() }

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
    // Restore frames upon exit so that closing the surrounding block works.
    let savedFrames = frames
    defer { frames = savedFrames }
    lowering(s) { me in
      while !me.frames.isEmpty {
        me._emitDeallocTopFrame()
        me.frames.pop()
      }
      me._return()
    }
  }

  /// Inserts IR for breaking from innermost loop, anchoring instructions at `s`.
  private mutating func emitControlFlow(break s: BreakStmt.ID) {
    // Restore frames upon exit so that closing the surrounding block works.
    let savedFrames = frames
    defer { frames = savedFrames }
    let innermost = loops.last!
    let f = insertionFunction!
    lowering(s) { me in
      while me.frames.depth > innermost.depth {
        me._emitDeallocTopFrame()
        me.frames.pop()
      }
      me._branch(to: Block.AbsoluteID(f, innermost.exit))
    }
  }

  /// Inserts the IR for `s`, returning its effect on control flow.
  private mutating func emit<T: StmtID>(stmt s: T) -> ControlFlow {
    lowering(s) { e in
      switch s.kind {
      case AssignStmt.self:
        return e._emit(assignStmt: .init(s)!)
      case BraceStmt.self:
        return e.emit(braceStmt: .init(s)!)
      case BreakStmt.self:
        return e.emit(breakStmt: .init(s)!)
      case ConditionalBindingStmt.self:
        return e.emit(conditionalBindingStmt: .init(s)!)
      case ConditionalCompilationStmt.self:
        return e.emit(condCompilationStmt: .init(s)!)
      case ConditionalStmt.self:
        return e.emit(conditionalStmt: .init(s)!)
      case DeclStmt.self:
        return e.emit(declStmt: .init(s)!)
      case DiscardStmt.self:
        return e.emit(discardStmt: .init(s)!)
      case DoWhileStmt.self:
        return e._emit(doWhileStmt: .init(s)!)
      case ExprStmt.self:
        return e._emit(exprStmt: .init(s)!)
      case ForStmt.self:
        return e.emit(forStmt: .init(s)!)
      case ReturnStmt.self:
        return e._emit(returnStmt: .init(s)!)
      case WhileStmt.self:
        return e.emit(whileStmt: .init(s)!)
      case YieldStmt.self:
        return e.emit(yieldStmt: .init(s)!)
      default:
        unexpected(s, in: e.ast)
      }
    }
  }

  private mutating func _emit(assignStmt s: AssignStmt.ID) -> ControlFlow {
    // The LHS should must be marked for mutation even if the statement denotes initialization.
    guard ast.isMarkedForMutation(ast[s].left) else {
      let p = program[s].left.site.start
      report(.error(assignmentLHSRequiresMutationMarkerAt: .empty(at: p)))
      return .next
    }

    // The RHS is evaluated first, stored into some local storage, and moved to the LHS. Implicit
    // conversion is necessary if the RHS is subtype of the LHS.
    let rhs = _alloc_stack(program[s].left.type)
    emitStore(convertingIfNecessary: ast[s].right, to: rhs)
    let lhs = emitLValue(ast[s].left)
    _emitMove([.inout, .set], rhs, to: lhs)

    return .next
  }

  private mutating func emit(braceStmt s: BraceStmt.ID) -> ControlFlow {
    frames.push()
    defer {
      lowering(after: s) { $0._emitDeallocTopFrame() }
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
    .break(s)
  }

  private mutating func emit(condCompilationStmt s: ConditionalCompilationStmt.ID) -> ControlFlow {
    emit(stmtList: ast[s].expansion(for: ast.compilationConditions))
  }

  private mutating func emit(conditionalBindingStmt s: ConditionalBindingStmt.ID) -> ControlFlow {
    let storage = emitAllocation(binding: ast[s].binding)

    let fail = appendBlock()
    let next = emitConditionalNarrowing(
      ast[s].binding, movingConsumedValuesTo: storage,
      branchingOnFailureTo: fail, in: insertionScope!)

    insertionPoint = .end(of: Block.ID(fail))
    let flow = emit(braceStmt: ast[s].fallback)
    emitControlFlow(flow) { me in
      me.lowering(me.ast[s].fallback) { $0._unreachable() }
    }

    insertionPoint = .end(of: Block.ID(next))
    return .next
  }

  private mutating func emit(conditionalStmt s: ConditionalStmt.ID) -> ControlFlow {
    lowering(s) { me in
      let (firstBranch, secondBranch) = me.emitTest(condition: me.ast[s].condition, in: AnyScopeID(s))
      let tail = me.appendBlock()

      me.insertionPoint = .end(of: firstBranch)
      let f1 = me.emit(braceStmt: me.ast[s].success)
      me.emitControlFlow(f1) { (me) in
        me._branch(to: tail)
      }

      me.insertionPoint = .end(of: secondBranch)
      guard let failure = me.ast[s].failure else {
        me._branch(to: tail)
        me.insertionPoint = .end(of: Block.ID(tail))
        return .next
      }

      let f2 = me.emit(stmt: failure.value)
      me.emitControlFlow(f2) { (me) in
        me._branch(to: tail)
      }

      me.insertionPoint = .end(of: Block.ID(tail))
      return .next
    }
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
    lowering(s) { $0._emitDeinit(v) }
    return .next
  }

  private mutating func _emit(doWhileStmt s: DoWhileStmt.ID) -> ControlFlow {
    let body = appendBlock(in: ast[s].body)
    let exit = appendBlock(in: ast[s].body)
    loops.append(LoopID(depth: frames.depth, exit: Block.ID(exit)))
    defer { loops.removeLast() }

    lowering(before: s) { $0._branch(to: body) }
    insertionPoint = .end(of: Block.ID(body))

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
    _emitDeallocTopFrame()
    frames.pop()

    _cond_branch(if: c, then: body, else: exit)
    insertionPoint = .end(of: Block.ID(exit))
    return .next
  }

  private mutating func _emit(exprStmt s: ExprStmt.ID) -> ControlFlow {
    let v = emitStore(value: ast[s].expr)
    _emitDeinit(v)
    return .next
  }

  /// Inserts the IR for loop `s`, returning its effect on control flow.
  private mutating func emit(forStmt s: ForStmt.ID) -> ControlFlow {
    if program.ast.isConsuming(s) {
      return emit(consumingForStmt: s)
    } else {
      return emit(nonConsumingForStmt: s)
    }
  }

  /// Inserts the IR for consuming loop `s`, returning its effect on control flow.
  private mutating func emit(consumingForStmt s: ForStmt.ID) -> ControlFlow {
    let d = program[program[s].domain.value].type
    let c = program.conformance(
      of: d, to: ast.core.iterator.type, exposedTo: program[s].scope)!
    let witness = IteratorWitness(c, in: &module)
    let introducer = program[s].introducerSite

    // The collection on which the loop iterates.
    let domain = emitLValue(program[s].domain.value)
    // The element extracted before each iteration.
    let element =  lowering(at: introducer) {
      $0._alloc_stack(^$0.ast.optional(witness.element))
    }
    // The storage containing the result of binding each element.
    let storage = emitAllocation(binding: ast[s].binding)

    // The "head" of the loop; extracts the next element.
    let head = appendBlock(in: s)
    // The remainder of the program, after the loop.
    let exit = appendBlock()

    loops.append(LoopID(depth: frames.depth, exit: Block.ID(exit)))
    defer { loops.removeLast() }

    lowering(at: introducer) { me in
      me._branch(to: head)
      me.insertionPoint = .end(of: Block.ID(head))

      let x0 = me._access(.inout, from: domain)
      me._emitApply(witness.next, to: [x0], writingResultTo: element)
      me._end_access(x0)
    }

    let next = emitUnionNarrowing(
      from: element, to: ast[ast[s].binding].pattern, typed: witness.element,
      movingConsumedValuesTo: storage, branchingOnFailureTo: exit,
      in: insertionScope!)

    // TODO: Filter
    precondition(ast[s].filter == nil, "loop filters are not implemented")

    insertionPoint = .end(of: Block.ID(next))
    // TODO: check this currentSource control logic.
    lowering(at: introducer) { me in
      let flow = me.emit(braceStmt: me.ast[s].body)
      me.emitControlFlow(flow) { (me) in
        me.lowering(after: me.program[s].body) { $0._branch(to: head) }
      }
    }

    insertionPoint = .end(of: Block.ID(exit))
    return .next
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
    lowering(at: introducer) { me in
      let quit = me._alloc_stack(^me.ast.coreType("Bool")!)
      // The start and end positions of the collection; the former is updated with each iteration.
      let (currentPosition, endPosition) = me._emitPositions(
        forIteratingOver: domain, usingWitness: collectionWitness)

      // The "head" of the loop; tests for the exit condition.
      let head = me.appendBlock(in: s)
      // The "lens" of the loop; tests for narrowing conditions and applies filters.
      let enter = me.appendBlock(in: s)
      // The "tail" of the loop; increments the index and jumps back to the head.
      let tail = me.appendBlock(in: s)
      // The remainder of the program, after the loop.
      let exit = me.appendBlock()

      me.loops.append(LoopID(depth: me.frames.depth, exit: Block.ID(exit)))
      defer { me.loops.removeLast() }

      me._branch(to: head)

      me.insertionPoint = .end(of: Block.ID(head))
      let x0 = me._access(.let, from: currentPosition)
      let x1 = me._access(.let, from: endPosition)
      me._emitApply(.constant(equal), to: [x0, x1], writingResultTo: quit)
      me._end_access(x1)
      me._end_access(x0)
      let x2 = me._emitLoadBuiltinBool(quit)
      me._cond_branch(if: x2, then: exit, else: enter)

      me.insertionPoint = .end(of: Block.ID(enter))
      let x6 = me._access(.let, from: domain)
      let x7 = me._access(.let, from: currentPosition)

      let t = RemoteType(.let, collectionWitness.element)
      let x8 = me._project(
        t, applying: collectionWitness.access.function,
        specializedBy: collectionWitness.access.specialization, to: [x6, x7])

      if me.module.type(of: x8, in: me.insertionFunction!).ast != collectionWitness.element {
        UNIMPLEMENTED("narrowing projections #1099")
      }

      me.emitLocalDeclarations(
        introducedBy: me.program[s].binding.pattern, referringTo: [], relativeTo: x8)

      // TODO: Filter
      precondition(me.ast[s].filter == nil, "loop filters are not implemented")

      let flow = me.emit(braceStmt: me.ast[s].body)
      me.emitControlFlow(flow) { (me) in
        me.lowering(after: me.program[s].body) { $0._branch(to: tail) }
      }

      me.insertionPoint = .end(of: Block.ID(tail))
      let x3 = me._alloc_stack(collectionWitness.position)
      let x4 = me._access(.let, from: domain)
      let x5 = me._access(.let, from: currentPosition)
      me._emitApply(collectionWitness.positionAfter, to: [x4, x5], writingResultTo: x3)
      me._end_access(x4)
      me._end_access(x5)
      me._emitMove(.inout, x3, to: currentPosition)
      me._dealloc_stack(x3)
      me._branch(to: head)

      me.insertionPoint = .end(of: Block.ID(exit))
    }
    return .next
  }

  /// Inserts the IR for initializing the start and end positions of `domain`, whose conformance to
  /// the collection trait is witnessed by `witness`, returning these positions.
  private mutating func _emitPositions(
    forIteratingOver domain: Operand,
    usingWitness witness: CollectionWitness
  ) -> (startIndex: Operand, endIndex: Operand) {
    let start = _alloc_stack(witness.position)
    let end = _alloc_stack(witness.position)

    let x0 = _access(.let, from: domain)
    _emitApply(witness.startPosition, to: [x0], writingResultTo: start)
    _emitApply(witness.endPosition, to: [x0], writingResultTo: end)
    _end_access(x0)

    return (startIndex: start, endIndex: end)
  }

  /// Inserts the IR for `s`, returning its effect on control flow.
  private mutating func _emit(returnStmt s: ReturnStmt.ID) -> ControlFlow {
    if module[insertionFunction!].isSubscript {
      report(.error(returnInSubscript: s, in: ast))
      return .next
    }

    if let e = ast[s].value {
      emitStore(value: e, to: returnValue!)
    } else {
      _mark_state(.initialized, returnValue)
    }

    // The return instruction is emitted by the caller handling this control-flow effect.
    return .return(s)
  }

  private mutating func emit(whileStmt s: WhileStmt.ID) -> ControlFlow {
    // Enter the loop.
    let head = appendBlock(in: s)
    lowering(before: s) { $0._branch(to: head) }

    // Test the conditions.
    insertionPoint = .end(of: Block.ID(head))
    let (body, exit) = emitTest(condition: ast[s].condition, in: AnyScopeID(s))

    // Add the current loop to the emitter context.
    loops.append(LoopID(depth: frames.depth, exit: exit))
    defer { loops.removeLast() }

    // Execute the body.
    insertionPoint = .end(of: body)
    let flow = emit(braceStmt: ast[s].body)
    lowering(after: program[s].body) {
      $0.emitControlFlow(flow) {
        $0._branch(to: head)
      }
    }

    // Exit.
    insertionPoint = .end(of: exit)
    return .next
  }

  private mutating func emit(yieldStmt s: YieldStmt.ID) -> ControlFlow {
    // TODO: Read mutability of current subscript

    let x0 = emitLValue(ast[s].value)
    lowering(s) { me in
      let x1 = me._access(.let, from: x0)
      me._yield(.let, x1)
    }
    return .next
  }

  // MARK: Values

  /// Inserts the IR for initializing `storage` by storing `value` to it.
  private mutating func _emitInitialize(storage: Operand, to value: Operand) {
    let x0 = _access(.set, from: storage)
    _store(value, x0)
    _end_access(x0)
  }

  /// Inserts the IR for storing the value of `e` to a fresh stack allocation, returning the
  /// address of this allocation.
  @discardableResult
  private mutating func emitStore<T: ExprID>(value e: T) -> Operand {
    let r = lowering(e) {
      $0._alloc_stack($0.program[e].type)
    }
    emitStore(value: e, to: r)
    return r
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  ///
  /// - Requires: `storage` is the address of some uninitialized memory block capable of storing
  ///   the value of `e`.
  private mutating func emitStore<T: ExprID>(value e: T, to storage: Operand) {
    lowering(e) { me in
      switch e.kind {
      case BooleanLiteralExpr.self:
        me._emitStore(boolean: me.ast[BooleanLiteralExpr.ID(e)!].value, to: storage)
      case BufferLiteralExpr.self:
        me._emitStore(BufferLiteralExpr.ID(e)!, to: storage)
      case CaptureExpr.self:
        me._emitStore(CaptureExpr.ID(e)!, to: storage)
      case CastExpr.self:
        me.emitStore(CastExpr.ID(e)!, to: storage)
      case ConditionalExpr.self:
        me._emitStore(ConditionalExpr.ID(e)!, to: storage)
      case FloatLiteralExpr.self:
        me.emitStore(FloatLiteralExpr.ID(e)!, to: storage)
      case FunctionCallExpr.self:
        me._emitStore(FunctionCallExpr.ID(e)!, to: storage)
      case IntegerLiteralExpr.self:
        me.emitStore(IntegerLiteralExpr.ID(e)!, to: storage)
      case LambdaExpr.self:
        me._emitStore(LambdaExpr.ID(e)!, to: storage)
      case NameExpr.self:
        me._emitStore(NameExpr.ID(e)!, to: storage)
      case PragmaLiteralExpr.self:
        me._emitStore(PragmaLiteralExpr.ID(e)!, to: storage)
      case RemoteTypeExpr.self:
        me._emitStore(RemoteTypeExpr.ID(e)!, to: storage)
      case SequenceExpr.self:
        me.emitStore(SequenceExpr.ID(e)!, to: storage)
      case SubscriptCallExpr.self:
        me.emitStore(SubscriptCallExpr.ID(e)!, to: storage)
      case StringLiteralExpr.self:
        me._emitStore(StringLiteralExpr.ID(e)!, to: storage)
      case TupleExpr.self:
        me._emitStore(TupleExpr.ID(e)!, to: storage)
      case TupleMemberExpr.self:
        me._emitStore(TupleMemberExpr.ID(e)!, to: storage)
      default:
        unexpected(e, in: me.ast)
      }
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func _emitStore(_ e: BufferLiteralExpr.ID, to storage: Operand) {
    if program[e].elements.isEmpty {
      _mark_state(.initialized, storage)
      return
    }

    // The elements of a buffer literal have the same type.
    for (i, v) in program[e].elements.enumerated() {
      let x0 = lowering(v) { $0._advanced(storage, byStrides: i) }
      emitStore(value: v, to: x0)
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func _emitStore(_ e: CaptureExpr.ID, to storage: Operand) {
    let t = RemoteType(program[e].type)!
    let x0 = emitLValue(program[e].source)
    let x1 = _access([t.access], from: x0)
    _emitStore(access: x1, to: storage)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: CastExpr.ID, to storage: Operand) {
    switch ast[e].direction {
    case .up:
      _emitStore(upcast: e, to: storage)
    case .down:
      emitStore(downcast: e, to: storage)
    case .pointerConversion:
      _emitStore(pointerConversion: e, to: storage)
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func _emitStore(upcast e: CastExpr.ID, to storage: Operand) {
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
      let x0 = _open_union(storage, as: source, .forInitialization)
      emitStore(value: ast[e].left, to: x0)
      _close_union(x0)
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
  private mutating func _emitStore(pointerConversion e: CastExpr.ID, to storage: Operand) {
    let x0 = emitLValue(pointerConversion: e)

    // Consuming a pointee requires a conformance to `Movable`.
    let u = canonical(program[e].type)
    let movable = program.ast.core.movable.type
    if !program.conforms(u, to: movable, in: insertionScope!) {
      report(.error(module.type(of: x0, in: insertionFunction!).ast, doesNotConformTo: movable, at: ast[e].site))
      return
    }

    _emitMove([.inout, .set], x0, to: storage)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func _emitStore(_ e: ConditionalExpr.ID, to storage: Operand) {
    let (success, failure) = emitTest(condition: ast[e].condition, in: AnyScopeID(e))
    let tail = appendBlock()

    // Emit the success branch.
    insertionPoint = .end(of: success)
    within(Frame()) { $0.emitStore(value: $0.ast[e].success, to: storage) }
    _branch(to: tail)

    // Emit the failure branch.
    insertionPoint = .end(of: failure)
    within(Frame()) { $0.emitStore(value: $0.ast[e].failure.value, to: storage) }
    _branch(to: tail)
    insertionPoint = .end(of: Block.ID(tail))
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: FloatLiteralExpr.ID, to storage: Operand) {
    emitStore(numericLiteral: e, to: storage)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func _emitStore(_ e: FunctionCallExpr.ID, to storage: Operand) {
    // Handle built-ins and constructor calls.
    if let n = NameExpr.ID(ast[e].callee) {
      switch program[n].referredDecl {
      case .builtinFunction(let f):
        let x0 = _emit(apply: f, to: ast[e].arguments)
        let x1 = _access(.set, from: storage)
        _store(x0, x1)
        return

      case .constructor:
        _emit(constructorCall: e, initializing: storage)
        return

      default:
        break
      }
    }


    // Arguments are evaluated first, from left to right; callee and captures are evaluated next
    let arguments = lowering(after: e)  {
      $0._emitArguments(to: $0.ast[e].callee, in: CallID(e), usingExplicit: $0.ast[e].arguments)
    }
    let m = ast.isMarkedForMutation(ast[e].callee)
    let (callee, captures) = emitFunctionCallee(ast[e].callee, markedForMutation: m)

    // Call is evaluated last.
    _emitApply(callee, to: captures + arguments, writingResultTo: storage)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: IntegerLiteralExpr.ID, to storage: Operand) {
    emitStore(numericLiteral: e, to: storage)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func _emitStore(_ e: LambdaExpr.ID, to storage: Operand) {
    let callee = lower(function: ast[e].decl)
    let r = FunctionReference(
      to: callee, in: module,
      specializedBy: module.specialization(in: insertionFunction!), in: insertionScope!)

    let x0 = _address_to_pointer(.constant(r))
    let x1 = _subfield_view(storage, at: [0])
    _emitInitialize(storage: x1, to: x0)

    let arrow = ArrowType(program.canonical(program[e].type, in: insertionScope!))!
    let x2 = _subfield_view(storage, at: [1])

    // Simply mark the lambda's environment initialized if it's empty.
    if arrow.environment == .void {
      _mark_state(.initialized, x2)
      return
    }

    // Otherwise, initialize each capture individually.
    var i = 0
    for b in program[e].decl.explicitCaptures {
      // TODO: See #878
      guard program[b].pattern.subpattern.kind == NamePattern.self else { UNIMPLEMENTED() }
      let y0 = _subfield_view(x2, at: [i])
      emitStore(value: program[b].initializer!, to: y0)
      i += 1
    }

    for c in program[e].decl.implicitCaptures {
      let y0 = _emitLValue(directReferenceTo: c.decl)
      let y1 = _access([c.type.access], from: y0)
      let y2 = _subfield_view(x2, at: [i])
      _emitStore(access: y1, to: y2)
      i += 1
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func _emitStore(_ e: NameExpr.ID, to storage: Operand) {
    let x0 = emitLValue(e)
    _emitMove([.inout, .set], x0, to: storage)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func _emitStore(
    _ e: PragmaLiteralExpr.ID, to storage: Operand
  ) {
    switch ast[e].kind {
    case .file:
      _emitStore(utf8: currentSource.file.url.absoluteURL.fileSystemPath.utf8, to: storage)
    case .line:
      _emitStore(int: currentSource.start.line.number, to: storage)
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func _emitStore(_ e: RemoteTypeExpr.ID, to storage: Operand) {
    let t = RemoteType(program[e].type)!
    let x0 = emitLValue(program[e].operand)
    let x1 = _access([t.access], from: x0)
    _emitStore(access: x1, to: storage)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: SequenceExpr.ID, to storage: Operand) {
    emitStore(program[e].folded, to: storage)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func emitStore(_ e: FoldedSequenceExpr, to storage: Operand) {
    switch e {
    case .infix(let callee, let lhs, let rhs):
      let t = canonical(program[callee.expr].type)
      let calleeType = ArrowType(t)!.lifted

      // Emit the operands, starting with RHS.
      let r = emit(infixOperand: rhs, passedTo: ParameterType(calleeType.inputs[1].type)!)
      let l = emit(infixOperand: lhs, passedTo: ParameterType(calleeType.inputs[0].type)!)

      // The callee must be a reference to member function.
      guard case .member(let d, let a, _) = program[callee.expr].referredDecl else {
        unreachable()
      }

      let lhsIsMarkedForMutation = program.ast.isMarkedForMutation(lhs)
      lowering(callee.expr) { me in
        let (callee, captures) = me._emitMemberFunctionCallee(
          referringTo: d, memberOf: l, markedForMutation: lhsIsMarkedForMutation,
          specializedBy: a, in: me.program[callee.expr].scope)
        me._emitApply(callee, to: captures + [r], writingResultTo: storage)
      }

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
  private mutating func _emitStore(_ e: StringLiteralExpr.ID, to storage: Operand) {
    _emitStore(utf8: ast[e].value.unescaped.utf8, to: storage)
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func _emitStore(_ e: TupleExpr.ID, to storage: Operand) {
    if ast[e].elements.isEmpty {
      _mark_state(.initialized, storage)
      return
    }

    for (i, element) in ast[e].elements.enumerated() {
      let xi = lowering(element.value) {
        $0._subfield_view(storage, at: [i])
      }
      emitStore(value: element.value, to: xi)
    }
  }

  /// Inserts the IR for storing the value of `e` to `storage`.
  private mutating func _emitStore(_ e: TupleMemberExpr.ID, to storage: Operand) {
    let x0 = emitLValue(e)
    _emitMove([.inout, .set], x0, to: storage)
  }

  /// Inserts the IR to store the value of `e` to `storage`, converting it to the type of `storage`
  /// if necessary.
  ///
  /// The type comparison is performed in the scope of `e`.
  private mutating func emitStore<T: ExprID>(
    convertingIfNecessary e: T,
    to storage: Operand
  ) {
    let lhsType = module.type(of: storage, in: insertionFunction!).ast
    let rhsType = canonical(program[e].type)

    if program.areEquivalent(lhsType, rhsType, in: program[e].scope) {
      emitStore(value: e, to: storage)
    } else if lhsType.base is UnionType {
      lowering(e) { me in
        let x0 = me._open_union(storage, as: rhsType, .forInitialization)
        me.emitStore(value: e, to: x0)
        me._close_union(x0)
      }
    } else {
      UNIMPLEMENTED()
    }
  }

  /// Writes the value of `literal` to `storage`.
  private mutating func emitStore<T: NumericLiteralExpr>(
    numericLiteral literal: T.ID, to storage: Operand
  ) {
    let literalType = canonical(program[literal].type)

    lowering(literal) { me in
      switch literalType {
      case me.ast.coreType("Int")!:
        me._emitStore(integer: literal, signed: true, bitWidth: 64, to: storage)
      case me.ast.coreType("Int8")!:
        me._emitStore(integer: literal, signed: true, bitWidth: 8, to: storage)
      case me.ast.coreType("Int32")!:
        me._emitStore(integer: literal, signed: true, bitWidth: 32, to: storage)
      case me.ast.coreType("Int64")!:
        me._emitStore(integer: literal, signed: true, bitWidth: 64, to: storage)
      case me.ast.coreType("UInt")!:
        me._emitStore(integer: literal, signed: false, bitWidth: 64, to: storage)
      case me.ast.coreType("UInt8")!:
        me._emitStore(integer: literal, signed: false, bitWidth: 8, to: storage)
      case me.ast.coreType("UInt32")!:
        me._emitStore(integer: literal, signed: false, bitWidth: 32, to: storage)
      case me.ast.coreType("UInt64")!:
        me._emitStore(integer: literal, signed: false, bitWidth: 64, to: storage)
      case me.ast.coreType("Float64")!:
        me._emitStore(
          floatingPoint: literal, to: storage, evaluatedBy: FloatingPointConstant.float64(_:))
      case me.ast.coreType("Float32")!:
        me._emitStore(
          floatingPoint: literal, to: storage, evaluatedBy: FloatingPointConstant.float32(_:))
      default:
        UNIMPLEMENTED()
      }
    }
  }

  /// Writes the value of `literal` to `storage`, knowing it is a core floating-point instance
  /// evaluated by `evaluate`.
  private mutating func _emitStore<T: NumericLiteralExpr>(
    floatingPoint literal: T.ID, to storage: Operand,
    evaluatedBy evaluate: (String) -> FloatingPointConstant
  ) {
    let x0 = _subfield_view(storage, at: [0])
    let x1 = _access(.set, from: x0)
    let x2 = Operand.constant(evaluate(ast[literal].value))
    _store(x2, x1)
  }

  /// Writes the value of `literal` to `storage`, knowing it is a core integer instance with given
  /// sign and width.
  private mutating func _emitStore<T: NumericLiteralExpr>(
    integer literal: T.ID, signed: Bool, bitWidth: Int, to storage: Operand
  ) {
    let literalValue = ast[literal].value
    guard let bits = WideUInt(hyloLiteral: literalValue, signed: signed, bitWidth: bitWidth) else {
      diagnostics.insert(
        .error(
          integerLiteral: literalValue,
          overflowsWhenStoredInto: program[literal].type,
          at: currentSource))
      return
    }

    let x0 = _subfield_view(storage, at: [0])
    let x1 = _access(.set, from: x0)
    let x2 = Operand.constant(IntegerConstant(bits))
    _store(x2, x1)
  }

  /// Writes an instance of `Hylo.Bool` with value `v` to `storage`.
  ///
  /// - Requires: `storage` is the address of uninitialized memory of type `Hylo.Int`.
  private mutating func _emitStore(boolean v: Bool, to storage: Operand) {
    let x0 = _subfield_view(storage, at: [0])
    let x1 = _access(.set, from: x0)
    _store(.i1(v), x1)
    _end_access(x1)
  }

  /// Writes an instance of `Hylo.Int` with value `v` to `storage`.
  ///
  /// - Requires: `storage` is the address of uninitialized memory of type `Hylo.Int`.
  mutating func _emitStore(int v: Int, to storage: Operand) {
    let x0 = _subfield_view(storage, at: [0])
    let x1 = _access(.set, from: x0)
    _store(.word(v), x1)
    _end_access(x1)
  }

  /// Writes an instance of `Hylo.String` with value `v` to `storage`.
  ///
  /// - Requires: `storage` is the address of uninitialized memory of type `Hylo.String`.
  private mutating func _emitStore(utf8 v: String.UTF8View, to storage: Operand) {
    let x0 = _constant_string(utf8: Data(v))
    let x1 = _subfield_view(storage, at: [0, 0])
    let x2 = _access(.set, from: x1)
    _store(x0, x2)
    _end_access(x2)
  }

  /// Inserts the IR for storing `a`, which is an `access`, to `storage`.
  ///
  /// - Parameter storage: an address derived from an `alloc_stack` that is outlived by the
  ///   provenances of `a`.
  private mutating func _emitStore(
    access a: Operand, to storage: Operand
  ) {
    guard let s = module.provenances(storage, in: insertionFunction!).uniqueElement, module[s] is AllocStack else {
      report(.error(cannotCaptureAccessAt: currentSource))
      return
    }

    let x0 = _access(.set, from: storage)
    _capture(a, in: x0)
    _end_access(x0)
    frames.top.setMayHoldCaptures(s)
  }

  /// Inserts the IR for calling `callee` on `arguments`, storing the result to `storage`.
  private mutating func _emitApply(
    _ callee: Callee, to arguments: [Operand],
    writingResultTo storage: Operand
  ) {
    switch callee {
    case .direct(let r):
      _emitApply(.constant(r), to: arguments, writingResultTo: storage)
    case .lambda(let r):
      _emitApply(r, to: arguments, writingResultTo: storage)
    case .bundle(let r):
      _emitApply(r, to: arguments, writingResultTo: storage)
    }
  }

  /// Inserts the IR for calling `callee` on `arguments`, storing the result to `storage`.
  private mutating func _emitApply(
    _ callee: Operand, to arguments: [Operand],
    writingResultTo storage: Operand
  ) {
    let o = _access(.set, from: storage)
    _call(callee, arguments, to: o)
    _end_access(o)
  }

  /// Inserts the IR for calling `callee` on `arguments`, storing the result to `storage`.
  private mutating func _emitApply(
    _ callee: BundleReference<MethodDecl>, to arguments: [Operand],
    writingResultTo storage: Operand
  ) {
    let o = _access(.set, from: storage)
    _call_bundle(callee, arguments, to: o, scopeOfUse: insertionScope!)
    _end_access(o)
  }

  /// Inserts the IR for given constructor `call`, which initializes storage `r` by applying
  /// initializer `d` parameterized by `a`.
  ///
  /// - Parameters:
  ///   - call: The syntax of the call.
  ///   - s: The address of uninitialized storage typed by the receiver of `call`. This storage is
  ///     borrowed for initialization after evaluating `call`'s arguments and before the call.
  private mutating func _emit(constructorCall call: FunctionCallExpr.ID, initializing s: Operand) {
    let callee = NameExpr.ID(ast[call].callee)!
    guard case .constructor(let d, let a) = program[callee].referredDecl else {
      preconditionFailure()
    }

    // Handle memberwise constructor calls.
    if ast[d].isMemberwise {
      _emit(memberwiseInitializerCall: call, initializing: s)
      return
    }

    // Arguments are evaluated first, from left to right.
    let c = ast[call]
    let arguments = lowering(after: call) {
      $0._emitArguments(to: c.callee, in: CallID(call), usingExplicit: c.arguments)
    }

    // Receiver is captured next.
    let receiver = _access(.set, from: s)

    // Call is evaluated last.
    let f = Operand.constant(
      FunctionReference(to: AnyDeclID(d), in: &module, specializedBy: a, in: insertionScope!))
    let x0 = _alloc_stack(.void)
    let x1 = _access(.set, from: x0)

    _call(f, [receiver] + arguments, to: x1)
  }

  /// Inserts the IR for given memberwise constructor `call`, which initializes `receiver`.
  ///
  /// - Parameters:
  ///   - call: The syntax of the call.
  ///   - receiver: The address of uninitialized storage typed by the receiver of `d`. This storage
  ///     is borrowed for initialization after evaluating `call`'s arguments.
  private mutating func _emit(
    memberwiseInitializerCall call: FunctionCallExpr.ID, initializing receiver: Operand
  ) {
    let callee = ArrowType(canonical(program[ast[call].callee].type))!

    if callee.inputs.isEmpty {
      _mark_state(.initialized, receiver)
      return
    }

    for i in callee.inputs.indices {
      // TODO: Handle remote types
      let p = ParameterType(callee.inputs[i].type)!
      if p.bareType.base is RemoteType {
        UNIMPLEMENTED()
      }

      let s = _subfield_view(receiver, at: [i])
      emitStore(value: ast[call].arguments[i].value, to: s)
    }
  }

  /// Inserts the IR preparing the run-time arguments passed to `callee` in `call`, lowering
  /// `arguments` and synthesizing default values at `syntheticSite`.
  ///
  /// Information about argument resolution is read from `program.callOperands`. Arguments passed
  /// explicitly have a corresponding expression in `arguments`. If default arguments are used,
  /// `callee` is a name expression referring to a callable declaration.
  private mutating func _emitArguments(
    to callee: AnyExprID, in call: CallID,
    usingExplicit arguments: [LabeledArgument]
  ) -> [Operand] {
    let parameters = (canonical(program[callee].type).base as! CallableType).inputs
    let inputs = program.callOperands[call]!
    assert(parameters.count == inputs.count)

    // Nothing to do if the callee has no parameter.
    if parameters.isEmpty { return [] }

    var result: [Operand] = []
    for i in inputs.indices {
      let p = ParameterType(parameters[i].type)!

      switch inputs[i] {
      case .explicit(let n):
        let a = arguments[n].value
        result.append(_emitArgument(a, to: p))

      case .defaulted:
        let parameterDecls = program.runtimeParameters(of: callee)!
        let a = program[parameterDecls[i]].defaultValue!
        result.append(_emitArgument(a, to: p))

      case .implicit(let d):
        let s = _emitLValue(directReferenceTo: d)
        result.append(_access([p.access], from: s))
      }
    }

    return result
  }

  /// Inserts the IR for the argument `e` passed to a parameter of type `p`
  private mutating func _emitArgument(
    _ e: AnyExprID, to p: ParameterType
  ) -> Operand {
    if p.isAutoclosure {
      return _emitAutoclosureArgument(e, to: p)
    }

    // Make sure arguments to 'set' or 'inout' parameters have a mutation marker, unless it's a
    // literal expression.
    if p.isSetOrInout && !e.isLiteral && !program.ast.isMarkedForMutation(e) {
      report(.error(argumentTo: p.access, requiresMutationMarkerAt: program[e].site))
    }

    if let a = PragmaLiteralExpr.ID(e) {
      return _emitPragmaLiteralArgument(a, to: p)
    }

    let x0 = emitLValue(e)
    let x1 = _unwrapCapture(x0)
    let x2 = _emitCoerce(x1, to: p.bareType)
    return _access([p.access], from: x2)
  }

  /// Inserts the IR for argument `e` passed to an autoclosure parameter of type `p`.
  private mutating func _emitAutoclosureArgument(_ e: AnyExprID, to p: ParameterType) -> Operand {
    // Emit synthesized function declaration.
    let t = ArrowType(p.bareType)!
    let h = Array(t.environment.skolems)
    let f = SynthesizedFunctionDecl(
      .autoclosure(e), typed: t, parameterizedBy: h, in: program[e].scope)
    let callee = withClearContext({ $0.lower(syntheticAutoclosure: f) })

    // Emit the IR code to reference the function declaration.
    let r = FunctionReference(
      to: callee, in: module,
      specializedBy: module.specialization(in: insertionFunction!), in: insertionScope!)

    let x0 = _address_to_pointer(.constant(r))
    let x1 = _alloc_stack(p.bareType)
    _emitInitialize(storage: x1, to: x0)
    return _access([p.access], from: x1)
  }

  /// Inserts the IR for argument `e` passed to a parameter of type `p`
  private mutating func _emitPragmaLiteralArgument(
    _ e: PragmaLiteralExpr.ID, to p: ParameterType
  ) -> Operand {
    let x0 = _alloc_stack(program[e].type)
    _emitStore(e, to: x0)
    return _access([p.access], from: x0)
  }

  /// Inserts the IR generating the operands of the subscript call `e`.
  private mutating func emitOperands(
    _ e: SubscriptCallExpr.ID
  ) -> (callee: BundleReference<SubscriptDecl>, arguments: [Operand]) {
    let arguments = lowering(after: e) {
    // Arguments are evaluated first, from left to right; callee and captures are evaluated next.
      $0._emitArguments(to: $0.ast[e].callee, in: CallID(e), usingExplicit: $0.ast[e].arguments)
    }
    let (callee, captures) = emitSubscriptCallee(ast[e].callee)
    return (callee, captures + arguments)
  }

  /// Inserts the IR for infix operand `e` passed to a parameter of type `p`.
  private mutating func emit(
    infixOperand e: FoldedSequenceExpr, passedTo p: ParameterType
  ) -> Operand {
    lowering(at: ast.site(of: e)) { me in
      switch e {
      case .infix(let f, _, _):
        let t = ArrowType(me.canonical(me.program[f.expr].type))!.lifted
        let s = me._alloc_stack(t.output)
        me.emitStore(e, to: s)
        let u = me._emitCoerce(s, to: p.bareType)
        return me._access([p.access], from: u)

      case .leaf(let e):
        return me._emitArgument(e, to: p)
      }
    }
  }

  /// Emits the IR of a call to `f` with given `arguments`.
  private mutating func _emit(apply f: BuiltinFunction, to arguments: [LabeledArgument]) -> Operand {
    switch f {
    case .addressOf:
      let source = emitLValue(arguments[0].value)
      return _address_to_pointer(source)

    case .markUninitialized:
      let source = emitLValue(arguments[0].value)
      _mark_state(.uninitialized, source)
      return .void

    default:
      var a: [Operand] = []
      for e in arguments {
        let x0 = emitStore(value: e.value)
        let x1 = _access(.sink, from: x0)
        let x2 = _load(x1)
        a.append(x2)
        _end_access(x1)
      }
      return _call_builtin(f, a)
    }
  }

  /// Inserts the IR for given `callee`, which is marked for mutation iff `isMutating` is `true`,
  /// and returns the callee's value along with its lifted arguments.
  ///
  /// Lifted arguments correspond to the captures of the `callee`, which are additional parameters
  /// of the lowered function. Bound member functions have a single lifted argument denoting their
  /// receiver. Local functions with non-empty environments may have many. Lifted arguments are
  /// returned in the same order as the additional parameters in the lowered function.
  ///
  /// - Requires: `callee` has a lambda type.
  private mutating func emitFunctionCallee(
    _ callee: AnyExprID, markedForMutation isMutating: Bool
  ) -> (callee: Callee, captures: [Operand]) {
    switch callee.kind {
    case NameExpr.self:
      return emitNamedFunctionCallee(.init(callee)!, markedForMutation: isMutating)
    case InoutExpr.self:
      return emitFunctionCallee(ast[InoutExpr.ID(callee)!].subject, markedForMutation: true)
    default:
      return (callee: .lambda(emitLambdaCallee(callee)), captures: [])
    }
  }

  /// Inserts the IR for given `callee`, which is marked for mutation iff `isMutating` is `true`,
  /// and returns the callee's value along with its lifted arguments.
  private mutating func emitNamedFunctionCallee(
    _ callee: NameExpr.ID, markedForMutation isMutating: Bool
  ) -> (callee: Callee, captures: [Operand]) {
    switch program[callee].referredDecl {
    case .direct(let d, let a) where d.isCallable:
      // Callee is a direct reference to a lambda declaration.
      guard ArrowType(canonical(program[callee].type))!.environment.isVoid else {
        UNIMPLEMENTED("Generate IR for calls to local functions with captures #1088")
      }
      let f = FunctionReference(to: d, in: &module, specializedBy: a, in: insertionScope!)
      return (.direct(f), [])

    case .member(let d, _, _) where d.isCallable:
      // Callee is a member reference; the receiver is the only capture.
      return emitMemberFunctionCallee(callee, markedForMutation: isMutating)

    case .builtinFunction, .builtinType:
      // Calls to built-ins should have been handled already.
      unreachable()

    default:
      // Callee is a lambda.
      let f = emitLambdaCallee(.init(callee))
      return (.lambda(f), [])
    }
  }

  /// Inserts the IR evaluating `callee`, which refers to a member function, returning the callee's
  /// value along with the call receiver.
  ///
  /// The callee is marked for mutation iff `isMutating` is `true`, in which case the receiver is
  /// accessed with a `set` or `inout` capability.
  private mutating func emitMemberFunctionCallee(
    _ callee: NameExpr.ID, markedForMutation isMutating: Bool
  ) -> (callee: Callee, captures: [Operand]) {
    guard case .member(let d, let a, let s) = program[callee].referredDecl else { unreachable() }

    return lowering(callee) { me in
      let r = me._emitLValue(receiver: s)
      return me._emitMemberFunctionCallee(
        referringTo: d, memberOf: r, markedForMutation: isMutating,
        specializedBy: a, in: me.program[callee].scope)
    }
  }

  /// Inserts the IR constructing the callee of a call referring to `d`, which is a member function
  /// of `r`, returning the callee's value along with the call receiver.
  ///
  /// The callee is marked for mutation iff `isMutating` is `true`, in which case the receiver is
  /// accessed with a `set` or `inout` capability.
  private mutating func _emitMemberFunctionCallee(
    referringTo d: AnyDeclID, memberOf r: Operand, markedForMutation isMutating: Bool,
    specializedBy a: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> (callee: Callee, captures: [Operand]) {
    let available = receiverCapabilities(program[d].type)
    var requested = available.intersection(.forUseOfBundle(performingInPlaceMutation: isMutating))

    // TODO: Should report an error when available is `let|sink` and requested is `inout/set`
    requested = requested.isEmpty ? available : requested

    let entityToCall = module.memberCallee(
      referringTo: d, memberOf: module.type(of: r, in: insertionFunction!).ast, accessedWith: requested,
      specializedBy: a, usedIn: scopeOfUse)

    if case .bundle(let b) = entityToCall {
      return _emitMethodBundleCallee(referringTo: b, on: r)
    } else {
      let c = _access(requested, from: r)
      return (callee: entityToCall, captures: [c])
    }
  }

  /// Returns the value of a reference to `b`, which is bound to `receiver`.
  ///
  /// If `b` requests only one capability, the returned callee is a direct function reference to
  /// the corresponding variant. Otherwise, it is `b` unchanged. The returned capture is an access
  /// on `receiver` taking the weakest capability that `b` requests. This access may be modified
  /// later to take a stronger capability if last use analysis allows it.
  private mutating func _emitMethodBundleCallee(
    referringTo b: BundleReference<MethodDecl>, on receiver: Operand
  ) -> (callee: Callee, captures: [Operand]) {
    if let k = b.capabilities.uniqueElement {
      let d = module.demandDeclaration(lowering: program.ast.implementation(k, of: b.bundle)!)
      let f = FunctionReference(to: d, in: module, specializedBy: b.arguments, in: insertionScope!)
      let c = _access([k], from: receiver)
      return (callee: .direct(f), captures: [c])
    } else {
      let c = _access([b.capabilities.weakest!], from: receiver)
      return (callee: .bundle(b), captures: [c])
    }
  }

  /// Inserts the IR for given `callee` and returns its value.
  ///
  /// - Requires: `callee` has a lambda type.
  private mutating func emitLambdaCallee(_ callee: AnyExprID) -> Operand {
    switch ArrowType(program[callee].type)!.receiverEffect {
    case .yielded:
      unreachable()
    case .set, .sink:
      UNIMPLEMENTED()

    case let k:
      let l = emitLValue(callee)
      return lowering(callee) { $0._access([k], from: l) }
    }
  }

  /// Inserts the IR for given `callee` and returns its value along with its lifted arguments.
  private mutating func emitSubscriptCallee(
    _ callee: AnyExprID
  ) -> (callee: BundleReference<SubscriptDecl>, captures: [Operand]) {
    // TODO: Handle captures
    switch callee.kind {
    case NameExpr.self:
      return emitNamedSubscriptCallee(.init(callee)!)
    case InoutExpr.self:
      return emitSubscriptCallee(ast[InoutExpr.ID(callee)!].subject)
    default:
      UNIMPLEMENTED("call to an anonymous subscript of an rvalue")
    }
  }

  /// Inserts the IR for given `callee` and returns its value along with its lifted arguments.
  private mutating func emitNamedSubscriptCallee(
    _ callee: NameExpr.ID
  ) -> (callee: BundleReference<SubscriptDecl>, captures: [Operand]) {
    switch program[callee].referredDecl {
    case .direct(let d, let a) where d.kind == SubscriptDecl.self:
      // Callee is a direct reference to a subscript declaration.
      guard SubscriptType(canonical(program[d].type))!.environment.isVoid else {
        UNIMPLEMENTED("subscript with non-empty environment")
      }

      let entityToCall = program.subscriptBundleReference(to: .init(d)!, specializedBy: a)
      return (entityToCall, [])

    case .member(let d, _, _) where d.kind == SubscriptDecl.self:
      // Callee is a member reference; the receiver is the only capture.
      return emitMemberSubscriptCallee(callee)

    case .builtinFunction, .builtinType:
      // There are no built-in subscripts.
      unreachable()

    default:
      UNIMPLEMENTED()
    }
  }

  /// Inserts the IR evaluating `callee`, which refers to a member subscript, returning the
  /// callee's value along with the call receiver.
  private mutating func emitMemberSubscriptCallee(
    _ callee: NameExpr.ID
  ) -> (callee: BundleReference<SubscriptDecl>, captures: [Operand]) {
    guard case .member(let d, let a, let s) = program[callee].referredDecl else { unreachable() }

    let entityToCall = program.subscriptBundleReference(to: .init(d)!, specializedBy: a)
    return lowering(callee) { me in
      let r = me._emitLValue(receiver: s)
      let c = me._access(entityToCall.capabilities, from: r)
      return (entityToCall, [c])
    }
  }


  /// Returns `(success: a, failure: b)` where `a` is the basic block reached if all items in
  /// `condition` hold and `b` is the basic block reached otherwise, creating new basic blocks
  /// in `scope`.
  private mutating func emitTest(
    condition: [ConditionItem], in scope: AnyScopeID
  ) -> (success: Block.ID, failure: Block.ID) {
    // Allocate storage for all the declarations in the condition before branching so that all
    // `dealloc_stack` are dominated by their corresponding `alloc_stack`.
    var allocations: [Operand?] = []
    for case .decl(let d) in condition {
      allocations.append(emitAllocation(binding: d))
    }

    let failure = module.appendBlock(in: scope, to: insertionFunction!)
    var nextAllocation = 0
    for item in condition {
      switch item {
      case .expr(let e):
        let test = within(Frame(), { $0.emit(branchCondition: e) })
        let next = appendBlock(in: scope)
        lowering(e) {
          $0._cond_branch(if: test, then: next, else: failure)
        }
        insertionPoint = .end(of: Block.ID(next))

      case .decl(let d):
        let next = emitConditionalNarrowing(
          d, movingConsumedValuesTo: allocations[nextAllocation],
          branchingOnFailureTo: failure, in: scope)
        insertionPoint = .end(of: Block.ID(next))
        nextAllocation += 1
      }
    }

    return (success: insertionBlock!, failure: Block.ID(failure))
  }

  /// If `d` declares stored bindings, inserts the IR for allocating
  /// their storage and returns a the address of that
  /// storage. Otherwise, returns `nil`.
  private mutating func emitAllocation(binding d: BindingDecl.ID) -> Operand? {
    if program[d].pattern.introducer.value.isConsuming {
      lowering(d) { $0._alloc_stack($0.program[d].type) }
    } else {
      nil
    }
  }

  /// Returns a basic block in which the names in `d` have been declared and initialized.
  ///
  /// This method emits IR to:
  /// - evaluate the `d`'s initializer as value *v*,
  /// - check whether the value in *v* is an instance of `d`'s type;
  /// - if it isn't, jump to `failure`;
  /// - if it is, jump to a new basic block and define and initialize the bindings declared in `d`.
  ///
  /// If `d` has a consuming introducer (e.g., `var`), the value of `d`'s initializer is moved to
  /// `storage`, which denotes a memory location with `d`'s type. Otherwise, `storage` is `nil` and
  /// the bindings in `d` are defined as new projections. In either case, the emitter's context is
  /// is updated to associate each binding to its value.
  ///
  /// The return value of the method is a basic block, defined in `scope`. If *v* has the same type
  /// as `d`, the narrowing is irrefutable and `failure` is unreachable in the generated IR.
  private mutating func emitConditionalNarrowing(
    _ d: BindingDecl.ID,
    movingConsumedValuesTo storage: Operand?,
    branchingOnFailureTo failure: Block.AbsoluteID,
    in scope: AnyScopeID
  ) -> Block.AbsoluteID {
    let lhsType = canonical(program[d].type)
    let rhs = emitLValue(ast[d].initializer!)
    let lhs = ast[d].pattern

    assert(program[lhs].introducer.value.isConsuming || (storage == nil))

    if module.type(of: rhs, in: insertionFunction!).ast.base is UnionType {
      return emitUnionNarrowing(
        from: rhs, to: lhs, typed: lhsType,
        movingConsumedValuesTo: storage, branchingOnFailureTo: failure,
        in: scope)
    } else {
      UNIMPLEMENTED()
    }
  }

  /// Returns a basic block in which the names in `lhs` have been declared and initialized.
  ///
  /// - Parameters:
  ///   - rhs: A union container of a type that includes `lhsType`.
  ///   - storage: For a consuming narrowing, the storage of the bindings declared in `lhs`.
  ///   - failure: The basic block to which control flow jumps if the narrowing fails.
  ///   - scope: The scope in which the new basic block is introducked.
  private mutating func emitUnionNarrowing(
    from rhs: Operand, to lhs: BindingPattern.ID, typed lhsType: AnyType,
    movingConsumedValuesTo storage: Operand?,
    branchingOnFailureTo failure: Block.AbsoluteID,
    in scope: AnyScopeID
  ) -> Block.AbsoluteID {
    let rhsType = UnionType(module.type(of: rhs, in: insertionFunction!).ast)!
    precondition(rhsType.elements.contains(lhsType), "recursive narrowing is unimplemented")

    let next = appendBlock(in: scope)
    var targets = UnionSwitch.Targets(
      rhsType.elements.map({ (e) in (key: e, value: failure) }),
      uniquingKeysWith: { (a, _) in a })
    targets[lhsType] = next
    lowering(lhs) { me in
      me._emitUnionSwitch(on: rhs, toOneOf: targets)

      me.insertionPoint = .end(of: Block.ID(next))

      if let target = storage {
        let x0 = me._access(.sink, from: rhs)
        let x1 = me._open_union(x0, as: lhsType)
        me._emitMove(.set, x1, to: target)
        me.emitLocalDeclarations(introducedBy: lhs, referringTo: [], relativeTo: target)
        me._close_union(x1)
        me._end_access(x0)
      } else {
        let k = AccessEffect(me.program[lhs].introducer.value)
        let x0 = me._access([k], from: rhs)
        let x1 = me._open_union(x0, as: lhsType)
        me.assignProjections(of: x1, to: lhs)
      }
    }
    return next
  }

  /// Inserts the IR for branch condition `e`.
  ///
  /// - Requires: `e.type` is `Hylo.Bool`
  private mutating func emit(branchCondition e: AnyExprID) -> Operand {
    precondition(
      program.areEquivalent(program[e].type, ^ast.coreType("Bool")!, in: insertionScope!))
    let wrapper = emitLValue(e)
    return lowering(e) { $0._emitLoadBuiltinBool(wrapper) }
  }

  /// Inserts the IR for extracting the built-in value stored in an instance of `Hylo.Bool`.
  private mutating func _emitLoadBuiltinBool(_ wrapper: Operand) -> Operand {
    precondition(module.type(of: wrapper, in: insertionFunction!) == .address(ast.coreType("Bool")!))
    let x0 = _subfield_view(wrapper, at: [0])
    let x1 = _access(.sink, from: x0)
    let x2 = _load(x1)
    _end_access(x1)
    return x2
  }

  /// If `s` has a remote type, returns the result of an instruction exposing the captured access;
  /// otherwise, returns `s`.
  private mutating func _unwrapCapture(_ s: Operand) -> Operand {
    if module.type(of: s, in: insertionFunction!).ast.base is RemoteType {
      return _open_capture(s)
    } else {
      return s
    }
  }

  /// Inserts the IR for coercing `source` to an address of type `target`.
  ///
  /// `source` is returned unchanged if it stores an instance of `target`. Otherwise, the IR
  /// producing an address of type `target` is inserted, consuming `source` if necessary.
  private mutating func _emitCoerce(_ source: Operand, to target: AnyType) -> Operand {
    let lhs = module.type(of: source, in: insertionFunction!).ast
    let rhs = program.canonical(target, in: insertionScope!)

    if program.areEquivalent(lhs, rhs, in: insertionScope!) {
      return source
    }

    if lhs.base is RemoteType {
      let s = _open_capture(source)
      return _emitCoerce(s, to: rhs)
    }

    switch rhs.base {
    case let t as ExistentialType:
      return _emitCoerce(source, to: t)
    case let t as ArrowType:
      return _emitCoerce(source, to: t)
    case let t as UnionType:
      return _emitCoerce(source, to: t)
    default:
      unexpectedCoercion(from: lhs, to: rhs)
    }
  }

  /// Inserts the IR for coercing `source` to an address of type `target`.
  ///
  /// - Requires: `target` is canonical.
  private mutating func _emitCoerce(
    _ source: Operand, to target: ExistentialType
  ) -> Operand {
    let t = module.type(of: source, in: insertionFunction!).ast
    if t.base is ExistentialType {
      return source
    }

    return _emitExistential(target, wrapping: source)
  }

  /// Inserts the IR for coercing `source` to an address of type `target`.
  ///
  /// - Requires: `target` is canonical.
  private mutating func _emitCoerce(
    _ source: Operand, to target: ArrowType
  ) -> Operand {
    let t = module.type(of: source, in: insertionFunction!).ast
    guard let lhs = ArrowType(t) else {
      unexpectedCoercion(from: t, to: ^target)
    }

    // TODO: Handle variance
    let rhs = ArrowType(
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
    _ source: Operand, to target: UnionType
  ) -> Operand {
    let lhs = module.type(of: source, in: insertionFunction!).ast

    let x0 = _alloc_stack(^target)
    let x1 = _open_union(x0, as: lhs, .forInitialization)
    _emitMove(.set, source, to: x1)
    _close_union(x1)
    return x0
  }

  /// Traps on this execution path because of un unexpected coercion from `lhs` to `rhs`.
  private func unexpectedCoercion(
    from lhs: AnyType, to rhs: AnyType, file: StaticString = #file, line: UInt = #line
  ) -> Never {
    fatalError("unexpected coercion from '\(lhs)' to '\(rhs)'", file: file, line: line)
  }

  /// Inserts the IR for converting `foreign` to a value of type `ir`.
  private mutating func _emitConvert(foreign: Operand, to ir: AnyType) -> Operand {
    precondition(module.type(of: foreign, in: insertionFunction!).isObject)

    let foreignConvertible = ast.core.foreignConvertible.type
    let foreignConvertibleConformance = program.conformance(
      of: ir, to: foreignConvertible, exposedTo: insertionScope!)!
    let r = ast.requirements(
      Name(stem: "init", labels: ["foreign_value"]), in: foreignConvertible.decl)[0]

    // TODO: Handle cases where the foreign representation of `t` is not built-in.

    // Store the foreign representation in memory to call the converter.
    let source = _alloc_stack(module.type(of: foreign, in: insertionFunction!).ast)
    _emitInitialize(storage: source, to: foreign)

    switch foreignConvertibleConformance.implementations[r]! {
    case .explicit(let m):
      let convert = module.demandDeclaration(lowering: m)!
      let f = module.reference(to: convert, implementedFor: foreignConvertibleConformance)

      let x0 = _alloc_stack(ir)
      let x1 = _access(.set, from: x0)
      let x2 = _alloc_stack(ArrowType(f.type.ast)!.output)
      let x3 = _access(.set, from: x2)
      let x4 = _access(.sink, from: source)

      _call(.constant(f), [x1, x4], to: x3)

      _end_access(x4)
      _end_access(x3)
      _end_access(x1)
      return x0

    case .synthetic:
      UNIMPLEMENTED()
    }
  }

  /// Appends the IR to convert `o` to a FFI argument.
  ///
  /// The returned operand is the result of a `load` instruction.
  private mutating func _emitConvertToForeign(_ o: Operand) -> Operand {
    let t = module.type(of: o, in: insertionFunction!)
    precondition(t.isAddress)

    let foreignConvertible = ast.core.foreignConvertible.type
    let foreignConvertibleConformance = program.conformance(
      of: t.ast, to: foreignConvertible, exposedTo: insertionScope!)!
    let r = ast.requirements("foreign_value", in: foreignConvertible.decl)[0]

    // TODO: Handle cases where the foreign representation of `t` is not built-in.

    switch foreignConvertibleConformance.implementations[r]! {
    case .explicit(let m):
      let convert = module.demandDeclaration(lowering: m)!
      let f = module.reference(to: convert, implementedFor: foreignConvertibleConformance)

      let x0 = _access(.let, from: o)
      let x1 = _alloc_stack(ArrowType(f.type.ast)!.output)
      let x2 = _access(.set, from: x1)
      _call(.constant(f), [x0], to: x2)
      _end_access(x2)
      _end_access(x0)

      let x3 = _access(.sink, from: x1)
      let x4 = _load(x3)
      _end_access(x3)
      return x4

    case .synthetic:
      UNIMPLEMENTED()
    }
  }

  /// Returns an existential container of type `t` wrappring `witness`.
  private mutating func _emitExistential(
    _ t: ExistentialType, wrapping witness: Operand
  ) -> Operand {
    let w = module.type(of: witness, in: insertionFunction!).ast
    let table = Operand.constant(module.demandWitnessTable(w, in: insertionScope!))
    return _wrap_existential_addr(witness, table, as: t)
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
    case BooleanLiteralExpr.self:
      return emitStore(value: ast[e].left)
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
    let t = RemoteType(MetatypeType(canonical(program[e].right.type))!.instance)!
    let x0 = emitLValue(ast[e].left)

    return lowering(e) { me in
      let x1 = me._access(.sink, from: x0)
      let x2 = me._load(x1)
      me._end_access(x1)
      return me._pointer_to_address(x2, as: t)
    }
  }

  /// Inserts the IR for lvalue `e`.
  private mutating func emitLValue(_ e: InoutExpr.ID) -> Operand {
    emitLValue(ast[e].subject)
  }

  /// Inserts the IR for lvalue `e`.
  private mutating func emitLValue(_ e: NameExpr.ID) -> Operand {
    lowering(e) {
      $0._emitLValue(reference: $0.program[e].referredDecl)
    }
  }

  /// Inserts the IR for lvalue `e`.
  private mutating func emitLValue(_ e: SubscriptCallExpr.ID) -> Operand {
    let (b, a) = emitOperands(e)
    return lowering(e) {
      $0._project_bundle(applying: b, to: a)
    }
  }

  /// Inserts the IR for lvalue `e`.
  private mutating func emitLValue(_ e: TupleMemberExpr.ID) -> Operand {
    let base = emitLValue(ast[e].tuple)
    let i = ast[e].index
    return lowering(i) {
      $0._subfield_view(base, at: [i.value])
    }
  }

  /// Inserts the IR for `r` used a lvalue.
  private mutating func _emitLValue(reference r: DeclReference) -> Operand {
    switch r {
    case .direct(let d, _):
      return _emitLValue(directReferenceTo: d)

    case .member(let d, let a, let s):
      let receiver = _emitLValue(receiver: s)
      return _emitProperty(boundTo: receiver, declaredBy: d, specializedBy: a)

    case .constructor:
      UNIMPLEMENTED()

    case .builtinModule, .builtinFunction, .builtinType, .compilerKnownType:
      // Built-in symbols and compiler-known types are never used as l-value.
      unreachable()
    }
  }

  /// Returns the address of the `operation`'s receiver, which refers to a member declaration.
  private mutating func _emitLValue(receiver r: DeclReference.Receiver) -> Operand {
    switch r {
    case .operand, .implicit:
      unreachable()
    case .explicit(let e):
      return emitLValue(e)
    case .elided(let s):
      return _emitLValue(reference: s)
    }
  }

  /// Inserts the IR denoting a direct reference to `d`.
  private mutating func _emitLValue(
    directReferenceTo d: AnyDeclID
  ) -> Operand {
    // Handle local bindings.
    if let s = frames[d] {
      return s
    }

    // Handle references to type declarations.
    if let t = MetatypeType(program[d].type) {
      let s = _alloc_stack(^t)
      _emitInitialize(storage: s, to: .constant(t))
      return s
    }

    assert(program.isGlobal(d), "unhandled local declaration")

    switch d.kind {
    case GenericParameterDecl.self:
      return _generic_parameter(at: .init(d)!)

    case VarDecl.self:
      let (root, subfied) = program.subfieldRelativeToRoot(of: .init(d)!)
      let s = _global_addr(at: root)
      return _subfield_view(s, at: subfied)

    default:
      unexpected(d, in: program.ast)
    }
  }

  /// Inserts the IR to return the address of the member declared by `d`, bound to `r`, and
  /// specialized by `z`.
  private mutating func _emitProperty(
    boundTo r: Operand, declaredBy d: AnyDeclID, specializedBy z: GenericArguments
  ) -> Operand {
    switch d.kind {
    case SubscriptDecl.self:
      return _emitComputedProperty(
        boundTo: r, declaredByBundle: .init(d)!, specializedBy: z)

    case VarDecl.self:
      let l = AbstractTypeLayout(of: module.type(of: r, in: insertionFunction!).ast, definedIn: program)
      let i = l.offset(of: ast[VarDecl.ID(d)!].baseName)!
      return _subfield_view(r, at: [i])

    default:
      UNIMPLEMENTED()
    }
  }

  /// Returns the projection the property declared by `d`, bound to `r`, and specialized by `z`.
  private mutating func _emitComputedProperty(
    boundTo r: Operand, declaredByBundle d: SubscriptDecl.ID, specializedBy z: GenericArguments
  ) -> Operand {
    if let i = ast[d].impls.uniqueElement {
      return _emitComputedProperty(boundTo: r, declaredBy: i, specializedBy: z)
    }

    let t = SubscriptType(canonicalType(of: d, specializedBy: z))!
    let b = BundleReference(to: d, specializedBy: z, requesting: t.capabilities)
    let a = _access(t.capabilities, from: r)

    return _project_bundle(applying: b, to: [a])
  }

  /// Returns the projection of the property declared by `d`, bound to `r`, and specialized by `z`.
  private mutating func _emitComputedProperty(
    boundTo r: Operand, declaredBy d: SubscriptImpl.ID, specializedBy z: GenericArguments
  ) -> Operand {
    let t = SubscriptImplType(canonicalType(of: d, specializedBy: z))!
    let o = RemoteType(ast[d].introducer.value, t.output)
    let a = _access([o.access], from: r)
    let f = module.demandDeclaration(lowering: d)

    return _project(o, applying: f, specializedBy: z, to: [a])
  }

  // MARK: Move

  /// Replaces `i`, which is a `move` instruction with move-assignment of `semantics == .inout` or
  /// move-initialization if `semantics == .set`, returning the identity of the first instruction
  /// taking the place of `i`.
  ///
  /// After the call, `insertionPoint` set to `nil`.
  mutating func replaceMove(_ i: InstructionID, in f: Function.ID, with semantics: AccessEffect) -> InstructionID {
    let s = module[i, in: f] as! Move
    insertionFunction = f
    let predecessor = module.instruction(before: i, in: f)

    insertionPoint = .before(i)
    lowering(at: s.site) {
      $0._emitMove(semantics, s.object, to: s.target, withMovableConformance: s.movable)
    }
    module.removeInstruction(i, in: f)

    if let p = predecessor {
      return module.instruction(after: p, in: f)!
    } else {
      let b = insertionBlock!
      return .init(b.address, module[b, in: f].instructions.firstAddress!)
    }
  }

  /// Inserts IR for move-initializing/assigning `storage` with `value`.
  ///
  /// The value of `semantics` defines the type of move to emit:
  /// - `.set` emits move-initialization.
  /// - `.inout` emits move-assignment.
  /// - `[.inout, .set]` emits a `move` instruction that will is later replaced during definite
  ///   initialization analysis by either move-assignment if `storage` is found initialized or
  ///   by move-initialization otherwise.
  private mutating func _emitMove(
    _ semantics: AccessEffectSet, _ value: Operand, to storage: Operand
  ) {
    precondition(!semantics.isEmpty && semantics.isSubset(of: [.set, .inout]))
    let model = module.type(of: value, in: insertionFunction!).ast
    precondition(model == module.type(of: storage, in: insertionFunction!).ast)

    // Built-in types are handled as a special case.
    if model.isBuiltin {
      _emitMoveBuiltIn(value, to: storage)
      return
    }

    // Other types must be movable.
    let m = program.ast.core.movable.type
    guard let movable = program.conformance(of: model, to: m, exposedTo: insertionScope!) else {
      report(.error(model, doesNotConformTo: m, at: currentSource))
      return
    }

    // Use memcpy of `source` is trivially movable.
    if program.isTrivial(movable) {
      let x0 = _access(.sink, from: value)
      let x1 = _access(.set, from: storage)
      _memory_copy(x0, x1)
      _end_access(x1)
      _mark_state(.uninitialized, x0)
      _end_access(x0)
      return
    }

    // Insert a call to the appropriate move implementation if its semantics is unambiguous.
    // Otherwise, insert a call to the method bundle.
    if let k = semantics.uniqueElement {
      _emitMove(k, value, to: storage, withMovableConformance: movable)
    } else {
      _move(value, storage, via: movable)
    }
  }

  /// Inserts IR for move-initializing/assigning `storage` with built-in `value`.
  private mutating func _emitMoveBuiltIn(_ value: Operand, to storage: Operand) {
    // Built-in are always stored.
    let x0 = _access(.set, from: storage)
    let x1 = _access(.sink, from: value)
    let x2 = _load(x1)
    _store(x2, x0)
    _end_access(x1)
    _end_access(x0)
  }

  /// Inserts IR for move-initializing/assigning `storage` with `value` using `movable`
  /// to locate the implementations of these operations.
  ///
  /// The value of `semantics` defines the type of move to emit:
  /// - `.set` emits move-initialization.
  /// - `.inout` emits move-assignment.
  ///
  /// - Requires: `storage` does not have a built-in type.
  private mutating func _emitMove(
    _ semantics: AccessEffect, _ value: Operand, to storage: Operand,
    withMovableConformance movable: FrontEnd.Conformance
  ) {
    let d = module.demandTakeValueDeclaration(semantics, definedBy: movable)
    let f = module.reference(to: d, implementedFor: movable)

    let x0 = _alloc_stack(.void)
    let x1 = _access(.set, from: x0)
    let x2 = _access([semantics], from: storage)
    let x3 = _access(.sink, from: value)
    _call(.constant(f), [x2, x3], to: x1)
    _end_access(x3)
    _end_access(x2)
    _end_access(x1)
    _dealloc_stack(x0)
  }

  // MARK: Copy

  /// Inserts IR for copying `source` to `target`.
  private mutating func _emitCopy(
    _ source: Operand, to target: Operand
  ) {
    let model = module.type(of: source, in: insertionFunction!).ast
    precondition(model == module.type(of: target, in: insertionFunction!).ast)

    // Built-in types are handled as a special case.
    if model.isBuiltin {
      _emitMoveBuiltIn(source, to: target)
      return
    }

    // Other types must be copyable.
    guard
      let copyable = program.conformance(
        of: model, to: program.ast.core.copyable.type, exposedTo: insertionScope!)
    else { preconditionFailure("expected '\(model)' to be 'Copyable'") }
    _emitCopy(source, to: target, withCopyableConformance: copyable)
  }

  /// Inserts IR for copying `source` to `target` using `copyable` to locate the
  /// implementation of the copy operation.
  private mutating func _emitCopy(
    _ source: Operand, to target: Operand,
    withCopyableConformance copyable: FrontEnd.Conformance
  ) {
    let d = module.demandCopyDeclaration(definedBy: copyable)
    let f = module.reference(to: d, implementedFor: copyable)

    let x0 = _access(.let, from: source)
    let x1 = _access(.set, from: target)
    _call(.constant(f), [x0], to: x1)
    _end_access(x1)
    _end_access(x0)
  }

  // MARK: Deinitialization

  /// If `storage` is deinitializable in `self.insertionScope`, inserts the IR for deinitializing
  /// it, or reports a diagnostic otherwise.
  ///
  /// Let `T` be the type of `storage`, `storage` is deinitializable iff `T` has a deinitializer
  /// exposed to `self.insertionScope`.
  mutating func _emitDeinit(_ storage: Operand) {
    let m = module.type(of: storage, in: insertionFunction!).ast
    let d = program.ast.core.deinitializable.type

    if m.base is RemoteType {
      _mark_state(.uninitialized, storage)
    } else if let c = program.conformance(of: m, to: d, exposedTo: insertionScope!) {
      if program.isTrivial(c) {
        _mark_state(.uninitialized, storage)
      } else {
        _emitDeinit(storage, via: c)
      }
    } else if m.isBuiltinOrRawTuple {
      _mark_state(.uninitialized, storage)
    } else {
      report(.error(m, doesNotConformTo: d, at: currentSource))
    }
  }

  /// Inserts the IR for deinitializing `storage` using the `Deinitializable` conformance `c`.
  private mutating func _emitDeinit(
    _ storage: Operand, via c: FrontEnd.Conformance
  ) {
    let d = module.demandDeinitDeclaration(from: c)
    let f = module.reference(to: d, implementedFor: c)

    let x0 = _alloc_stack(.void)
    let x1 = _access(.set, from: x0)
    let x2 = _access(.sink, from: storage)
    _call(.constant(f), [x2], to: x1)
    _end_access(x2)
    _end_access(x1)
    _mark_state(.uninitialized, x0)
    _dealloc_stack(x0)
  }

  /// If `storage` is deinitializable in `self.insertionScope`, inserts the IR for deinitializing
  /// it; reports a diagnostic for each part that isn't deinitializable otherwise.
  mutating func _emitDeinitParts(of storage: Operand) {
    let t = module.type(of: storage, in: insertionFunction!).ast

    if program.isTriviallyDeinitializable(t, in: insertionScope!) {
      _mark_state(.uninitialized, storage)
    } else if t.base is UnionType {
      _emitDeinitUnionPayload(of: storage)
    } else if t.hasRecordLayout {
      _emitDeinitRecordParts(of: storage)
    } else {
      report(.error(t, doesNotConformTo: ast.core.deinitializable.type, at: currentSource))
    }
  }

  /// If `storage`, which stores a record, is deinitializable in `self.insertionScope`, inserts
  /// the IR for deinitializing it; reports a diagnostic for each part that isn't
  /// deinitializable otherwise.
  ///
  /// - Requires: the type of `storage` has a record layout.
  private mutating func _emitDeinitRecordParts(of storage: Operand) {
    let t = module.type(of: storage, in: insertionFunction!).ast
    precondition(t.hasRecordLayout)

    let layout = AbstractTypeLayout(of: t, definedIn: module.program)

    // If the object is empty, simply mark it uninitialized.
    if layout.properties.isEmpty {
      _mark_state(.uninitialized, storage)
      return
    }

    // Otherwise, deinitialize each property.
    for i in layout.properties.indices {
      let x0 = _subfield_view(storage, at: [i])
      _emitDeinit(x0)
    }
  }

  /// If `storage`, which stores a union, is deinitializable in `self.insertionScope`, inserts
  /// the IR for deinitializing it; reports a diagnostic for each part that isn't
  /// deinitializable otherwise.
  ///
  /// - Requires: the type of `storage` is a union.
  private mutating func _emitDeinitUnionPayload(of storage: Operand) {
    let t = UnionType(module.type(of: storage, in: insertionFunction!).ast)!

    // If union is empty, simply mark it uninitialized.
    if t.elements.isEmpty {
      _mark_state(.uninitialized, storage)
      return
    }

    // Trivial if the union has a single member.
    if let e = t.elements.uniqueElement {
      _emitDeinitUnionPayload(of: storage, containing: e)
      return
    }

    // One successor per member in the union, ordered by their mangled representation.
    let targets = UnionSwitch.Targets(
      t.elements.map({ (e) in (key: e, value: appendBlock()) }),
      uniquingKeysWith: { (a, _) in a })
    _emitUnionSwitch(on: storage, toOneOf: targets)

    let tail = appendBlock()
    for (u, b) in targets {
      insertionPoint = .end(of: Block.ID(b))
      _emitDeinitUnionPayload(of: storage, containing: u)
      _branch(to: tail)
    }

    insertionPoint = .end(of: Block.ID(tail))
  }

  /// If `storage`, which stores a union container holding a `payload`, is deinitializable in
  /// `self.insertionScope`, inserts the IR for deinitializing it; reports a diagnostic for each
  /// part that isn't deinitializable otherwise.
  private mutating func _emitDeinitUnionPayload(of storage: Operand, containing payload: AnyType) {
    let x0 = _open_union(storage, as: payload)
    _emitDeinit(x0)
    _close_union(x0)
  }

  // MARK: Equality

  private mutating func _emitStoreEquality(_ lhs: Operand, _ rhs: Operand, to target: Operand) {
    let m = module.type(of: lhs, in: insertionFunction!).ast
    let d = program.ast.core.equatable.type

    if let equatable = program.conformance(of: m, to: d, exposedTo: insertionScope!) {
      let d = module.demandEqualDeclaration(definedBy: equatable)
      let f = module.reference(to: d, implementedFor: equatable)

      let x0 = _access(.set, from: target)
      let x1 = _access(.let, from: lhs)
      let x2 = _access(.let, from: rhs)
      _call(.constant(f), [x1, x2], to: x0)
      _end_access(x2)
      _end_access(x1)
      _end_access(x0)
    } else {
      report(.error(m, doesNotConformTo: d, at: currentSource))
    }
  }

  /// Inserts the IR writing in `target` whether the parts of `lhs` and `rhs` are pairwise equal.
  private mutating func _emitStorePartsEquality(
    _ lhs: Operand, _ rhs: Operand,
    to target: Operand
  ) {
    let layout = AbstractTypeLayout(
      of: module.type(of: lhs, in: insertionFunction!).ast, definedIn: module.program)

    // If the object is empty, return true.
    var parts = layout.properties[...]
    if parts.isEmpty {
      _emitStore(boolean: true, to: target)
      return
    }

    // Otherwise, compare all parts pairwise.
    let tail = appendBlock()
    while !parts.isEmpty {
      let x0 = _subfield_view(lhs, at: [parts.startIndex])
      let x1 = _subfield_view(rhs, at: [parts.startIndex])
      _emitStoreEquality(x0, x1, to: target)

      parts = parts.dropFirst()
      if parts.isEmpty {
        _branch(to: tail)
        insertionPoint = .end(of: Block.ID(tail))
      } else {
        let x2 = _emitLoadBuiltinBool(target)
        let next = appendBlock()
        _cond_branch(if: x2, then: next, else: tail)
        insertionPoint = .end(of: Block.ID(next))
      }
    }
  }

  /// Inserts the IR writing in `target` whether the payloads of `lhs` and `rhs` are equal.
  private mutating func _emitStoreUnionPayloadEquality(
    _ lhs: Operand, _ rhs: Operand, to target: Operand
  ) {
    let union = UnionType(module.type(of: lhs, in: insertionFunction!).ast)!

    // If the union is empty, return true.
    if union.elements.isEmpty {
      _emitStore(boolean: true, to: target)
      return
    }

    // Otherwise, compare their payloads.
    let same = appendBlock()
    let targets = UnionSwitch.Targets(
      union.elements.map({ (e) in (key: e, value: appendBlock()) }),
      uniquingKeysWith: { (a, _) in a })
    let fail = appendBlock()
    let tail = appendBlock()

    // The success blocks compare discriminators and then payloads.
    let dl = _emitUnionDiscriminator(lhs)
    let dr = _emitUnionDiscriminator(rhs)
    let x0 = _call_builtin(.icmp(.eq, .discriminator), [dl, dr])
    _cond_branch(if: x0, then: same, else: fail)

    insertionPoint = .end(of: Block.ID(same))
    _emitUnionSwitch(on: lhs, toOneOf: targets)
    for (u, b) in targets {
      insertionPoint = .end(of: Block.ID(b))
      let y0 = _open_union(lhs, as: u)
      let y1 = _open_union(rhs, as: u)
      _emitStoreEquality(y0, y1, to: target)
      _close_union(y1)
      _close_union(y0)
      _branch(to: tail)
    }

    // The failure block writes `false` to the return storage.
    insertionPoint = .end(of: Block.ID(fail))
    _emitStore(boolean: false, to: target)
    _branch(to: tail)

    // The tail block represents the continuation.
    insertionPoint = .end(of: Block.ID(tail))
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
    case let t as ArrowType:
      return [RemoteType(t.captures[0].type)?.access ?? .sink]
    case let t as MethodType:
      return t.capabilities
    default:
      unreachable()
    }
  }

  /// Inserts a stack allocation for an object of type `t`.
  private mutating func _alloc_stack(_ t: AnyType) -> Operand {
    // This is a temporary hack to deal with the fact that later passes come
    // back and start emitting code in the middle of a block without
    // having set up a record of frames and allocations.  When we recompute
    // allocations as on-demand, that will become a non-issue.
    if frames.isEmpty { frames.push() }
    let s = insert(module.makeAllocStack(canonical(t), at: currentSource))!
    frames.top.allocs.append((source: s, mayHoldCaptures: false))
    return s
  }

  /// Inserts the IR for deallocating each allocation in the top frame of `self.frames`.
  private mutating func _emitDeallocTopFrame() {
    for a in frames.top.allocs.reversed() {
      if a.mayHoldCaptures {
        _release_capture(a.source)
      }
      _dealloc_stack(a.source)
    }
    assert(frames.top.allocs.isEmpty)
  }

  private mutating func _dealloc_stack(_ source: Operand) {
    let a = frames.top.allocs.popLast()!
    precondition(
      a.source == source,
      "dealloc_stack(\(source)) doesn't match last allocated \(a.source)")
    if a.mayHoldCaptures {
      _release_capture(a.source)
    }
    insert(module.makeDeallocStack(for: source, in: insertionFunction!, at: currentSource))
  }

  /// Appends the IR for computing the address of the given `subfield` of the record at
  /// `recordAddress` and returns the resulting address.
  mutating func _subfield_view(
    _ recordAddress: Operand, at subfield: RecordPath
  ) -> Operand {
    if subfield.isEmpty { return recordAddress }

    if let r = module[recordAddress] as? SubfieldView {
      let p = r.subfield + subfield
      let s = module.makeSubfieldView(of: r.recordAddress, subfield: p, in: insertionFunction!, at: currentSource)
      return insert(s)!
    } else {
      let s = module.makeSubfieldView(of: recordAddress, subfield: subfield, in: insertionFunction!, at: currentSource)
      return insert(s)!
    }
  }

  /// Emits the IR for copying the union discriminator of `container`, which is the address of
  /// a union container.
  private mutating func _emitUnionDiscriminator(
    _ container: Operand
  ) -> Operand {
    let x0 = _access(.let, from: container)
    let x1 = _union_discriminator(x0)
    _end_access(x0)
    return x1
  }

  /// Appends the IR for jumping to the block assigned to the type of `scrutinee`'s payload in
  /// `targets`.
  private mutating func _emitUnionSwitch(
    on scrutinee: Operand, toOneOf targets: UnionSwitch.Targets
  ) {
    let u = UnionType(module.type(of: scrutinee, in: insertionFunction!).ast)!
    let i = _emitUnionDiscriminator(scrutinee)
    _union_switch(case: i, of: u, targets)
  }

  /// Returns the result of calling `action` on `self` within `newFrame`.
  ///
  /// `newFrame` is pushed on `self.frames` before `action` is called. When `action` returns,
  /// outstanding stack allocations are deallocated and `newFrame` is popped. References to stack
  /// memory allocated by `action` are invalidated when this method returns.
  private mutating func within<T>(_ newFrame: Frame, _ action: (inout Self) -> T) -> T {
    frames.push(newFrame)
    defer {
      let savedSource = currentSource

      lowering(after: insertionScope!) {
        $0._emitDeallocTopFrame()
      }
      frames.pop()
      currentSource = savedSource
    }
    return action(&self)
  }

  /// Returns the result of calling `action` on a copy of `self` whose insertion block and frames
  /// are clear.
  private mutating func withClearContext<T>(_ action: (inout Self) throws -> T) rethrows -> T {
    var ff: Function.ID? = nil
    var p: InsertionPoint? = nil
    var f = Stack()
    var l = LoopIDs()
    var s = program[module.id].site

    swap(&ff, &insertionFunction)
    swap(&p, &insertionPoint)
    swap(&f, &frames)
    swap(&l, &loops)
    swap(&s, &currentSource)
    defer {
      swap(&ff, &insertionFunction)
      swap(&p, &insertionPoint)
      swap(&f, &frames)
      swap(&l, &loops)
      swap(&s, &currentSource)
    }
    return try action(&self)
  }

  /// Inserts a `cond_branch` instruction that jumps to `targetIfTrue` if `condition` is true or `targetIfFalse` otherwise.
  mutating func _cond_branch(if condition: Operand, then targetIfTrue: Block.AbsoluteID, else targetIfFalse: Block.AbsoluteID) {
    checkEntryStack(targetIfTrue)
    checkEntryStack(targetIfFalse)
    insert(module.makeCondBranch(if: condition, then: targetIfTrue, else: targetIfFalse, in: insertionFunction!, at: currentSource))
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

    /// Returns `true` iff `other` describes the same stack of allocations.
    func hasSameAllocations(as other: Self) -> Bool {
      allocs.elementsEqual(other.allocs) { $0.source == $1.source }
    }
  }

  /// A stack of frames.
  fileprivate struct Stack {

    /// Returns `true` iff `other` describes the same number of frames
    /// having the same stacks of allocations.
    func hasSameAllocations(as s: Stack) -> Bool {
      elements.elementsEqual(s.elements) {
        $0.hasSameAllocations(as: $1)
      }
    }

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

  /// If the state of the stack upon entering `b` is not known,
  /// records it as `frames`; otherwise asserts that it has the same
  /// allocations as `frames`.
  ///
  /// This test is used to ensure that all points branching to a block
  /// have consistent stack allocations.
  fileprivate mutating func checkEntryStack(_ b: Block.AbsoluteID) {
    modify(&stackOnEntry[b]) { x in
      if let y = x {
        assert(y.hasSameAllocations(as: frames))
      }
      else {
        x = frames
      }
    }
  }

}

extension Diagnostic {

  fileprivate static func error(
    argumentTo a: AccessEffect, requiresMutationMarkerAt site: SourceRange
  ) -> Diagnostic {
    .error("argument to '\(a)' parameter must be marked for mutation", at: site)
  }

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
    .error("statement will never be executed", at: .empty(at: ast[s].site.start))
  }

}

extension Emitter {

  /// Calls `body(&self)` with `currentSource` set to the lexical
  /// location of `x`, returning the result.
  mutating func lowering<ASTNode: NodeIDProtocol, R>(_ x: ASTNode, _ body: (inout Emitter)->R) -> R {
    lowering(at: ast[x].site, body)
  }

  /// Calls `body(&self)` with `currentSource` set to the lexical
  /// location of `x`, returning the result.
  mutating func lowering<T, R>(_ x: SourceRepresentable<T>, _ body: (inout Emitter)->R) -> R {
    lowering(at: x.site, body)
  }

  /// Calls `body(&self)` with `currentSource` set to the lexical
  /// beginning of `x`, returning the result.
  mutating func lowering<ID: NodeIDProtocol, R>(before x: ID, _ body: (inout Emitter)->R) -> R {
    lowering(at: .empty(at: ast[x].site.start), body)
  }

  /// Calls `body(&self)` with `currentSource` set to the lexical
  /// end of `x`, returning the result.
  mutating func lowering<ID: NodeIDProtocol, R>(after x: ID, _ body: (inout Emitter)->R) -> R {
    lowering(at: .empty(at: ast[x].site.end), body)
  }

  /// Calls `body(&self)` with `currentSource` set to `x`, returning
  /// the result.
  mutating func lowering<R>(at x: SourceRange, _ body: (inout Emitter)->R) -> R {
    let savedSite = currentSource
    defer { currentSource = savedSite }
    currentSource = x
    return body(&self)
  }

  /// The conditions described by the `mark_state` instruction.
  fileprivate enum InitializationState {
    /// The operand is initialized.
    case initialized

    /// The operand is not initialized.
    case uninitialized
  }

  /// The ways of opening described by the `open_union` instruction.
  fileprivate enum OpenUnionOption {
    /// The input operand is uninitialized.
    case forInitialization

    /// The input operand is already initialized.
    case notForInitialization
  }

}

/// Instruction inserters.
extension Emitter {

  fileprivate mutating func _mark_state(_ x: InitializationState, _ op: Operand?) {
    insert(module.makeMarkState(op!, initialized: x == .initialized, in: insertionFunction!, at: currentSource))
  }

  fileprivate mutating func _call_ffi<T: TypeProtocol>(_ foreignName: String, on arguments: [Operand], returning returnType: T) -> Operand {
    insert(
      module.makeCallFFI(
        returning: .object(returnType), applying: foreignName, to: arguments, in: insertionFunction!, at: currentSource))!
  }

  fileprivate mutating func _unreachable() {
    insert(module.makeUnreachable(at: currentSource))
  }

  fileprivate mutating func _return() {
    insert(module.makeReturn(at: currentSource))
  }

  fileprivate mutating func _access(
    _ capabilities: AccessEffectSet, from s: Operand, correspondingTo binding: VarDecl.ID? = nil
  ) -> Operand {
    insert(module.makeAccess(capabilities, from: s, correspondingTo: binding, in: insertionFunction!, at: currentSource))!
  }

  fileprivate mutating func _end_access(_ x: Operand) {
    insert(module.makeEndAccess(x, in: insertionFunction!, at: currentSource))
  }

  fileprivate mutating func _yield(_ c: AccessEffect, _ a: Operand) {
    _ = insert(module.makeYield(c, a, in: insertionFunction!, at: currentSource))
  }

  fileprivate mutating func _branch(to x: Block.AbsoluteID) {
    checkEntryStack(x)
    _ = insert(module.makeBranch(to: x, at: currentSource))
  }

  fileprivate mutating func _open_union(
    _ container: Operand, as payload: AnyType,
    _ option: OpenUnionOption = .notForInitialization
  ) -> Operand {
    insert(
      module.makeOpenUnion(
        container, as: payload, forInitialization: option == .forInitialization, in: insertionFunction!, at: currentSource))!
  }

  fileprivate mutating func _close_union(_ x: Operand  ) {
    insert(module.makeCloseUnion(x, in: insertionFunction!, at: currentSource))
  }

  fileprivate mutating func _project(
    _ t: RemoteType, applying s: Function.ID, specializedBy z: GenericArguments,
    to arguments: [Operand]
  ) -> Operand {
    insert(
      module.makeProject(t, applying: s, specializedBy: z, to: arguments, at: currentSource))!
  }

  fileprivate mutating func _store(_ source: Operand, _ target: Operand) {
    insert(module.makeStore(source, at: target, in: insertionFunction!, at: currentSource))
  }

  /// Inserts a `load` instruction reading from `source`.
  fileprivate mutating func _load(_ source: Operand) -> Operand {
    insert(module.makeLoad(source, in: insertionFunction!, at: currentSource))!
  }


  fileprivate mutating func _address_to_pointer(_ source: Operand) -> Operand {
    insert(module.makeAddressToPointer(source, at: currentSource))!
  }

  fileprivate mutating func _call_builtin(
    _ f: BuiltinFunction, _ arguments: [Operand]
  ) -> Operand {
    insert(module.makeCallBuiltin(applying: f, to: arguments, in: insertionFunction!, at: currentSource))!
  }

  fileprivate mutating func _project_bundle(
    applying b: BundleReference<SubscriptDecl>, to arguments: [Operand]
  ) -> Operand {
    insert(
      module.makeProjectBundle(
        applying: b, to: arguments, at: currentSource,
        canonicalizingTypesIn: insertionScope!))!
  }

  fileprivate mutating func _open_capture(_ s: Operand) -> Operand {
    insert(module.makeOpenCapture(s, in: insertionFunction!, at: currentSource))!
  }

  fileprivate mutating func _release_capture(_ source: Operand) {
    insert(module.makeReleaseCapture(source, in: insertionFunction!, at: currentSource))
  }

  fileprivate mutating func _advanced(_ source: Operand, byStrides n: Int) -> Operand {
    insert(module.makeAdvanced(source, byStrides: n, in: insertionFunction!, at: currentSource))!
  }

  fileprivate mutating func _constant_string(utf8 value: Data) -> Operand {
    insert(module.makeConstantString(utf8: value, at: currentSource))!
  }

  fileprivate mutating func _capture(_ source: Operand, in target: Operand) {
    insert(module.makeCapture(source, in: target, in: insertionFunction!, at: currentSource))
  }

  fileprivate mutating func _call(
    _ callee: Operand, _ arguments: [Operand], to output: Operand
  ) {
    insert(module.makeCall(applying: callee, to: arguments, writingResultTo: output, in: insertionFunction!, at: currentSource))
  }

  fileprivate mutating func _call_bundle(
    _ m: BundleReference<MethodDecl>, _ a: [Operand],
    to o: Operand,
    scopeOfUse: AnyScopeID
  ) {
    insert(module.makeCallBundle(
      applying: m, to: a, writingResultTo: o, in: insertionFunction!, at: currentSource,
      canonicalizingTypesIn: scopeOfUse))
  }

  fileprivate mutating func _wrap_existential_addr(
    _ witness: Operand, _ table: Operand, as interface: ExistentialType
  ) -> Operand {
    insert(module.makeWrapExistentialAddr(witness, table, as: interface, in: insertionFunction!, at: currentSource))!
  }

  fileprivate mutating func _pointer_to_address(_ x: Operand, as t: RemoteType) -> Operand {
    insert(module.makePointerToAddress(x, to: t, at: currentSource))!
  }

  fileprivate mutating func _generic_parameter(at x: GenericParameterDecl.ID) -> Operand {
    insert(module.makeGenericParameter(passedTo: x, at: currentSource))!
  }

  fileprivate mutating func _global_addr(at x: BindingDecl.ID) -> Operand {
    insert(module.makeGlobalAddr(of: x, at: currentSource))!
  }

  fileprivate mutating func _memory_copy(_ source: Operand, _ target: Operand) {
    insert(module.makeMemoryCopy(source, target, in: insertionFunction!, at: currentSource))
  }

  fileprivate mutating func _move(_ source: Operand, _ target: Operand, via movability: FrontEnd.Conformance) {
    insert(module.makeMove(source, to: target, usingConformance: movability, in: insertionFunction!, at: currentSource))
  }

  fileprivate mutating func _union_discriminator(_ x: Operand) -> Operand {
    insert(module.makeUnionDiscriminator(x, in: insertionFunction!, at: currentSource))!
  }

  fileprivate mutating func _union_switch(case discriminator: Operand, of u: UnionType, _ targets: UnionSwitch.Targets) {
    insert(module.makeUnionSwitch(over: discriminator, of: u, toOneOf: targets, in: insertionFunction!, at: currentSource))
  }

}
