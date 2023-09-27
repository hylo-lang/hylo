import Core
import IR
import Foundation
import LLVM
import Utils

extension LLVM.Module {

  /// Creates the LLVM transpilation of the Hylo IR module `m` in `ir`.
  init(transpiling m: ModuleDecl.ID, from ir: IR.Program) {
    let source = ir.modules[m]!
    self.init(source.name)

    for g in source.globals.indices {
      incorporate(g, of: source, from: ir)
    }
    for f in source.functions.keys {
      incorporate(f, of: source, from: ir)
    }
  }

  /// Transpiles and incorporates `g`, which is a function of `m` in `ir`.
  mutating func incorporate(_ g: IR.Module.GlobalID, of m: IR.Module, from ir: IR.Program) {
    let v = transpiledConstant(m.globals[g], usedIn: m, from: ir)
    let d = declareGlobalVariable("\(m.id)\(g)", v.type)
    setInitializer(v, for: d)
    setLinkage(.private, for: d)
    setGlobalConstant(true, for: d)
  }

  /// Transpiles and incorporates `f`, which is a function or subscript of `m` in `ir`.
  mutating func incorporate(_ f: IR.Function.ID, of m: IR.Module, from ir: IR.Program) {
    // Don't transpile generic functions.
    if m[f].isGeneric {
      return
    }

    if m[f].isSubscript {
      let d = declareSubscript(transpiledFrom: f, of: m, from: ir)
      transpile(contentsOf: f, of: m, from: ir, into: d)
    } else {
      let d = declareFunction(transpiledFrom: f, of: m, from: ir)
      transpile(contentsOf: f, of: m, from: ir, into: d)
      if f == m.entryFunction {
        defineMain(calling: f, of: m, from: ir)
      }
    }
  }

  private mutating func defineMain(
    calling f: IR.Function.ID,
    of m: IR.Module,
    from ir: IR.Program
  ) {
    let main = declareFunction("main", FunctionType(from: [], to: i32, in: &self))

    let b = appendBlock(to: main)
    let p = endOf(b)

    let transpilation = function(named: ir.base.mangled(f))!

    let val32 = ir.ast.coreType("Int32")!
    switch m[f].output {
    case val32:
      let t = StructType(ir.llvm(val32, in: &self))!
      let s = insertAlloca(t, at: p)
      _ = insertCall(transpilation, on: [s], at: p)

      let statusPointer = insertGetStructElementPointer(of: s, typed: t, index: 0, at: p)
      let status = insertLoad(i32, from: statusPointer, at: p)
      insertReturn(status, at: p)

    default:
      let t = ir.llvm(AnyType.void, in: &self)
      let s = insertAlloca(t, at: p)
      _ = insertCall(transpilation, on: [s], at: p)
      insertReturn(i32.zero, at: p)
    }
  }

  /// Returns the LLVM type of a metatype instance.
  private mutating func metatypeType() -> LLVM.StructType {
    if let t = type(named: "_val_metatype") {
      return .init(t)!
    }

    let fields: [LLVM.IRType] = [
      word(),  // size
      word(),  // alignment
      ptr,  // representation
    ]
    return LLVM.StructType(fields, in: &self)
  }

  /// Returns the LLVM type of an existential container.
  private mutating func containerType() -> LLVM.StructType {
    if let t = type(named: "_val_container") {
      return .init(t)!
    }
    return StructType([ptr, ptr], in: &self)
  }

  /// Returns the prototype of subscript slides.
  private mutating func slidePrototype() -> LLVM.Function {
    if let f = function(named: "_val_slide") {
      return f
    }

    let f = declareFunction(
      "_val_slide",
      FunctionType(from: [ptr, i1], to: void, in: &self))
    addAttribute(named: .zeroext, to: f.parameters[1])

    return f
  }

  /// Returns the declaration of `malloc`.
  private mutating func mallocPrototype() -> LLVM.Function {
    if let f = function(named: "malloc") {
      return f
    }

    let f = declareFunction(
      "malloc",
      FunctionType(from: [i32], to: ptr, in: &self))
    addAttribute(named: .noundef, to: f.parameters[0])
    addAttribute(named: .noalias, to: f.returnValue)

    return f
  }

  /// Returns the declaration of `free`.
  private mutating func freePrototype() -> LLVM.Function {
    if let f = function(named: "free") {
      return f
    }

    let f = declareFunction(
      "free",
      FunctionType(from: [ptr], to: void, in: &self))
    addAttribute(named: .noundef, to: f.parameters[0])

    return f
  }

  /// Returns the type of a transpiled function whose type in Hylo is `t`.
  ///
  /// - Note: the type of a function in Hylo IR typically doesn't match the type of its transpiled
  ///   form 1-to-1, as return values are often passed by references.
  private mutating func transpiledType(_ t: LambdaType) -> LLVM.FunctionType {
    // Return value is passed by reference.
    var parameters: Int = t.inputs.count + 1

    // Environment is passed before explicit arguments.
    if t.environment != .void {
      parameters += t.captures.count
    }

    return .init(from: Array(repeating: ptr, count: parameters), to: void, in: &self)
  }

  /// Returns the LLVM IR value corresponding to the Hylo IR constant `c` when used in `m` in `ir`.
  private mutating func transpiledConstant(
    _ c: any IR.Constant,
    usedIn m: IR.Module,
    from ir: IR.Program
  ) -> LLVM.IRValue {
    switch c {
    case let v as IR.IntegerConstant:
      guard v.value.bitWidth <= 64 else { UNIMPLEMENTED() }
      let t = LLVM.IntegerType(v.value.bitWidth, in: &self)
      return t.constant(v.value.words[0])

    case let v as IR.FloatingPointConstant:
      let t = LLVM.FloatingPointType(ir.llvm(c.type.ast, in: &self))!
      return t.constant(parsing: v.value)

    case let v as IR.WitnessTable:
      return transpiledWitnessTable(v, usedIn: m, from: ir)

    case let v as IR.PointerConstant:
      return global(named: "\(v.container)\(v.id)")!

    case let v as IR.FunctionReference:
      return declare(v, from: ir)

    case let v as MetatypeType:
      return transpiledMetatype(of: v.instance, usedIn: m, from: ir)

    case let v as TraitType:
      return transpiledTrait(v, usedIn: m, from: ir)

    case is IR.VoidConstant:
      return LLVM.StructConstant(aggregating: [], in: &self)

    default:
      unreachable()
    }
  }

  /// Returns the LLVM IR value of the witness table `t` used in `m` in `ir`.
  private mutating func transpiledWitnessTable(
    _ t: WitnessTable,
    usedIn m: IR.Module,
    from ir: IR.Program
  ) -> LLVM.IRValue {
    // A witness table is composed of a header, a trait map, and a (possibly empty) sequence of
    // implementation maps. All parts are laid out inline without any padding.
    //
    // The header consists of a pointer to the witness it describes and the number of traits to
    // which the witness conforms.
    //
    // The trait map is a sequence of pairs `(t, d)` where `t` is a pointer to a trait identifier
    // and `d` the offset of the associated implementation map. The last element of the trait map
    // is a sentinel `(nullptr, end)` where `end` is the "past-the-end" position of the last
    // implementation map. An implementation map is a sequence of pairs `(r, i)` where `r` is a
    // trait requirement identifier and `i` is a pointer to its implementation.

    // Encode the table's header.
    var tableContents: [LLVM.IRValue] = [
      transpiledMetatype(of: t.witness, usedIn: m, from: ir),
      word().constant(t.conformances.count),
    ]

    // Encode the table's trait and implementation maps.
    var entries: [LLVM.IRValue] = []
    var implementations: [LLVM.IRValue] = []
    for c in t.conformances {
      let entry: [LLVM.IRValue] = [
        transpiledTrait(c.concept, usedIn: m, from: ir),
        word().constant(implementations.count),
      ]
      entries.append(LLVM.StructConstant(aggregating: entry, in: &self))

      for (r, d) in c.implementations {
        let requirement: [LLVM.IRValue] = [
          word().constant(r.rawValue),
          transpiledRequirementImplementation(d, from: ir),
        ]
        implementations.append(LLVM.StructConstant(aggregating: requirement, in: &self))
      }
    }

    // Append the sentinel at the end of the trait map.
    entries.append(
      LLVM.StructConstant(
        aggregating: [ptr.null, word().constant(UInt64(implementations.count))], in: &self))

    // Put everything together.
    tableContents.append(
      LLVM.ArrayConstant(
        of: LLVM.StructType([ptr, word()], in: &self), containing: entries, in: &self))

    if !implementations.isEmpty {
      tableContents.append(
        LLVM.ArrayConstant(
          of: LLVM.StructType([word(), ptr], in: &self), containing: implementations, in: &self))
    }

    let table = LLVM.StructConstant(aggregating: tableContents, in: &self)

    let g = declareGlobalVariable(ir.base.mangled(t), table.type)
    setInitializer(table, for: g)
    setLinkage(.linkOnce, for: g)
    setGlobalConstant(true, for: g)
    return g
  }

  /// Returns the LLVM IR value of the requirement implementation `i`, which is in `ir`.
  private mutating func transpiledRequirementImplementation(
    _ i: IR.Conformance.Implementation, from ir: IR.Program
  ) -> LLVM.Function {
    switch i {
    case .function(let f):
      return declare(f, from: ir)
    case .value:
      UNIMPLEMENTED()
    }
  }

  /// Returns the LLVM IR value of the metatype `t` used in `m` in `ir`.
  private mutating func transpiledMetatype(
    of t: AnyType, usedIn m: IR.Module, from ir: IR.Program
  ) -> LLVM.GlobalVariable {
    demandMetatype(of: t, usedIn: m, from: ir) { (me, v) in
      if let u = ProductType(t) {
        me.initializeTranspiledProductTypeMetatype(v, of: u, usedIn: m, from: ir)
      } else {
        me.initializeTranspiledMetatype(v, of: t, usedIn: m, from: ir)
      }
    }
  }

  /// Initializes `instance` with the value of the metatype of `t` used in `m` in `ir`.
  private mutating func initializeTranspiledMetatype<T: TypeProtocol>(
    _ instance: LLVM.GlobalVariable,
    of t: T, usedIn m: IR.Module, from ir: IR.Program
  ) {
    setLinkage(.linkOnce, for: instance)

    let layout = memoryLayout(of: t, from: ir)
    let v = LLVM.StructType(instance.valueType)!.constant(
      aggregating: [layout.size, layout.preferredAlignment, ptr.null],
      in: &self)

    setInitializer(v, for: instance)
    setGlobalConstant(true, for: instance)
  }

  /// Initializes `instance` with the value of the metatype of `t` used in `m` in `ir`.
  private mutating func initializeTranspiledProductTypeMetatype(
    _ instance: LLVM.GlobalVariable,
    of t: ProductType, usedIn m: IR.Module, from ir: IR.Program
  ) {
    // Initialize the instance if it's being used in the module defining `t`. Otherwise, simply let
    // the symbol be linked to its definition later.
    if m.id != ir.base.module(containing: t.decl) { return }

    // If `t` is generic, its metatype is only a stub.
    let layout: LLVMMemoryLayout
    if !ir.base[t.decl].genericParameters.isEmpty {
      layout = .init(size: word().zero, preferredAlignment: word().zero)
    } else {
      layout = memoryLayout(of: t, from: ir)
    }

    let v = LLVM.StructType(instance.valueType)!.constant(
      aggregating: [layout.size, layout.preferredAlignment, ptr.null],
      in: &self)

    setInitializer(v, for: instance)
    setGlobalConstant(true, for: instance)
  }

  private mutating func demandMetatype<T: TypeProtocol>(
    of t: T, usedIn m: IR.Module, from ir: IR.Program,
    initializedWith initializeInstance: (inout Self, LLVM.GlobalVariable) -> Void
  ) -> LLVM.GlobalVariable{
    let globalName = ir.base.mangled(t)
    if let g = global(named: globalName) { return g }

    let metatype = metatypeType()
    let instance = declareGlobalVariable(globalName, metatype)
    initializeInstance(&self, instance)
    return instance
  }

  /// Returns the memory layout of `t`, which is a canonical type in `ir`.
  ///
  /// - Requires: `t` is representable in LLVM.
  private mutating func memoryLayout<T: TypeProtocol>(
    of t: T, from ir: IR.Program
  ) -> LLVMMemoryLayout {
    let u = ir.llvm(t, in: &self)
    return .init(
      size: word().constant(layout.storageSize(of: u)),
      preferredAlignment: word().constant(layout.preferredAlignment(of: u)))
  }

  /// Returns the LLVM IR value of `t` used in `m` in `ir`.
  private mutating func transpiledTrait(
    _ t: TraitType, usedIn m: IR.Module, from ir: IR.Program
  ) -> LLVM.GlobalVariable {
    // Check if we already created the trait's instance.
    let globalName = ir.base.mangled(t)
    if let g = global(named: globalName) {
      return g
    }

    // Initialize the instance if it's being used in the module defining `t`. Otherwise, simply
    // declare the symbol and let it be linked later.
    let instance = declareGlobalVariable(globalName, ptr)
    if m.id != ir.base.module(containing: t.decl) {
      return instance
    }

    let s = LLVM.StringConstant(globalName, nullTerminated: true, in: &self)
    let g = addGlobalVariable("str", s.type)
    setInitializer(s, for: g)
    setLinkage(.private, for: g)
    setGlobalConstant(true, for: g)

    setInitializer(g, for: instance)
    setGlobalConstant(true, for: instance)
    return instance
  }

  /// Inserts and returns the transpiled declaration of `ref`, which is in `ir`.
  private mutating func declare(
    _ ref: IR.FunctionReference, from ir: IR.Program
  ) -> LLVM.Function {
    let t = transpiledType(LambdaType(ref.type.ast)!)
    return declareFunction(ir.base.mangled(ref.function), t)
  }

  /// Inserts and returns the transpiled declaration of `f`, which is a function of `m` in `ir`.
  private mutating func declareFunction(
    transpiledFrom f: IR.Function.ID, of m: IR.Module, from ir: IR.Program
  ) -> LLVM.Function {
    precondition(!m[f].isSubscript)

    // Parameters and return values are passed by reference.
    let parameters = Array(repeating: ptr as LLVM.IRType, count: m[f].inputs.count + 1)
    let transpilation = declareFunction(ir.base.mangled(f), .init(from: parameters, in: &self))

    configureAttributes(transpilation, transpiledFrom: f, of: m)
    configureInputAttributes(transpilation.parameters.dropLast(), transpiledFrom: f, in: m)

    return transpilation
  }

  /// Inserts and returns the transpiled declaration of `f`, which is a subscript of `m` in `ir`.
  private mutating func declareSubscript(
    transpiledFrom f: IR.Function.ID, of m: IR.Module, from ir: IR.Program
  ) -> LLVM.Function {
    precondition(m[f].isSubscript)

    // Parameters are a buffer for the subscript frame followed by its declared parameters. Return
    // type is a pair `(c, p)` where `c` points to a subscript slide and `p` is the address of the
    // projected value.
    let r = LLVM.StructType([ptr, ptr], in: &self)
    let parameters = Array(repeating: ptr as LLVM.IRType, count: m[f].inputs.count + 1)
    let transpilation = declareFunction(
      ir.base.mangled(f), .init(from: parameters, to: r, in: &self))

    configureAttributes(transpilation, transpiledFrom: f, of: m)
    configureInputAttributes(transpilation.parameters.dropFirst(), transpiledFrom: f, in: m)

    return transpilation
  }

  /// Adds to `llvmFunction` the attributes implied by its IR form `f`, which is in `m`.
  private mutating func configureAttributes(
    _ llvmFunction: LLVM.Function, transpiledFrom f: IR.Function.ID, of m: IR.Module
  ) {
    // FIXME: See #888
    // switch m[f].linkage {
    // case .external:
    //   setLinkage(.external, for: llvmFunction)
    // case .module:
    //   setLinkage(.private, for: llvmFunction)
    // }

    // Monomorphized functions always have private linkage.
    if f.isMonomorphized {
      setLinkage(.private, for: llvmFunction)
    }

    // Functions that return `Never` have the `noreturn` attribute.
    if !m[f].isSubscript && (m[f].output == .never) {
      addAttribute(.init(.noreturn, in: &self), to: llvmFunction)
    }
  }

  /// Adds to each parameter in `llvmParameters` the attributes implied by its corresponding IR
  /// form in `m[f].inputs`.
  private mutating func configureInputAttributes(
    _ llvmParameters: LLVM.Function.Parameters.SubSequence,
    transpiledFrom f: IR.Function.ID, in m: IR.Module
  ) {
    assert(llvmParameters.count == m[f].inputs.count)
    for (p, l) in llvmParameters.enumerated() {
      configureInputAttributes(l, transpiledFrom: p, in: f, in: m)
    }
  }

  /// Adds to `llvmParameter` the attributes implied by its IR form in `m[f].inputs[p]`.
  private mutating func configureInputAttributes(
    _ llvmParameter: LLVM.Parameter,
    transpiledFrom p: Int, in f: IR.Function.ID, in m: IR.Module
  ) {
    addAttribute(named: .noalias, to: llvmParameter)
    addAttribute(named: .nofree, to: llvmParameter)
    addAttribute(named: .nonnull, to: llvmParameter)
    addAttribute(named: .noundef, to: llvmParameter)

    if !m[f].isSubscript {
      addAttribute(named: .nocapture, to: llvmParameter)
    }

    if m[f].inputs[p].type.access == .let {
      addAttribute(named: .readonly, to: llvmParameter)
    }
  }

  /// Inserts into `transpilation `the transpiled contents of `f`, which is a function or subscript
  /// of `m` in `ir`.
  private mutating func transpile(
    contentsOf f: IR.Function.ID,
    of m: IR.Module,
    from ir: IR.Program,
    into transpilation: LLVM.Function
  ) {
    /// The function's entry.
    guard let entry = m[f].entry else { return }

    /// Where new LLVM IR instruction are inserted.
    var insertionPoint: LLVM.InsertionPoint!

    /// A map from Hylo IR basic block to its LLVM counterpart.
    var block: [IR.Block.ID: LLVM.BasicBlock] = [:]

    /// A map from Hylo IR register to its LLVM counterpart.
    var register: [IR.Operand: LLVM.IRValue] = [:]

    /// A map from projection to its side results in LLVM.
    ///
    /// Projection calls is transpiled as coroutine calls, producing a slide and a frame pointer in
    /// addition to the projected value. These values are stored here so that `register` can be a
    /// one-to-one mapping from Hylo registers to LLVM registers.
    var byproduct: [IR.InstructionID: (slide: LLVM.IRValue, frame: LLVM.IRValue)] = [:]

    /// The address of the function's frame if `f` is a subscript. Otherwise, `nil`.
    let frame: LLVM.IRValue?

    /// The prologue of the transpiled function, which contains its stack allocations.
    let prologue = appendBlock(named: "prologue", to: transpilation)

    // In subscripts, parameters are laid out after the frame buffer.
    let parameterOffset: Int
    if m[f].isSubscript {
      parameterOffset = 1
      frame = insertSubscriptPrologue(into: transpilation)
    } else {
      parameterOffset = 0
      frame = nil
    }

    for i in m[m.entry(of: f)!].inputs.indices {
      let o = Operand.parameter(.init(f, entry), i)
      let s = transpilation.parameters[parameterOffset + i]
      register[o] = s
    }

    for b in m.blocks(in: f) {
      block[b] = appendBlock(named: b.description, to: transpilation)
    }

    for b in m.blocks(in: f) {
      insertionPoint = endOf(block[b]!)
      for i in m.instructions(in: b) {
        insert(i)
      }
    }

    insertBr(to: block[.init(f, entry)]!, at: endOf(prologue))

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(_ i: IR.InstructionID) {
      switch m[i] {
      case is IR.AddressToPointer:
        insert(addressToPointer: i)
      case is IR.AdvancedByBytes:
        insert(advancedByBytes: i)
      case is IR.AllocStack:
        insert(allocStack: i)
      case is IR.Access:
        insert(access: i)
      case is IR.Branch:
        insert(branch: i)
      case is IR.Call:
        insert(call: i)
      case is IR.CallFFI:
        insert(callFFI: i)
      case is IR.CaptureIn:
        insert(captureIn: i)
      case is IR.CloseCapture:
        return
      case is IR.CloseUnion:
        insert(closeUnion: i)
      case is IR.CondBranch:
        insert(condBranch: i)
      case is IR.ConstantString:
        insert(constantString: i)
      case is IR.DeallocStack:
        return
      case is IR.EndAccess:
        return
      case is IR.EndProject:
        insert(endProjection: i)
      case is IR.GlobalAddr:
        insert(globalAddr: i)
      case is IR.LLVMInstruction:
        insert(llvm: i)
      case is IR.Load:
        insert(load: i)
      case is IR.MarkState:
        return
      case is IR.OpenCapture:
        insert(openCapture: i)
      case is IR.OpenUnion:
        insert(openUnion: i)
      case is IR.PointerToAddress:
        insert(pointerToAddress: i)
      case is IR.Project:
        insert(project: i)
      case is IR.ReleaseCaptures:
        return
      case is IR.Return:
        insert(return: i)
      case is IR.Store:
        insert(store: i)
      case is IR.SubfieldView:
        insert(subfieldView: i)
      case is IR.Switch:
        insert(switch: i)
      case is IR.UnionDiscriminator:
        insert(unionDiscriminator: i)
      case is IR.Unreachable:
        insert(unreachable: i)
      case is IR.WrapExistentialAddr:
        insert(wrapAddr: i)
      case is IR.Yield:
        insert(yield: i)
      default:
        UNIMPLEMENTED()
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(addressToPointer i: IR.InstructionID) {
      let s = m[i] as! AddressToPointer
      register[.register(i)] = llvm(s.source)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(advancedByBytes i: IR.InstructionID) {
      let s = m[i] as! AdvancedByBytes

      let base = llvm(s.base)
      let v = insertGetElementPointerInBounds(
        of: base, typed: ptr, indices: [llvm(s.byteOffset)], at: insertionPoint)
      register[.register(i)] = v
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(allocStack i: IR.InstructionID) {
      let s = m[i] as! AllocStack
      let t = ir.llvm(s.allocatedType, in: &self)
      register[.register(i)] = insertAlloca(t, atEntryOf: transpilation)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(access i: IR.InstructionID) {
      let s = m[i] as! Access
      register[.register(i)] = llvm(s.source)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(branch i: IR.InstructionID) {
      let s = m[i] as! Branch
      insertBr(to: block[s.target]!, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(call i: IR.InstructionID) {
      let s = m[i] as! Call
      var arguments: [LLVM.IRValue] = []

      // Callee is evaluated first; environment is passed before explicit arguments.
      let callee = unpackCallee(of: s.callee)
      arguments.append(contentsOf: callee.environment)

      // Arguments and return value are passed by reference.
      arguments.append(contentsOf: s.arguments.map(llvm(_:)))
      arguments.append(llvm(s.output))
      _ = insertCall(callee.function, typed: callee.type, on: arguments, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(callFFI i: IR.InstructionID) {
      let s = m[i] as! CallFFI
      let parameters = s.operands.map({ ir.llvm(m.type(of: $0).ast, in: &self) })

      let returnType: LLVM.IRType
      if s.returnType.ast.isVoidOrNever {
        returnType = void
      } else {
        returnType = ir.llvm(s.returnType.ast, in: &self)
      }

      let callee = declareFunction(s.callee, .init(from: parameters, to: returnType, in: &self))
      let arguments = s.operands.map({ llvm($0) })
      register[.register(i)] = insertCall(callee, on: arguments, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(captureIn i: IR.InstructionID) {
      let s = m[i] as! CaptureIn
      insertStore(llvm(s.source), to: llvm(s.target), at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(closeUnion i: IR.InstructionID) {
      let s = m[i] as! CloseUnion
      let open = m[s.start.instruction!] as! OpenUnion

      // TODO: Memoize somehow
      let t = UnionType(m.type(of: open.container).ast)!
      let e = m.program.discriminatorToElement(in: t)
      let n = e.firstIndex(of: open.payloadType)!

      let baseType = ir.llvm(unionType: t, in: &self)
      let container = llvm(open.container)
      let indices = [i32.constant(0), i32.constant(1)]
      let discriminator = insertGetElementPointerInBounds(
        of: container, typed: baseType, indices: indices, at: insertionPoint)
      insertStore(word().constant(UInt64(n)), to: discriminator, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(constantString i: IR.InstructionID) {
      let s = m[i] as! ConstantString
      let v = LLVM.ArrayConstant(bytes: s.value, in: &self)
      let d = declareGlobalVariable(UUID().uuidString, v.type)
      setInitializer(v, for: d)
      setLinkage(.private, for: d)
      setGlobalConstant(true, for: d)
      register[.register(i)] = d
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(condBranch i: IR.InstructionID) {
      let s = m[i] as! CondBranch
      let c = llvm(s.condition)
      insertCondBr(
        if: c, then: block[s.targetIfTrue]!, else: block[s.targetIfFalse]!,
        at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(endProjection i: IR.InstructionID) {
      let s = m[i] as! EndProject
      let start = s.start.instruction!
      assert(m[start] is Project)

      let t = LLVM.FunctionType(from: [ptr, i1], to: void, in: &self)
      let p = byproduct[start]!
      _ = insertCall(p.slide, typed: t, on: [p.frame, i1.zero], at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(globalAddr i: IR.InstructionID) {
      let s = m[i] as! IR.GlobalAddr
      register[.register(i)] = global(named: "\(s.container)\(s.id)")!
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(subfieldView i: IR.InstructionID) {
      let s = m[i] as! SubfieldView

      let base = llvm(s.recordAddress)
      let baseType = ir.llvm(m.type(of: s.recordAddress).ast, in: &self)
      let indices = [i32.constant(0)] + s.subfield.map({ i32.constant(UInt64($0)) })
      let v = insertGetElementPointerInBounds(
        of: base, typed: baseType, indices: indices, at: insertionPoint)
      register[.register(i)] = v
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(llvm i: IR.InstructionID) {
      let s = m[i] as! IR.LLVMInstruction
      switch s.instruction {
      case .add(let p, _):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertAdd(overflow: p, l, r, at: insertionPoint)

      case .sub(let p, _):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertSub(overflow: p, l, r, at: insertionPoint)

      case .mul(let p, _):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertMul(overflow: p, l, r, at: insertionPoint)

      case .shl:
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertShl(l, r, at: insertionPoint)

      case .lshr:
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertLShr(l, r, at: insertionPoint)

      case .sdiv(let e, _):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertSignedDiv(exact: e, l, r, at: insertionPoint)

      case .udiv(let e, _):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertUnsignedDiv(exact: e, l, r, at: insertionPoint)

      case .srem(_):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertSignedRem(l, r, at: insertionPoint)

      case .signedAdditionWithOverflow(let t):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        let f = intrinsic(
          named: Intrinsic.llvm.sadd.with.overflow, for: [ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(LLVM.Function(f)!, on: [l, r], at: insertionPoint)

      case .unsignedAdditionWithOverflow(let t):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        let f = intrinsic(
          named: Intrinsic.llvm.uadd.with.overflow, for: [ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(LLVM.Function(f)!, on: [l, r], at: insertionPoint)

      case .signedSubtractionWithOverflow(let t):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        let f = intrinsic(
          named: Intrinsic.llvm.ssub.with.overflow, for: [ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(LLVM.Function(f)!, on: [l, r], at: insertionPoint)

      case .unsignedSubtractionWithOverflow(let t):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        let f = intrinsic(
          named: Intrinsic.llvm.usub.with.overflow, for: [ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(LLVM.Function(f)!, on: [l, r], at: insertionPoint)

      case .signedMultiplicationWithOverflow(let t):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        let f = intrinsic(
          named: Intrinsic.llvm.smul.with.overflow, for: [ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(LLVM.Function(f)!, on: [l, r], at: insertionPoint)

      case .unsignedMultiplicationWithOverflow(let t):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        let f = intrinsic(
          named: Intrinsic.llvm.umul.with.overflow, for: [ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(LLVM.Function(f)!, on: [l, r], at: insertionPoint)

      case .icmp(let p, _):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertIntegerComparison(p, l, r, at: insertionPoint)

      case .and(_):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertBitwiseAnd(l, r, at: insertionPoint)

      case .or(_):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertBitwiseOr(l, r, at: insertionPoint)

      case .xor(_):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertBitwiseXor(l, r, at: insertionPoint)

      case .trunc(_, let t):
        let target = ir.llvm(builtinType: t, in: &self)
        let source = llvm(s.operands[0])
        register[.register(i)] = insertTrunc(source, to: target, at: insertionPoint)

      case .inttoptr(_):
        let source = llvm(s.operands[0])
        register[.register(i)] = insertIntToPtr(source, at: insertionPoint)

      case .ptrtoint(let t):
        let target = ir.llvm(builtinType: t, in: &self)
        let source = llvm(s.operands[0])
        register[.register(i)] = insertPtrToInt(source, to: target, at: insertionPoint)

      case .fadd:
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertFAdd(l, r, at: insertionPoint)

      case .fsub:
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertFSub(l, r, at: insertionPoint)

      case .fmul:
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertFMul(l, r, at: insertionPoint)

      case .fdiv:
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertFDiv(l, r, at: insertionPoint)

      case .frem:
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertFRem(l, r, at: insertionPoint)

      case .fcmp(_, let p, _):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertFloatingPointComparison(p, l, r, at: insertionPoint)

      case .fptrunc(_, let t):
        let target = ir.llvm(builtinType: t, in: &self)
        let source = llvm(s.operands[0])
        register[.register(i)] = insertFPTrunc(source, to: target, at: insertionPoint)

      case .zeroinitializer(let t):
        register[.register(i)] = ir.llvm(builtinType: t, in: &self).null

      case .advancedByBytes:
        let base = llvm(s.operands[0])
        let byteOffset = llvm(s.operands[1])
        register[.register(i)] = insertGetElementPointerInBounds(
          of: base, typed: i8, indices: [byteOffset], at: insertionPoint)

      default:
        unreachable("unexpected LLVM instruction '\(s.instruction)'")
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(load i: IR.InstructionID) {
      let s = m[i] as! Load
      let t = ir.llvm(s.objectType.ast, in: &self)
      let source = llvm(s.source)
      register[.register(i)] = insertLoad(t, from: source, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(openCapture i: IR.InstructionID) {
      let s = m[i] as! OpenCapture
      register[.register(i)] = insertLoad(ptr, from: llvm(s.source), at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(openUnion i: IR.InstructionID) {
      let s = m[i] as! OpenUnion
      let t = UnionType(m.type(of: s.container).ast)!

      let baseType = ir.llvm(unionType: t, in: &self)
      let container = llvm(s.container)
      let indices = [i32.constant(0), i32.constant(0)]
      register[.register(i)] = insertGetElementPointerInBounds(
        of: container, typed: baseType, indices: indices, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(pointerToAddress i: IR.InstructionID) {
      let s = m[i] as! IR.PointerToAddress
      register[.register(i)] = llvm(s.source)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(project i: IR.InstructionID) {
      let s = m[i] as! IR.Project

      // %0 = alloca [8 x i8], align 8
      let buffer = LLVM.ArrayType(8, i8, in: &self)
      let x0 = insertAlloca(buffer, at: insertionPoint)
      setAlignment(8, for: x0)

      // All arguments are passed by reference.
      var arguments: [LLVM.IRValue] = [x0]
      for a in s.operands {
        if m.type(of: a).isObject {
          let t = ir.llvm(s.result!.ast, in: &self)
          let l = insertAlloca(t, atEntryOf: transpilation)
          insertStore(llvm(a), to: l, at: insertionPoint)
          arguments.append(l)
        } else {
          arguments.append(llvm(a))
        }
      }

      // %1 = call ptr @llvm.coro.prepare.retcon(ptr @s)
      let f = declareSubscript(transpiledFrom: s.callee, of: m, from: ir)
      let prepare = intrinsic(named: Intrinsic.llvm.coro.prepare.retcon)!
      let x1 = insertCall(LLVM.Function(prepare)!, on: [f], at: insertionPoint)

      // %2 = call {ptr, ptr} %1(...)
      let x2 = insertCall(x1, typed: f.valueType, on: arguments, at: insertionPoint)

      register[.register(i)] = insertExtractValue(from: x2, at: 1, at: insertionPoint)
      byproduct[i] = (slide: insertExtractValue(from: x2, at: 0, at: insertionPoint), frame: x0)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(return i: IR.InstructionID) {
      if m[f].isSubscript {
        _ = insertCall(
          LLVM.Function(intrinsic(named: Intrinsic.llvm.coro.end)!)!,
          on: [frame!, i1.zero],
          at: insertionPoint)
        _ = insertUnreachable(at: insertionPoint)
      } else {
        insertReturn(at: insertionPoint)
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(store i: IR.InstructionID) {
      let s = m[i] as! IR.Store
      insertStore(llvm(s.object), to: llvm(s.target), at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(switch i: IR.InstructionID) {
      let s = m[i] as! Switch

      // Pick the case 0 as the "default".
      let cases = s.successors[1...].enumerated().map { (value, destination) in
        (word().constant(UInt64(value)), block[destination]!)
      }

      let n = llvm(s.index)
      insertSwitch(
        on: n, cases: cases, default: block[s.successors[0]]!,
        at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(unionDiscriminator i: IR.InstructionID) {
      let s = m[i] as! UnionDiscriminator
      let t = UnionType(m.type(of: s.container).ast)!

      let baseType = ir.llvm(unionType: t, in: &self)
      let container = llvm(s.container)
      let indices = [i32.constant(0), i32.constant(1)]
      let discriminator = insertGetElementPointerInBounds(
        of: container, typed: baseType, indices: indices, at: insertionPoint)
      register[.register(i)] = insertLoad(word(), from: discriminator, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(unreachable i: IR.InstructionID) {
      insertUnreachable(at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(wrapAddr i: IR.InstructionID) {
      let s = m[i] as! IR.WrapExistentialAddr
      let t = containerType()
      let a = insertAlloca(t, atEntryOf: transpilation)
      insertStore(container(witness: s.witness, table: s.table), to: a, at: insertionPoint)
      register[.register(i)] = a
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(yield i: IR.InstructionID) {
      let s = m[i] as! IR.Yield
      let p = llvm(s.projection)

      // The intrinsic will return a non-zero result if the subscript should resume abnormally.
      _ = insertCall(
        LLVM.Function(intrinsic(named: Intrinsic.llvm.coro.suspend.retcon, for: [i1])!)!,
        on: [p],
        at: insertionPoint)
    }

    /// Returns the LLVM IR value corresponding to the Hylo IR operand `o`.
    func llvm(_ o: IR.Operand) -> LLVM.IRValue {
      if case .constant(let c) = o {
        return transpiledConstant(c, usedIn: m, from: ir)
      } else {
        return register[o]!
      }
    }

    /// Returns the callee of `s`.
    func unpackCallee(of s: Operand) -> LambdaContents {
      if case .constant(let f) = s {
        let f = transpiledConstant(f, usedIn: m, from: ir)
        let t = LLVM.Function(f)!.valueType
        return .init(function: f, type: t, environment: [])
      }

      // `s` is a lambda.
      let hyloType = LambdaType(m.type(of: s).ast)!
      let llvmType = StructType(ir.llvm(hyloType, in: &self))!
      let lambda = llvm(s)

      // The first element of the representation is the function pointer.
      var f = insertGetStructElementPointer(
        of: lambda, typed: llvmType, index: 0, at: insertionPoint)
      f = insertLoad(ptr, from: f, at: insertionPoint)

      // Following elements constitute the environment.
      var environment: [LLVM.IRValue] = []
      for (i, c) in hyloType.captures.enumerated() {
        var x = insertGetStructElementPointer(
          of: lambda, typed: llvmType, index: i + 1, at: insertionPoint)

        // Remote captures are passed deferenced.
        if c.type.base is RemoteType {
          x = insertLoad(ptr, from: x, at: insertionPoint)
        }

        environment.append(x)
      }

      let t = transpiledType(hyloType)
      return .init(function: f, type: t, environment: environment)
    }

    /// Returns an existential container wrapping the given `witness` and witness `table`.
    func container(witness: Operand, table: Operand) -> LLVM.IRValue {
      let t = containerType()
      var v = t.null
      v = insertInsertValue(llvm(witness), at: 0, into: v, at: insertionPoint)
      v = insertInsertValue(llvm(table), at: 1, into: v, at: insertionPoint)
      return v
    }
  }

  /// Inserts the prologue of the subscript `transpilation` at the end of its entry and returns
  /// a pointer to its stack frame.
  fileprivate mutating func insertSubscriptPrologue(
    into transpilation: LLVM.Function
  ) -> IRValue {
    let insertionPoint = endOf(transpilation.entry!)
    let id = insertCall(
      LLVM.Function(intrinsic(named: Intrinsic.llvm.coro.id.retcon.once)!)!,
      on: [
        i32.constant(8), i32.constant(8), transpilation.parameters[0],
        slidePrototype(), mallocPrototype(), freePrototype(),
      ],
      at: insertionPoint)
    return insertCall(
      LLVM.Function(intrinsic(named: Intrinsic.llvm.coro.begin)!)!,
      on: [id, ptr.null],
      at: insertionPoint)
  }

}

extension LLVMProgram: CustomStringConvertible {

  public var description: String { "\(list: llvmModules, joinedBy: "\n")" }

}

/// The contents of a lambda.
private struct LambdaContents {

  /// A pointer to the underlying thin function.
  let function: LLVM.IRValue

  /// The type `function`.
  let type: LLVM.IRType

  /// The lambda's environment.
  let environment: [LLVM.IRValue]

}

/// The memory layout of a Hylo type represented in LLVM.
private struct LLVMMemoryLayout {

  /// The contiguous memory footprint of the type's instances, in bytes.
  let size: LLVM.IRValue

  /// The preferred memory alignment of the `T`'s instances, in bytes.
  let preferredAlignment: LLVM.IRValue

}
