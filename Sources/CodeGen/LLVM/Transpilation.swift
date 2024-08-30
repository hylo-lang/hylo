import Foundation
import FrontEnd
import IR
import SwiftyLLVM
import Utils

/// The state of a compilation from Hylo IR to LLVM IR.
struct CodeGenerationContext {

  /// The program containing the `module`.
  let ir: IR.Program

  /// The Hylo module being compiled.
  let module: ModuleDecl.ID

  /// A table from string constant to its representation in LLVM.
  var strings = Trie<Data, SwiftyLLVM.GlobalVariable>()

  /// Creates an instance for compiling `m`, which is a module of `p`.
  init(forCompiling m: ModuleDecl.ID, of p: IR.Program) {
    self.ir = p
    self.module = m
  }

  /// Projects the contents of the Hylo module being compiled.
  var source: IR.Module {
    _read { yield ir.modules[module]! }
  }

}

extension SwiftyLLVM.Module {

  /// Creates the LLVM transpilation of the Hylo IR module `m` in `ir`.
  init(transpiling m: ModuleDecl.ID, in context: inout CodeGenerationContext) {
    let source = context.ir.modules[m]!
    self.init(source.name)

    for t in source.productTypes {
      _ = demandMetatype(of: ^t, in: &context)
    }
    for t in source.traits {
      _ = demandTrait(t, in: &context)
    }
    for a in source.allocations {
      incorporate(a, in: &context)
    }
    for f in source.functions.keys {
      incorporate(f, in: &context)
    }
  }

  /// Transpiles and incorporates `f`, which is a function or subscript of `m` in `ir`.
  mutating func incorporate(_ f: IR.Function.ID, in context: inout CodeGenerationContext) {
    // Don't transpile generic functions.
    if context.source[f].isGeneric {
      return
    }

    if context.source[f].isSubscript {
      let d = declareSubscript(transpiledFrom: f, in: &context)
      transpile(contentsOf: f, into: d, inContext: &context)
    } else {
      let d = declareFunction(transpiledFrom: f, in: &context)
      transpile(contentsOf: f, into: d, inContext: &context)
      if f == context.source.entryFunction {
        defineMain(calling: f, in: &context)
      }
    }
  }

  /// Transpiles and incorates `s`, which is a static allocation of `m` in `ir`.
  ///
  /// The storage is allocated along with a flag keeping track of its initialization state. Access
  /// is provided by an addressor that initializes the storage the first time it is called using
  /// `s.initializer`.
  private mutating func incorporate(_ s: StaticStorage, in context: inout CodeGenerationContext) {
    let prefix = context.ir.base.mangled(s.id)

    // Define the static allocation.
    let t = context.ir.llvm(s.pointee, in: &self)
    let u = SwiftyLLVM.StructType(named: prefix + ".T", [t, self.i1], in: &self)
    let storage = addGlobalVariable(prefix + ".S", u)
    setInitializer(u.null, for: storage)
    setLinkage(.private, for: storage)

    // Define the addressor projecting the allocated access.
    incorporate(s.initializer, in: &context)
    let initializer = function(named: context.ir.llvmName(of: s.initializer))!
    let addressor = declareFunction(prefix, .init(from: [], to: ptr, in: &self))

    let entry = appendBlock(to: addressor)
    var insertionPoint = endOf(entry)

    // %0 = getelementptr @storage, 0
    // %1 = getelementptr @storage, 1
    // %2 = load %1
    let x0 = insertGetStructElementPointer(
      of: storage, typed: u, index: 0, at: insertionPoint)
    let x1 = insertGetStructElementPointer(
      of: storage, typed: u, index: 1, at: insertionPoint)
    let x2 = insertLoad(i1, from: x1, at: insertionPoint)

    // b1 %2, b0, b1
    let b0 = appendBlock(named: "project", to: addressor)
    let b1 = appendBlock(named: "init", to: addressor)
    insertCondBr(if: x2, then: b0, else: b1, at: insertionPoint)

    // %3 = alloca {}
    // call void initializer(%0, %3)
    // store true, %1
    // br b0
    insertionPoint = endOf(b1)
    let x3 = insertAlloca(context.ir.llvm(AnyType.void, in: &self), at: insertionPoint)
    _ = insertCall(initializer, on: [x0, x3], at: insertionPoint)
    _ = insertStore(i1.constant(1), to: x1, at: insertionPoint)
    insertBr(to: b0, at: insertionPoint)

    // ret %1
    insertionPoint = endOf(b0)
    insertReturn(x0, at: insertionPoint)
  }

  /// Defines a "main" function calling the function `f`, which represents the entry point of the
  /// entry module `m` of the program `ir`.
  ///
  /// This method creates a LLVM entry point calling `f`, which is the lowered form of a public
  /// function named "main", taking no parameter and returning either `Void` or `Int32`. `f` will
  /// be linked privately in `m`.
  private mutating func defineMain(
    calling f: IR.Function.ID, in context: inout CodeGenerationContext
  ) {
    let main = declareFunction("main", FunctionType(from: [], to: i32, in: &self))

    let b = appendBlock(to: main)
    let p = endOf(b)

    let transpilation = function(named: context.ir.llvmName(of: f))!
    setLinkage(.private, for: transpilation)

    let int32 = context.ir.ast.coreType("Int32")!
    switch context.source[f].output {
    case int32:
      let t = StructType(context.ir.llvm(int32, in: &self))!
      let s = insertAlloca(t, at: p)
      _ = insertCall(transpilation, on: [s], at: p)

      let statusPointer = insertGetStructElementPointer(of: s, typed: t, index: 0, at: p)
      let status = insertLoad(i32, from: statusPointer, at: p)
      insertReturn(status, at: p)

    default:
      let t = context.ir.llvm(AnyType.void, in: &self)
      let s = insertAlloca(t, at: p)
      _ = insertCall(transpilation, on: [s], at: p)
      insertReturn(i32.zero, at: p)
    }
  }

  /// Returns the LLVM type of a metatype instance.
  private mutating func metatypeType() -> SwiftyLLVM.StructType {
    if let t = type(named: "_hylo_metatype") {
      return .init(t)!
    }

    let fields: [SwiftyLLVM.IRType] = [
      word(),  // size
      word(),  // alignment
      word(),  // stride
      ptr,  // representation
    ]
    return SwiftyLLVM.StructType(fields, in: &self)
  }

  /// Returns the LLVM type of an existential container.
  private mutating func containerType() -> SwiftyLLVM.StructType {
    if let t = type(named: "_val_container") {
      return .init(t)!
    }
    return StructType([ptr, ptr], in: &self)
  }

  /// Returns the prototype of subscript slides.
  private mutating func slidePrototype() -> SwiftyLLVM.Function {
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
  private mutating func mallocPrototype() -> SwiftyLLVM.Function {
    if let f = function(named: "malloc") {
      return f
    }

    let f = declareFunction(
      "malloc",
      FunctionType(from: [word()], to: ptr, in: &self))
    addAttribute(named: .noundef, to: f.parameters[0])
    addAttribute(named: .noalias, to: f.returnValue)

    return f
  }

  /// Returns the declaration of `free`.
  private mutating func freePrototype() -> SwiftyLLVM.Function {
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
  private mutating func transpiledType(_ t: ArrowType) -> SwiftyLLVM.FunctionType {
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
    _ c: any IR.Constant, in context: inout CodeGenerationContext
  ) -> SwiftyLLVM.IRValue {
    switch c {
    case let v as IR.WordConstant:
      return transpiledConstant(v, in: &context)
    case let v as IR.IntegerConstant:
      return transpiledConstant(v, in: &context)
    case let v as IR.FloatingPointConstant:
      return transpiledConstant(v, in: &context)
    case let v as IR.WitnessTable:
      return transpiledWitnessTable(v, in: &context)
    case let v as IR.FunctionReference:
      return declare(v, from: context.ir)
    case let v as MetatypeType:
      return demandMetatype(of: v.instance, in: &context)
    case is IR.VoidConstant:
      return SwiftyLLVM.StructConstant(aggregating: [], in: &self)
    default:
      unreachable()
    }
  }

  /// Returns the LLVM IR value corresponding to the Hylo IR constant `c` when used in `m` in `ir`.
  private mutating func transpiledConstant(
    _ c: IR.WordConstant, in context: inout CodeGenerationContext
  ) -> SwiftyLLVM.IRValue {
    word().constant(c.value)
  }

  /// Returns the LLVM IR value corresponding to the Hylo IR constant `c` when used in `m` in `ir`.
  private mutating func transpiledConstant(
    _ c: IR.IntegerConstant, in context: inout CodeGenerationContext
  ) -> SwiftyLLVM.IRValue {
    guard c.value.bitWidth <= 64 else { UNIMPLEMENTED() }
    let t = SwiftyLLVM.IntegerType(c.value.bitWidth, in: &self)
    return t.constant(c.value.words[0])
  }

  /// Returns the LLVM IR value corresponding to the Hylo IR constant `c` when used in `m` in `ir`.
  private mutating func transpiledConstant(
    _ c: IR.FloatingPointConstant, in context: inout CodeGenerationContext
  ) -> SwiftyLLVM.IRValue {
    let t = SwiftyLLVM.FloatingPointType(context.ir.llvm(c.type.ast, in: &self))!
    return t.constant(parsing: c.value)
  }

  /// Returns the LLVM IR value of the witness table `t` used in `m` in `ir`.
  private mutating func transpiledWitnessTable(
    _ t: WitnessTable, in context: inout CodeGenerationContext
  ) -> SwiftyLLVM.IRValue {
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
    var tableContents: [SwiftyLLVM.IRValue] = [
      demandMetatype(of: t.witness, in: &context),
      word().constant(t.conformances.count),
    ]

    // Encode the table's trait and implementation maps.
    var entries: [SwiftyLLVM.IRValue] = []
    var implementations: [SwiftyLLVM.IRValue] = []
    for c in t.conformances {
      let entry: [SwiftyLLVM.IRValue] = [
        demandTrait(c.concept, in: &context),
        word().constant(implementations.count),
      ]
      entries.append(SwiftyLLVM.StructConstant(aggregating: entry, in: &self))

      for (r, d) in c.implementations {
        let requirement: [SwiftyLLVM.IRValue] = [
          word().constant(r.rawValue.bits),
          transpiledRequirementImplementation(d, from: context.ir),
        ]
        implementations.append(SwiftyLLVM.StructConstant(aggregating: requirement, in: &self))
      }
    }

    // Append the sentinel at the end of the trait map.
    entries.append(
      SwiftyLLVM.StructConstant(
        aggregating: [ptr.null, word().constant(UInt64(implementations.count))], in: &self))

    // Put everything together.
    tableContents.append(
      SwiftyLLVM.ArrayConstant(
        of: SwiftyLLVM.StructType([ptr, word()], in: &self), containing: entries, in: &self))

    if !implementations.isEmpty {
      tableContents.append(
        SwiftyLLVM.ArrayConstant(
          of: SwiftyLLVM.StructType([word(), ptr], in: &self), containing: implementations,
          in: &self))
    }

    let table = SwiftyLLVM.StructConstant(aggregating: tableContents, in: &self)

    let g = declareGlobalVariable(context.ir.base.mangled(t), table.type)
    setInitializer(table, for: g)
    setLinkage(.linkOnce, for: g)
    setGlobalConstant(true, for: g)
    return g
  }

  /// Returns the LLVM IR value of the requirement implementation `i`, which is in `ir`.
  private mutating func transpiledRequirementImplementation(
    _ i: IR.Conformance.Implementation, from ir: IR.Program
  ) -> SwiftyLLVM.Function {
    switch i {
    case .function(let f):
      return declare(f, from: ir)
    case .value:
      UNIMPLEMENTED()
    }
  }

  /// Returns the LLVM IR value of the metatype `t` used in `m` in `ir`.
  private mutating func demandMetatype(
    of t: AnyType, in context: inout CodeGenerationContext
  ) -> SwiftyLLVM.GlobalVariable {
    demandMetatype(of: t, in: &context) { (me, c, v) in
      if let u = ProductType(t) {
        me.initializeTranspiledProductTypeMetatype(v, of: u, in: &c)
      } else {
        me.initializeTranspiledMetatype(v, of: t, in: &c)
      }
    }
  }

  /// Initializes `instance` with the value of the metatype of `t` used in `m` in `ir`.
  private mutating func initializeTranspiledMetatype<T: TypeProtocol>(
    _ instance: SwiftyLLVM.GlobalVariable,
    of t: T, in context: inout CodeGenerationContext
  ) {
    setLinkage(.linkOnce, for: instance)

    let layout = ConcreteTypeLayout(of: ^t, definedIn: context.ir, forUseIn: &self)
    let v = SwiftyLLVM.StructType(instance.valueType)!.constant(
      aggregating: [
        word().constant(layout.size),
        word().constant(layout.alignment),
        word().constant(layout.stride),
        ptr.null,
      ],
      in: &self)

    setInitializer(v, for: instance)
    setGlobalConstant(true, for: instance)
  }

  /// Initializes `instance` with the value of the metatype of `t` used in `m` in `ir`.
  private mutating func initializeTranspiledProductTypeMetatype(
    _ instance: SwiftyLLVM.GlobalVariable,
    of t: ProductType, in context: inout CodeGenerationContext
  ) {
    // Initialize the instance if it's being used in the module defining `t`. Otherwise, simply let
    // the symbol be linked to its definition later.
    if context.module != context.ir.base.module(containing: t.decl) { return }

    // If `t` is generic, its metatype is only a stub.
    let layout: ConcreteTypeLayout
    if !context.ir.base[t.decl].genericParameters.isEmpty {
      layout = ConcreteTypeLayout(size: 0, alignment: 0)
    } else {
      layout = ConcreteTypeLayout(of: ^t, definedIn: context.ir, forUseIn: &self)
    }

    let v = SwiftyLLVM.StructType(instance.valueType)!.constant(
      aggregating: [
        word().constant(layout.size),
        word().constant(layout.alignment),
        word().constant(layout.stride),
        ptr.null,
      ],
      in: &self)

    setInitializer(v, for: instance)
    setGlobalConstant(true, for: instance)
  }

  /// Returns the LLVM IR value of `t` used in `m` in `ir`, calling `initializeInstance` to
  /// initialize it.
  private mutating func demandMetatype<T: TypeProtocol>(
    of t: T, in context: inout CodeGenerationContext,
    initializedWith initializeInstance: (
      inout Self, inout CodeGenerationContext, SwiftyLLVM.GlobalVariable
    ) -> Void
  ) -> SwiftyLLVM.GlobalVariable {
    let globalName = context.ir.base.mangled(t)
    if let g = global(named: globalName) { return g }

    let metatype = metatypeType()
    let instance = declareGlobalVariable(globalName, metatype)
    initializeInstance(&self, &context, instance)
    return instance
  }

  /// Returns the LLVM IR value of `t` used in `m` in `ir`.
  private mutating func demandTrait(
    _ t: TraitType, in context: inout CodeGenerationContext
  ) -> SwiftyLLVM.GlobalVariable {
    // Check if we already created the trait's instance.
    let globalName = context.ir.base.mangled(t)
    if let g = global(named: globalName) {
      return g
    }

    // Initialize the instance if it's being used in the module defining `t`. Otherwise, simply
    // declare the symbol and let it be linked later.
    let instance = declareGlobalVariable(globalName, ptr)
    if context.module != context.ir.base.module(containing: t.decl) {
      return instance
    }

    let s = SwiftyLLVM.StringConstant(globalName, nullTerminated: true, in: &self)
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
  ) -> SwiftyLLVM.Function {
    let t = transpiledType(ArrowType(ref.type.ast)!)
    return declareFunction(ir.llvmName(of: ref.function), t)
  }

  /// Inserts and returns the transpiled declaration of `f`, which is a function of `m` in `ir`.
  private mutating func declareFunction(
    transpiledFrom f: IR.Function.ID, in context: inout CodeGenerationContext
  ) -> SwiftyLLVM.Function {
    precondition(!context.source[f].isSubscript)

    // Parameters and return values are passed by reference.
    let parameters = Array(
      repeating: ptr as SwiftyLLVM.IRType, count: context.source[f].inputs.count + 1)
    let transpilation = declareFunction(
      context.ir.llvmName(of: f), .init(from: parameters, in: &self))

    configureAttributes(transpilation, transpiledFrom: f, of: context.source)
    configureInputAttributes(
      transpilation.parameters.dropLast(), transpiledFrom: f, in: context.source)

    return transpilation
  }

  /// Inserts and returns the transpiled declaration of `f`, which is a subscript of `m` in `ir`.
  private mutating func declareSubscript(
    transpiledFrom f: IR.Function.ID, in context: inout CodeGenerationContext
  ) -> SwiftyLLVM.Function {
    precondition(context.source[f].isSubscript)

    // Parameters are a buffer for the subscript frame followed by its declared parameters. Return
    // type is a pair `(c, p)` where `c` points to a subscript slide and `p` is the address of the
    // projected value.
    let r = SwiftyLLVM.StructType([ptr, ptr], in: &self)
    let parameters = Array(
      repeating: ptr as SwiftyLLVM.IRType, count: context.source[f].inputs.count + 1)
    let transpilation = declareFunction(
      context.ir.llvmName(of: f), .init(from: parameters, to: r, in: &self))

    configureAttributes(transpilation, transpiledFrom: f, of: context.source)
    configureInputAttributes(
      transpilation.parameters.dropFirst(), transpiledFrom: f, in: context.source)

    return transpilation
  }

  /// Adds to `llvmFunction` the attributes implied by its IR form `f`, which is in `m`.
  private mutating func configureAttributes(
    _ llvmFunction: SwiftyLLVM.Function, transpiledFrom f: IR.Function.ID, of m: IR.Module
  ) {
    if m[f].linkage == .module {
      setLinkage(.private, for: llvmFunction)
    }

    if !m[f].isSubscript {
      let r = llvmFunction.parameters.last!
      addAttribute(.init(.noalias, in: &self), to: r)
      addAttribute(.init(.nocapture, in: &self), to: r)
      addAttribute(.init(.nofree, in: &self), to: r)

      if m[f].output.isNever {
        addAttribute(.init(.noreturn, in: &self), to: llvmFunction)
      }
    }
  }

  /// Adds to each parameter in `llvmParameters` the attributes implied by its corresponding IR
  /// form in `m[f].inputs`.
  private mutating func configureInputAttributes(
    _ llvmParameters: SwiftyLLVM.Function.Parameters.SubSequence,
    transpiledFrom f: IR.Function.ID, in m: IR.Module
  ) {
    assert(llvmParameters.count == m[f].inputs.count)
    for (p, l) in llvmParameters.enumerated() {
      configureInputAttributes(l, transpiledFrom: p, in: f, in: m)
    }
  }

  /// Adds to `llvmParameter` the attributes implied by its IR form in `m[f].inputs[p]`.
  private mutating func configureInputAttributes(
    _ llvmParameter: SwiftyLLVM.Parameter,
    transpiledFrom p: Int, in f: IR.Function.ID, in m: IR.Module
  ) {
    addAttribute(named: .noalias, to: llvmParameter)
    addAttribute(named: .nofree, to: llvmParameter)

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
    into transpilation: SwiftyLLVM.Function,
    inContext context: inout CodeGenerationContext
  ) {
    /// The function's entry.
    guard let entry = context.source[f].entry else { return }

    /// Where new LLVM IR instruction are inserted.
    var insertionPoint: SwiftyLLVM.InsertionPoint!

    /// A map from Hylo IR basic block to its LLVM counterpart.
    var block: [IR.Block.ID: SwiftyLLVM.BasicBlock] = [:]

    /// A map from Hylo IR register to its LLVM counterpart.
    var register: [IR.Operand: SwiftyLLVM.IRValue] = [:]

    /// A map from projection to its side results in SwiftyLLVM.
    ///
    /// Projection calls is transpiled as coroutine calls, producing a slide and a frame pointer in
    /// addition to the projected value. These values are stored here so that `register` can be a
    /// one-to-one mapping from Hylo registers to LLVM registers.
    var byproduct: [IR.InstructionID: (slide: SwiftyLLVM.IRValue, frame: SwiftyLLVM.IRValue)] = [:]

    /// The address of the function's frame if `f` is a subscript, or `nil` otherwise.
    let frame: SwiftyLLVM.IRValue?

    /// The prologue of the transpiled function, which contains its stack allocations.
    let prologue = appendBlock(named: "prologue", to: transpilation)

    // In subscripts, parameters are laid out after the frame buffer.
    let parameterOffset: Int
    if context.source[f].isSubscript {
      parameterOffset = 1
      frame = insertSubscriptPrologue(into: transpilation)
    } else {
      parameterOffset = 0
      frame = nil
    }

    for i in context.source[context.source.entry(of: f)!].inputs.indices {
      let o = Operand.parameter(.init(f, entry), i)
      let s = transpilation.parameters[parameterOffset + i]
      register[o] = s
    }

    for b in context.source.blocks(in: f) {
      block[b] = appendBlock(named: b.description, to: transpilation)
    }

    for b in context.source.blocks(in: f) {
      insertionPoint = endOf(block[b]!)
      for i in context.source.instructions(in: b) {
        insert(i)
      }
    }

    insertBr(to: block[.init(f, entry)]!, at: endOf(prologue))

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(_ i: IR.InstructionID) {
      switch context.source[i] {
      case is IR.AddressToPointer:
        insert(addressToPointer: i)
      case is IR.AdvancedByBytes:
        insert(advancedByBytes: i)
      case is IR.AdvancedByStrides:
        insert(advancedByStrides: i)
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
      case is IR.MemoryCopy:
        insert(memoryCopy: i)
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
      case is IR.UnionSwitch:
        insert(unionSwitch: i)
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
      let s = context.source[i] as! AddressToPointer
      register[.register(i)] = llvm(s.source)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(advancedByBytes i: IR.InstructionID) {
      let s = context.source[i] as! AdvancedByBytes

      let base = llvm(s.base)
      let v = insertGetElementPointerInBounds(
        of: base, typed: ptr, indices: [llvm(s.byteOffset)], at: insertionPoint)
      register[.register(i)] = v
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(advancedByStrides i: IR.InstructionID) {
      let s = context.source[i] as! AdvancedByStrides

      let base = llvm(s.base)
      let baseType = context.ir.llvm(context.source.type(of: s.base).ast, in: &self)
      let indices = [i32.constant(0), i32.constant(s.offset)]
      let v = insertGetElementPointerInBounds(
        of: base, typed: baseType, indices: indices, at: insertionPoint)
      register[.register(i)] = v
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(allocStack i: IR.InstructionID) {
      let s = context.source[i] as! AllocStack
      let t = context.ir.llvm(s.allocatedType, in: &self)
      if layout.storageSize(of: t) == 0 {
        register[.register(i)] = ptr.null
      } else {
        register[.register(i)] = insertAlloca(t, atEntryOf: transpilation)
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(access i: IR.InstructionID) {
      let s = context.source[i] as! Access
      register[.register(i)] = llvm(s.source)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(branch i: IR.InstructionID) {
      let s = context.source[i] as! Branch
      insertBr(to: block[s.target]!, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(call i: IR.InstructionID) {
      let s = context.source[i] as! Call
      var arguments: [SwiftyLLVM.IRValue] = []

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
      let s = context.source[i] as! CallFFI
      let parameters = s.operands.map { (o) in
        context.ir.llvm(context.source.type(of: o).ast, in: &self)
      }

      let returnType: SwiftyLLVM.IRType
      if s.returnType.ast.isVoidOrNever {
        returnType = void
      } else {
        returnType = context.ir.llvm(s.returnType.ast, in: &self)
      }

      let callee = declareFunction(s.callee, .init(from: parameters, to: returnType, in: &self))
      let arguments = s.operands.map({ llvm($0) })
      register[.register(i)] = insertCall(callee, on: arguments, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(captureIn i: IR.InstructionID) {
      let s = context.source[i] as! CaptureIn
      insertStore(llvm(s.source), to: llvm(s.target), at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(closeUnion i: IR.InstructionID) {
      let s = context.source[i] as! CloseUnion
      let open = context.source[s.start.instruction!] as! OpenUnion

      // TODO: Memoize somehow
      let t = UnionType(context.source.type(of: open.container).ast)!
      let e = context.ir.base.discriminatorToElement(in: t)
      let n = e.firstIndex(of: open.payloadType)!

      let baseType = context.ir.llvm(unionType: t, in: &self)
      let container = llvm(open.container)
      let indices = [i32.constant(0), i32.constant(1)]
      let discriminator = insertGetElementPointerInBounds(
        of: container, typed: baseType, indices: indices, at: insertionPoint)
      insertStore(word().constant(UInt64(n)), to: discriminator, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(constantString i: IR.InstructionID) {
      let s = context.source[i] as! ConstantString
      let count = s.value.count

      // Contents fit inline storage.
      if count <= 7 {
        var units = UInt64(truncatingIfNeeded: count) << 2
        withUnsafeMutableBytes(of: &units) { (buffer) in
          let payload = UnsafeMutableBufferPointer(
            rebasing: buffer.assumingMemoryBound(to: UInt8.self)[1...])
          _ = payload.initialize(from: s.value)
        }
        register[.register(i)] = i64.constant(units)
      }

      // Contents has already been incorporated in the module.
      else if let storage = context.strings[s.value] {
        let x0 = insertPtrToInt(storage, to: i64, at: insertionPoint)
        let x1 = insertBitwiseOr(x0, i64(0b11), at: insertionPoint)
        register[.register(i)] = x1
      }

      // Contents require new out-of-line storage.
      else {
        let w = word()

        let payload = SwiftyLLVM.ArrayConstant(bytes: s.value, in: &self)
        let storageType = StructType([w, w, payload.type], in: &self)
        let storageValue = storageType.constant(
          aggregating: [w(count), w(count), payload] as [IRValue],
          in: &self)

        let storage = declareGlobalVariable("_" + UUID().uuidString, storageType)
        setInitializer(storageValue, for: storage)
        setLinkage(.private, for: storage)
        setGlobalConstant(true, for: storage)
        context.strings[s.value] = storage

        let x0 = insertPtrToInt(storage, to: i64, at: insertionPoint)
        let x1 = insertBitwiseOr(x0, i64(0b11), at: insertionPoint)
        register[.register(i)] = x1
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(condBranch i: IR.InstructionID) {
      let s = context.source[i] as! CondBranch
      let c = llvm(s.condition)
      insertCondBr(
        if: c, then: block[s.targetIfTrue]!, else: block[s.targetIfFalse]!,
        at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(endProjection i: IR.InstructionID) {
      let s = context.source[i] as! EndProject
      let start = s.start.instruction!
      assert(context.source[start] is Project)

      let t = SwiftyLLVM.FunctionType(from: [ptr, i1], to: void, in: &self)
      let p = byproduct[start]!
      _ = insertCall(p.slide, typed: t, on: [p.frame, i1.zero], at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(globalAddr i: IR.InstructionID) {
      let s = context.source[i] as! IR.GlobalAddr
      let n = context.ir.base.mangled(s.binding)
      let a = declareFunction(n, .init(from: [], to: ptr, in: &self))
      register[.register(i)] = insertCall(a, on: [], at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(subfieldView i: IR.InstructionID) {
      let s = context.source[i] as! SubfieldView

      let base = llvm(s.recordAddress)
      let baseType = context.ir.llvm(context.source.type(of: s.recordAddress).ast, in: &self)
      let indices = [i32.constant(0)] + s.subfield.map({ i32.constant(UInt64($0)) })
      let v = insertGetElementPointerInBounds(
        of: base, typed: baseType, indices: indices, at: insertionPoint)
      register[.register(i)] = v
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(llvm i: IR.InstructionID) {
      let s = context.source[i] as! IR.LLVMInstruction
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

      case .ashr:
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertAShr(l, r, at: insertionPoint)

      case .sdiv(let e, _):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertSignedDiv(exact: e, l, r, at: insertionPoint)

      case .udiv(let e, _):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertUnsignedDiv(exact: e, l, r, at: insertionPoint)

      case .srem:
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertSignedRem(l, r, at: insertionPoint)

      case .urem:
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i)] = insertUnsignedRem(l, r, at: insertionPoint)

      case .signedAdditionWithOverflow(let t):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        let f = intrinsic(
          named: Intrinsic.llvm.sadd.with.overflow,
          for: [context.ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(
          SwiftyLLVM.Function(f)!, on: [l, r], at: insertionPoint)

      case .unsignedAdditionWithOverflow(let t):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        let f = intrinsic(
          named: Intrinsic.llvm.uadd.with.overflow,
          for: [context.ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(
          SwiftyLLVM.Function(f)!, on: [l, r], at: insertionPoint)

      case .signedSubtractionWithOverflow(let t):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        let f = intrinsic(
          named: Intrinsic.llvm.ssub.with.overflow,
          for: [context.ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(
          SwiftyLLVM.Function(f)!, on: [l, r], at: insertionPoint)

      case .unsignedSubtractionWithOverflow(let t):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        let f = intrinsic(
          named: Intrinsic.llvm.usub.with.overflow,
          for: [context.ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(
          SwiftyLLVM.Function(f)!, on: [l, r], at: insertionPoint)

      case .signedMultiplicationWithOverflow(let t):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        let f = intrinsic(
          named: Intrinsic.llvm.smul.with.overflow,
          for: [context.ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(
          SwiftyLLVM.Function(f)!, on: [l, r], at: insertionPoint)

      case .unsignedMultiplicationWithOverflow(let t):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        let f = intrinsic(
          named: Intrinsic.llvm.umul.with.overflow,
          for: [context.ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(
          SwiftyLLVM.Function(f)!, on: [l, r], at: insertionPoint)

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
        let target = context.ir.llvm(builtinType: t, in: &self)
        let source = llvm(s.operands[0])
        register[.register(i)] = insertTrunc(source, to: target, at: insertionPoint)

      case .zext(_, let t):
        let target = context.ir.llvm(builtinType: t, in: &self)
        let source = llvm(s.operands[0])
        register[.register(i)] = insertZeroExtend(source, to: target, at: insertionPoint)

      case .sext(_, let t):
        let target = context.ir.llvm(builtinType: t, in: &self)
        let source = llvm(s.operands[0])
        register[.register(i)] = insertSignExtend(source, to: target, at: insertionPoint)

      case .inttoptr(_):
        let source = llvm(s.operands[0])
        register[.register(i)] = insertIntToPtr(source, at: insertionPoint)

      case .ptrtoint(let t):
        let target = context.ir.llvm(builtinType: t, in: &self)
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
        let target = context.ir.llvm(builtinType: t, in: &self)
        let source = llvm(s.operands[0])
        register[.register(i)] = insertFPTrunc(source, to: target, at: insertionPoint)

      case .ctpop(let t):
        let source = llvm(s.operands[0])
        let f = intrinsic(
          named: Intrinsic.llvm.ctpop,
          for: [context.ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(
          SwiftyLLVM.Function(f)!, on: [source], at: insertionPoint)

      case .ctlz(let t):
        let source = llvm(s.operands[0])
        let f = intrinsic(
          named: Intrinsic.llvm.ctlz,
          for: [context.ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(
          SwiftyLLVM.Function(f)!, on: [source, i1.zero], at: insertionPoint)

      case .cttz(let t):
        let source = llvm(s.operands[0])
        let f = intrinsic(
          named: Intrinsic.llvm.cttz,
          for: [context.ir.llvm(builtinType: t, in: &self)])!
        register[.register(i)] = insertCall(
          SwiftyLLVM.Function(f)!, on: [source, i1.zero], at: insertionPoint)

      case .zeroinitializer(let t):
        register[.register(i)] = context.ir.llvm(builtinType: t, in: &self).null

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
      let s = context.source[i] as! Load
      let t = context.ir.llvm(s.objectType.ast, in: &self)
      let source = llvm(s.source)
      register[.register(i)] = insertLoad(t, from: source, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(memoryCopy i: IR.InstructionID) {
      let s = context.source[i] as! MemoryCopy

      let memcpy = SwiftyLLVM.Function(
        intrinsic(named: Intrinsic.llvm.memcpy, for: [ptr, ptr, i32])!)!
      let source = llvm(s.source)
      let target = llvm(s.target)

      let l = ConcreteTypeLayout(
        of: context.source.type(of: s.source).ast, definedIn: context.ir, forUseIn: &self)
      let byteCount = i32.constant(l.size)
      _ = insertCall(memcpy, on: [target, source, byteCount, i1.zero], at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(openCapture i: IR.InstructionID) {
      let s = context.source[i] as! OpenCapture
      register[.register(i)] = insertLoad(ptr, from: llvm(s.source), at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(openUnion i: IR.InstructionID) {
      let s = context.source[i] as! OpenUnion
      let t = UnionType(context.source.type(of: s.container).ast)!

      let baseType = context.ir.llvm(unionType: t, in: &self)
      let container = llvm(s.container)
      let indices = [i32.constant(0), i32.constant(0)]
      register[.register(i)] = insertGetElementPointerInBounds(
        of: container, typed: baseType, indices: indices, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(pointerToAddress i: IR.InstructionID) {
      let s = context.source[i] as! IR.PointerToAddress
      register[.register(i)] = llvm(s.source)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(project i: IR.InstructionID) {
      let s = context.source[i] as! IR.Project

      // %0 = alloca [8 x i8], align 8
      let buffer = SwiftyLLVM.ArrayType(8, i8, in: &self)
      let x0 = insertAlloca(buffer, at: insertionPoint)
      setAlignment(8, for: x0)

      // All arguments are passed by reference.
      var arguments: [SwiftyLLVM.IRValue] = [x0]
      for a in s.operands {
        if context.source.type(of: a).isObject {
          let t = context.ir.llvm(s.result!.ast, in: &self)
          let l = insertAlloca(t, atEntryOf: transpilation)
          insertStore(llvm(a), to: l, at: insertionPoint)
          arguments.append(l)
        } else {
          arguments.append(llvm(a))
        }
      }

      // %1 = call ptr @llvm.coro.prepare.retcon(ptr @s)
      let f = declareSubscript(transpiledFrom: s.callee, in: &context)
      let prepare = intrinsic(named: Intrinsic.llvm.coro.prepare.retcon)!
      let x1 = insertCall(SwiftyLLVM.Function(prepare)!, on: [f], at: insertionPoint)

      // %2 = call {ptr, ptr} %1(...)
      let x2 = insertCall(x1, typed: f.valueType, on: arguments, at: insertionPoint)

      register[.register(i)] = insertExtractValue(from: x2, at: 1, at: insertionPoint)
      byproduct[i] = (slide: insertExtractValue(from: x2, at: 0, at: insertionPoint), frame: x0)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(return i: IR.InstructionID) {
      if context.source[f].isSubscript {
        _ = insertCall(
          SwiftyLLVM.Function(intrinsic(named: Intrinsic.llvm.coro.end)!)!,
          on: [frame!, i1.zero],
          at: insertionPoint)
        _ = insertUnreachable(at: insertionPoint)
      } else {
        insertReturn(at: insertionPoint)
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(store i: IR.InstructionID) {
      let s = context.source[i] as! IR.Store
      let v = llvm(s.object)
      if layout.storageSize(of: v.type) > 0 {
        insertStore(llvm(s.object), to: llvm(s.target), at: insertionPoint)
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(switch i: IR.InstructionID) {
      let s = context.source[i] as! Switch

      let branches = s.successors.enumerated().map { (value, destination) in
        (word().constant(UInt64(value)), block[destination]!)
      }

      // The last branch is the "default".
      let n = llvm(s.index)
      insertSwitch(
        on: n, cases: branches.dropLast(), default: branches.last!.1,
        at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(unionDiscriminator i: IR.InstructionID) {
      let s = context.source[i] as! UnionDiscriminator
      register[.register(i)] = discriminator(s.container)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(unionSwitch i: IR.InstructionID) {
      let s = context.source[i] as! UnionSwitch

      if let (_, b) = s.targets.elements.uniqueElement {
        insertBr(to: block[b]!, at: insertionPoint)
      } else {
        let e = context.ir.base.discriminatorToElement(in: s.union)
        let branches = s.targets.map { (t, b) in
          (word().constant(e.firstIndex(of: t)!), block[b]!)
        }

        // The last branch is the "default".
        let d = llvm(s.discriminator)
        insertSwitch(
          on: d, cases: branches.dropLast(), default: branches.last!.1,
          at: insertionPoint)
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(unreachable i: IR.InstructionID) {
      insertUnreachable(at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(wrapAddr i: IR.InstructionID) {
      let s = context.source[i] as! IR.WrapExistentialAddr
      let t = containerType()
      let a = insertAlloca(t, atEntryOf: transpilation)
      insertStore(container(witness: s.witness, table: s.table), to: a, at: insertionPoint)
      register[.register(i)] = a
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(yield i: IR.InstructionID) {
      let s = context.source[i] as! IR.Yield
      let p = llvm(s.projection)

      // The intrinsic will return a non-zero result if the subscript should resume abnormally.
      _ = insertCall(
        SwiftyLLVM.Function(intrinsic(named: Intrinsic.llvm.coro.suspend.retcon, for: [i1])!)!,
        on: [p],
        at: insertionPoint)
    }

    /// Returns the LLVM IR value corresponding to the Hylo IR operand `o`.
    func llvm(_ o: IR.Operand) -> SwiftyLLVM.IRValue {
      if case .constant(let c) = o {
        return transpiledConstant(c, in: &context)
      } else {
        return register[o]!
      }
    }

    /// Returns the callee of `s`.
    func unpackCallee(of s: Operand) -> ArrowContents {
      if case .constant(let f) = s {
        let f = transpiledConstant(f, in: &context)
        let t = SwiftyLLVM.Function(f)!.valueType
        return .init(function: f, type: t, environment: [])
      }

      // `s` is an arrow.
      let hyloType = ArrowType(context.source.type(of: s).ast)!
      let llvmType = StructType(context.ir.llvm(hyloType, in: &self))!
      let lambda = llvm(s)

      // The first element of the representation is the function pointer.
      var f = insertGetStructElementPointer(
        of: lambda, typed: llvmType, index: 0, at: insertionPoint)
      f = insertLoad(ptr, from: f, at: insertionPoint)

      let e = insertGetStructElementPointer(
        of: lambda, typed: llvmType, index: 1, at: insertionPoint)
      let captures = StructType(context.ir.llvm(hyloType.environment, in: &self))!

      // Following elements constitute the environment.
      var environment: [SwiftyLLVM.IRValue] = []
      for (i, c) in hyloType.captures.enumerated() {
        var x = insertGetStructElementPointer(
          of: e, typed: captures, index: i, at: insertionPoint)

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
    func container(witness: Operand, table: Operand) -> SwiftyLLVM.IRValue {
      let t = containerType()
      var v = t.null
      v = insertInsertValue(llvm(witness), at: 0, into: v, at: insertionPoint)
      v = insertInsertValue(llvm(table), at: 1, into: v, at: insertionPoint)
      return v
    }

    /// Returns the value of `container`'s discriminator.
    func discriminator(_ container: IR.Operand) -> SwiftyLLVM.Instruction {
      let union = UnionType(context.source.type(of: container).ast)!
      let baseType = context.ir.llvm(unionType: union, in: &self)
      let container = llvm(container)
      let indices = [i32.constant(0), i32.constant(1)]
      let discriminator = insertGetElementPointerInBounds(
        of: container, typed: baseType, indices: indices, at: insertionPoint)
      return insertLoad(word(), from: discriminator, at: insertionPoint)
    }
  }

  /// Inserts the prologue of the subscript `transpilation` at the end of its entry and returns
  /// a pointer to its stack frame.
  fileprivate mutating func insertSubscriptPrologue(
    into transpilation: SwiftyLLVM.Function
  ) -> IRValue {
    let insertionPoint = endOf(transpilation.entry!)
    let id = insertCall(
      SwiftyLLVM.Function(intrinsic(named: Intrinsic.llvm.coro.id.retcon.once)!)!,
      on: [
        i32.constant(8), i32.constant(8), transpilation.parameters[0],
        slidePrototype(), mallocPrototype(), freePrototype(),
      ],
      at: insertionPoint)
    return insertCall(
      SwiftyLLVM.Function(intrinsic(named: Intrinsic.llvm.coro.begin)!)!,
      on: [id, ptr.null],
      at: insertionPoint)
  }

}

extension LLVMProgram: CustomStringConvertible {

  public var description: String { "\(list: llvmModules, joinedBy: "\n")" }

}

/// The contents of an arrow.
private struct ArrowContents {

  /// A pointer to the underlying thin function.
  let function: SwiftyLLVM.IRValue

  /// The type `function`.
  let type: SwiftyLLVM.IRType

  /// The arrow's environment.
  let environment: [SwiftyLLVM.IRValue]

}

extension IR.Program {

  /// Returns the name of `f` in LLVM IR.
  func llvmName(of f: IR.Function.ID) -> String {
    if case .lowered(let d) = f.value {
      return FunctionDecl.ID(d).flatMap({ base[$0].attributes.externalName }) ?? base.mangled(f)
    } else {
      return base.mangled(f)
    }
  }

}
