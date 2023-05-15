import Core
import IR
import LLVM
import Utils

extension LLVM.Module {

  /// Transpiles and incorporates `g`, which is a function of `m` in `ir`.
  mutating func incorporate(_ g: IR.Module.GlobalID, of m: IR.Module, from ir: LoweredProgram) {
    let v = transpiledConstant(m.globals[g], usedIn: m, from: ir)

    let d = declareGlobalVariable("\(m.syntax.id)\(g)", v.type)
    setInitializer(v, for: d)
    setLinkage(.private, for: d)
    setGlobalConstant(true, for: d)
  }

  /// Transpiles and incorporates `f`, which is a function or subscript of `m` in `ir`.
  mutating func incorporate(_ f: IR.Function.ID, of m: IR.Module, from ir: LoweredProgram) {
    // Don't transpile generic functions.
    if m[f].isGeneric {
      return
    }

    if m[f].isSubscript {
      let d = declare(subscript: f, of: m, from: ir)
      transpile(contentsOf: f, of: m, from: ir, into: d)
    } else {
      let d = declare(function: f, of: m, from: ir)
      transpile(contentsOf: f, of: m, from: ir, into: d)
      if f == m.entryFunction {
        defineMain(calling: f, of: m, from: ir)
      }
    }
  }

  private mutating func defineMain(
    calling f: IR.Function.ID,
    of m: IR.Module,
    from ir: LoweredProgram
  ) {
    let i32 = IntegerType(32, in: &self)
    let main = declareFunction("main", FunctionType(from: [], to: i32, in: &self))

    let b = appendBlock(to: main)
    let p = endOf(b)

    let transpilation = function(named: ir.mangle(f))!

    let val32 = ir.syntax.ast.coreType("Int32")!
    switch m[f].output {
    case val32:
      let t = StructType(ir.llvm(val32, in: &self))!
      let s = insertAlloca(t, at: p)
      _ = insertCall(transpilation, on: [s], at: p)

      let statusPointer = insertGetStructElementPointer(of: s, typed: t, index: 0, at: p)
      let status = insertLoad(i32, from: statusPointer, at: p)
      insertReturn(status, at: p)

    default:
      _ = insertCall(transpilation, on: [], at: p)
      insertReturn(i32.zero, at: p)
    }
  }

  /// Returns the LLVM type of a machine word.
  private mutating func wordType() -> LLVM.IntegerType {
    IntegerType(64, in: &self)
  }

  /// Returns the LLVM type of a metatype instance.
  private mutating func metatypeType() -> LLVM.StructType {
    if let t = type(named: "_val_metatype") {
      return .init(t)!
    }
    let i64 = IntegerType(64, in: &self)
    let ptr = PointerType(in: &self)
    return StructType([i64, i64, ptr], in: &self)
  }

  /// Returns the LLVM type of an existential container.
  private mutating func containerType() -> LLVM.StructType {
    if let t = type(named: "_val_container") {
      return .init(t)!
    }
    let ptr = PointerType(in: &self)
    return StructType([ptr, ptr], in: &self)
  }

  /// Returns the prototype of subscript slides.
  private mutating func slidePrototype() -> LLVM.Function {
    if let f = function(named: "_val_slide") {
      return f
    }

    let f = declareFunction(
      "_val_slide",
      FunctionType(
        from: [PointerType(in: &self), IntegerType(1, in: &self)],
        to: VoidType(in: &self),
        in: &self))
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
      FunctionType(from: [IntegerType(32, in: &self)], to: PointerType(in: &self), in: &self))
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
      FunctionType(from: [PointerType(in: &self)], to: VoidType(in: &self), in: &self))
    addAttribute(named: .noundef, to: f.parameters[0])

    return f
  }

  /// Returns the type of a transpiled function whose type in Val is `t`.
  ///
  /// - Note: the type of a function in Val IR typically doesn't match the type of its transpiled
  ///   form 1-to-1, as return values are often passed by references.
  private mutating func transpiledType(_ t: LambdaType) -> LLVM.FunctionType {
    var parameters: Int = t.inputs.count

    // Return values are passed by reference.
    if !t.output.isVoidOrNever {
      parameters += 1
    }

    // Environments are passed before explicit arguments.
    if t.environment != .void {
      parameters += 1
    }

    return .init(
      from: Array(repeating: LLVM.PointerType(in: &self), count: parameters),
      to: VoidType(in: &self),
      in: &self)
  }

  /// Returns the LLVM IR value corresponding to the Val IR constant `c` when used in `m` in `ir`.
  private mutating func transpiledConstant(
    _ c: any IR.Constant,
    usedIn m: IR.Module,
    from ir: LoweredProgram
  ) -> LLVM.IRValue {
    switch c {
    case let v as IR.IntegerConstant:
      guard v.value.bitWidth <= 64 else { fatalError("not implemented") }
      let t = LLVM.IntegerType(v.value.bitWidth, in: &self)
      return t.constant(UInt64(v.value.words[0]))

    case let v as IR.FloatingPointConstant:
      let t = LLVM.FloatingPointType(ir.llvm(c.type.ast, in: &self))!
      return t.constant(parsing: v.value)

    case let v as IR.BufferConstant:
      return LLVM.ArrayConstant(bytes: v.contents, in: &self)

    case let v as IR.MetatypeConstant:
      return transpiledMetatype(of: v.value.instance, usedIn: m, from: ir)

    case let v as IR.WitnessTable:
      return transpiledWitnessTable(v, usedIn: m, from: ir)

    case let v as IR.PointerConstant:
      return global(named: "\(v.container)\(v.id)")!

    case let v as IR.FunctionReference:
      return declare(v, from: ir)

    case is IR.Poison:
      let t = ir.llvm(c.type.ast, in: &self)
      return LLVM.Poison(of: t)

    case is IR.VoidConstant:
      fatalError("not implemented")

    default:
      unreachable()
    }
  }

  /// Returns the LLVM IR value of the witness table `t` used in `m` in `ir`.
  private mutating func transpiledWitnessTable(
    _ t: WitnessTable,
    usedIn m: IR.Module,
    from ir: LoweredProgram
  ) -> LLVM.IRValue {
    let ptr = LLVM.PointerType(in: &self)
    let word = wordType()

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
      word.constant(UInt64(t.conformances.count)),
    ]

    // Encode the table's trait and implementation maps.
    var entries: [LLVM.IRValue] = []
    var implementations: [LLVM.IRValue] = []
    for c in t.conformances {
      let entry: [LLVM.IRValue] = [
        transpiledMetatype(of: c.concept, usedIn: m, from: ir),
        word.constant(UInt64(implementations.count)),
      ]
      entries.append(LLVM.StructConstant(aggregating: entry, in: &self))

      for (r, d) in c.implementations.storage {
        let requirement: [LLVM.IRValue] = [
          word.constant(UInt64(r.rawValue)),
          transpiledRequirementImplementation(d, from: ir),
        ]
        implementations.append(LLVM.StructConstant(aggregating: requirement, in: &self))
      }
    }

    // Append the sentinel at the end of the trait map.
    entries.append(
      LLVM.StructConstant(
        aggregating: [ptr.null, word.constant(UInt64(implementations.count))], in: &self))

    // Put everything together.
    tableContents.append(
      LLVM.ArrayConstant(
        of: LLVM.StructType([ptr, word], in: &self), containing: entries, in: &self))

    if !implementations.isEmpty {
      tableContents.append(
        LLVM.ArrayConstant(
          of: LLVM.StructType([word, ptr], in: &self), containing: implementations, in: &self))
    }

    let table = LLVM.StructConstant(aggregating: tableContents, in: &self)

    let g = declareGlobalVariable(ir.mangle(t), table.type)
    setInitializer(table, for: g)
    setLinkage(.linkOnce, for: g)
    setGlobalConstant(true, for: g)
    return g
  }

  /// Returns the LLVM IR value of the requirement implementation `i`, which is in `ir`.
  private mutating func transpiledRequirementImplementation(
    _ i: IR.LoweredConformance.Implementation, from ir: LoweredProgram
  ) -> LLVM.Function {
    switch i {
    case .function(let f):
      return declare(f, from: ir)
    case .value:
      fatalError("not implemented")
    }
  }

  /// Returns the LLVM IR value of the metatype `t` used in `m` in `ir`.
  private mutating func transpiledMetatype(
    of t: AnyType,
    usedIn m: IR.Module,
    from ir: LoweredProgram
  ) -> LLVM.GlobalVariable {
    switch t.base {
    case let u as ProductType:
      return transpiledMetatype(of: u, usedIn: m, from: ir)
    case let u as TraitType:
      return transpiledMetatype(of: u, usedIn: m, from: ir)
    default:
      fatalError("not implemented")
    }
  }

  /// Returns the LLVM IR value of the metatype `t` used in `m` in `ir`.
  private mutating func transpiledMetatype(
    of t: ProductType,
    usedIn m: IR.Module,
    from ir: LoweredProgram
  ) -> LLVM.GlobalVariable {
    // Check if we already created the metatype's instance.
    let n = ir.mangle(t)
    if let g = global(named: n) { return g }

    let metatype = metatypeType()
    let instance = declareGlobalVariable(n, metatype)

    // Initialize the instance if it's being used in the module defining `t`.
    if m.syntax.id != ir.syntax.module(containing: t.decl) {
      return instance
    }

    // TODO: compute size, alignment, and representation
    setInitializer(metatype.null, for: instance)
    setGlobalConstant(true, for: instance)
    return instance
  }

  /// Returns the LLVM IR value of the metatype `t` used in `m` in `ir`.
  private mutating func transpiledMetatype(
    of t: TraitType,
    usedIn m: IR.Module,
    from ir: LoweredProgram
  ) -> LLVM.GlobalVariable {
    // Check if we already created the metatype's instance.
    let n = ir.mangle(t)
    if let g = global(named: n) { return g }

    let instance = declareGlobalVariable(n, LLVM.PointerType(in: &self))

    // Initialize the instance if it's being used in the module defining `t`.
    if m.syntax.id != ir.syntax.module(containing: t.decl) {
      return instance
    }

    let s = LLVM.StringConstant(n, nullTerminated: true, in: &self)
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
    _ ref: IR.FunctionReference, from ir: LoweredProgram
  ) -> LLVM.Function {
    let t = transpiledType(LambdaType(ref.type.ast)!)
    return declareFunction(ir.mangle(ref.function), t)
  }

  /// Inserts and returns the transpiled declaration of `f`, which is a function of `m` in `ir`.
  private mutating func declare(
    function f: IR.Function.ID, of m: IR.Module, from ir: LoweredProgram
  ) -> LLVM.Function {
    precondition(!m[f].isSubscript)

    // Parameters and return values are passed by reference.
    let ptr = LLVM.PointerType(in: &self)
    var parameters: [LLVM.IRType] = []
    if !m[f].output.isVoidOrNever {
      parameters.append(ptr)
    }
    parameters.append(contentsOf: Array(repeating: ptr, count: m[f].inputs.count))
    let result = declareFunction(ir.mangle(f), .init(from: parameters, in: &self))

    if m[f].output == .never {
      addAttribute(.init(.noreturn, in: &self), to: result)
    }

    return result
  }

  /// Inserts and returns the transpiled declaration of `f`, which is a subscript of `m` in `ir`.
  private mutating func declare(
    subscript f: IR.Function.ID, of m: IR.Module, from ir: LoweredProgram
  ) -> LLVM.Function {
    precondition(m[f].isSubscript)

    // Parameters are a buffer for the subscript frame followed by its declared parameters. Return
    // type is a pair `(c, p)` where `c` points to a subscript slide and `p` is the address of the
    // projected value.
    let ptr = LLVM.PointerType(in: &self)
    let ret = LLVM.StructType([ptr, ptr], in: &self)
    let parameters = Array(repeating: ptr, count: m[f].inputs.count + 1)
    let coroutine = declareFunction(ir.mangle(f), .init(from: parameters, to: ret, in: &self))

    return coroutine
  }

  /// Inserts into `transpilation `the transpiled contents of `f`, which is a function or subscript
  /// of `m` in `ir`.
  private mutating func transpile(
    contentsOf f: IR.Function.ID,
    of m: IR.Module,
    from ir: LoweredProgram,
    into transpilation: LLVM.Function
  ) {
    /// The function's entry.
    guard let entry = m[f].entry else { return }

    /// Where new LLVM IR instruction are inserted.
    var insertionPoint: LLVM.InsertionPoint!

    /// A map from Val IR basic block to its LLVM counterpart.
    var block: [IR.Block.ID: LLVM.BasicBlock] = [:]

    /// A map from Val IR register to its LLVM counterpart.
    var register: [IR.Operand: LLVM.IRValue] = [:]

    /// The address of the function's frame if `f` is a subscript. Otherwise, `nil`.
    let frame: LLVM.IRValue?

    /// The prologue of the transpiled function, which contains its stack allocations.
    let prologue = appendBlock(named: "prologue", to: transpilation)

    let parameterOffset: Int
    if m[f].isSubscript {
      // In subscripts, parameters are laid out after the frame buffer.
      parameterOffset = 1
      frame = insertSubscriptPrologue(into: transpilation)
    } else {
      // In functions, parameters are laid out after the return value.
      parameterOffset = m[f].output.isVoidOrNever ? 0 : 1
      frame = nil
    }

    for i in m[f].inputs.indices {
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
      case is IR.AddressToPointerInstruction:
        insert(addressToPointer: i)
      case is IR.AllocStackInstruction:
        insert(allocStack: i)
      case is IR.BorrowInstruction:
        insert(borrow: i)
      case is IR.BranchInstruction:
        insert(branch: i)
      case is IR.CallInstruction:
        insert(call: i)
      case is IR.CallFIIInstruction:
        insert(callFFI: i)
      case is IR.CondBranchInstruction:
        insert(condBranch: i)
      case is IR.DeallocStackInstruction:
        return
      case is IR.DeinitInstruction:
        return
      case is IR.DestructureInstruction:
        insert(destructure: i)
      case is IR.ElementAddrInstruction:
        insert(elementAddr: i)
      case is IR.EndBorrowInstruction:
        return
      case is IR.EndProjectInstruction:
        insert(endProjection: i)
      case is IR.LLVMInstruction:
        insert(llvm: i)
      case is IR.LoadInstruction:
        insert(load: i)
      case is IR.PartialApplyInstruction:
        insert(partialApply: i)
      case is IR.ProjectInstruction:
        insert(project: i)
      case is IR.RecordInstruction:
        insert(record: i)
      case is IR.ReturnInstruction:
        insert(return: i)
      case is IR.StoreInstruction:
        insert(store: i)
      case is IR.UnrechableInstruction:
        insert(unreachable: i)
      case is IR.WrapAddrInstruction:
        insert(wrapAddr: i)
      case is IR.YieldInstruction:
        insert(yield: i)
      default:
        fatalError("not implemented")
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(addressToPointer i: IR.InstructionID) {
      let s = m[i] as! AddressToPointerInstruction
      register[.register(i, 0)] = llvm(s.source)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(allocStack i: IR.InstructionID) {
      let s = m[i] as! AllocStackInstruction
      let t = ir.llvm(s.allocatedType, in: &self)
      register[.register(i, 0)] = insertAlloca(t, atEntryOf: transpilation)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(borrow i: IR.InstructionID) {
      let s = m[i] as! BorrowInstruction
      register[.register(i, 0)] = llvm(s.location)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(branch i: IR.InstructionID) {
      let s = m[i] as! BranchInstruction
      insertBr(to: block[s.target]!, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(call i: IR.InstructionID) {
      let s = m[i] as! CallInstruction
      var arguments: [LLVM.IRValue] = []

      // Return value is passed by reference.
      let returnType: LLVM.IRType?
      if s.types[0].ast.isVoidOrNever {
        returnType = nil
      } else {
        returnType = ir.llvm(s.types[0].ast, in: &self)
        arguments.append(insertAlloca(returnType!, atEntryOf: transpilation))
      }

      // Callee is evaluated first.
      let callee: LLVM.IRValue
      let calleeType: LLVM.IRType
      if case .constant(let f) = s.callee {
        callee = transpiledConstant(f, usedIn: m, from: ir)
        calleeType = LLVM.Function(callee)!.valueType
      } else {
        let c = unpackLambda(s.callee)
        callee = c.function
        calleeType = c.type

        if let e = c.environment {
          arguments.append(e)
        }
      }

      // All arguments are passed by reference.
      for a in s.arguments {
        if m.type(of: a).isObject {
          let t = ir.llvm(s.types[0].ast, in: &self)
          let l = insertAlloca(t, atEntryOf: transpilation)
          insertStore(llvm(a), to: l, at: insertionPoint)
          arguments.append(l)
        } else {
          arguments.append(llvm(a))
        }
      }

      _ = insertCall(callee, typed: calleeType, on: arguments, at: insertionPoint)

      // Load the return value if necessary.
      if let t = returnType {
        register[.register(i, 0)] = insertLoad(t, from: arguments[0], at: insertionPoint)
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(callFFI i: IR.InstructionID) {
      let s = m[i] as! CallFIIInstruction
      let parameters = s.operands.map({ ir.llvm(m.type(of: $0).ast, in: &self) })

      let returnType: IRType
      if s.returnType.ast.isVoidOrNever {
        returnType = LLVM.VoidType(in: &self)
      } else {
        returnType = ir.llvm(s.returnType.ast, in: &self)
      }

      let callee = declareFunction(s.callee, .init(from: parameters, to: returnType, in: &self))
      let arguments = s.operands.map({ llvm($0) })
      register[.register(i, 0)] = insertCall(callee, on: arguments, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(condBranch i: IR.InstructionID) {
      let s = m[i] as! CondBranchInstruction
      let c = llvm(s.condition)
      insertCondBr(
        if: c, then: block[s.targetIfTrue]!, else: block[s.targetIfFalse]!,
        at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(destructure i: IR.InstructionID) {
      let s = m[i] as! DestructureInstruction
      let whole = llvm(s.whole)
      for j in s.types.indices {
        register[.register(i, j)] = insertExtractValue(from: whole, at: j, at: insertionPoint)
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(elementAddr i: IR.InstructionID) {
      let s = m[i] as! ElementAddrInstruction
      let t = LLVM.IntegerType(32, in: &self)

      let base = llvm(s.base)
      let baseType = ir.llvm(m.type(of: s.base).ast, in: &self)
      let indices = [t.constant(0)] + s.elementPath.map({ t.constant(UInt64($0)) })
      let v = insertGetElementPointerInBounds(
        of: base, typed: baseType, indices: indices, at: insertionPoint)
      register[.register(i, 0)] = v
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(endProjection i: IR.InstructionID) {
      let s = m[i] as! EndProjectInstruction
      let start = s.projection.instruction!
      assert(m[start] is ProjectInstruction)

      let i1 = LLVM.IntegerType(1, in: &self)
      let t = LLVM.FunctionType(
        from: [LLVM.PointerType(in: &self), i1],
        to: LLVM.VoidType(in: &self),
        in: &self)

      let slide = register[.register(start, 1)]!
      let buffer = register[.register(start, 2)]!
      _ = insertCall(slide, typed: t, on: [buffer, i1.zero], at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(llvm i: IR.InstructionID) {
      let s = m[i] as! IR.LLVMInstruction
      switch s.instruction {
      case .add(let p, _):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i, 0)] = insertAdd(overflow: p, l, r, at: insertionPoint)

      case .sub(let p, _):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i, 0)] = insertSub(overflow: p, l, r, at: insertionPoint)

      case .mul(let p, _):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i, 0)] = insertMul(overflow: p, l, r, at: insertionPoint)

      case .icmp(let p, _):
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i, 0)] = insertIntegerComparison(p, l, r, at: insertionPoint)

      case .trunc(_, let t):
        let target = ir.llvm(builtinType: t, in: &self)
        let source = llvm(s.operands[0])
        register[.register(i, 0)] = insertTrunc(source, to: target, at: insertionPoint)

      case .inttoptr(_):
        let source = llvm(s.operands[0])
        register[.register(i, 0)] = insertIntToPtr(source, at: insertionPoint)

      case .ptrtoint(let t):
        let target = ir.llvm(builtinType: t, in: &self)
        let source = llvm(s.operands[0])
        register[.register(i, 0)] = insertPtrToInt(source, to: target, at: insertionPoint)

      case .fptrunc(_, let t):
        let target = ir.llvm(builtinType: t, in: &self)
        let source = llvm(s.operands[0])
        register[.register(i, 0)] = insertFPTrunc(source, to: target, at: insertionPoint)

      case .zeroinitializer(let t):
        register[.register(i, 0)] = ir.llvm(builtinType: t, in: &self).null

      default:
        unreachable("unexpected LLVM instruction '\(s.instruction)'")
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(load i: IR.InstructionID) {
      let s = m[i] as! LoadInstruction
      let t = ir.llvm(s.objectType.ast, in: &self)
      let source = llvm(s.source)
      register[.register(i, 0)] = insertLoad(t, from: source, at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(partialApply i: IR.InstructionID) {
      let s = m[i] as! IR.PartialApplyInstruction
      let t = LambdaType(s.function.type.ast)!

      if t.environment == .void {
        register[.register(i, 0)] = transpiledConstant(s.function, usedIn: m, from: ir)
      } else {
        fatalError("not implemented")
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(project i: IR.InstructionID) {
      let s = m[i] as! IR.ProjectInstruction

      // %0 = alloca [8 x i8], align 8
      let buffer = LLVM.ArrayType(8, LLVM.IntegerType(8, in: &self), in: &self)
      let x0 = insertAlloca(buffer, at: insertionPoint)
      setAlignment(8, for: x0)

      // All arguments are passed by reference.
      var arguments: [LLVM.IRValue] = [x0]
      for a in s.operands {
        if m.type(of: a).isObject {
          let t = ir.llvm(s.types[0].ast, in: &self)
          let l = insertAlloca(t, atEntryOf: transpilation)
          insertStore(llvm(a), to: l, at: insertionPoint)
          arguments.append(l)
        } else {
          arguments.append(llvm(a))
        }
      }

      // %1 = call ptr @llvm.coro.prepare.retcon(ptr @s)
      let f = declare(subscript: s.callee, of: m, from: ir)
      let prepare = intrinsic(named: Intrinsic.llvm.coro.prepare.retcon)!
      let x1 = insertCall(LLVM.Function(prepare)!, on: [f], at: insertionPoint)

      // %2 = call {ptr, ptr} %1(...)
      let x2 = insertCall(x1, typed: f.valueType, on: arguments, at: insertionPoint)

      register[.register(i, 0)] = insertExtractValue(from: x2, at: 1, at: insertionPoint)
      register[.register(i, 1)] = insertExtractValue(from: x2, at: 0, at: insertionPoint)
      register[.register(i, 2)] = x0
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(record i: IR.InstructionID) {
      let s = m[i] as! IR.RecordInstruction
      let t = ir.llvm(s.objectType.ast, in: &self)
      var record: LLVM.IRValue = LLVM.Undefined(of: t)
      for (i, part) in s.operands.enumerated() {
        let v = llvm(part)
        record = insertInsertValue(v, at: i, into: record, at: insertionPoint)
      }
      register[.register(i, 0)] = record
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(return i: IR.InstructionID) {
      if m[f].isSubscript {
        _ = insertCall(
          LLVM.Function(intrinsic(named: Intrinsic.llvm.coro.end)!)!,
          on: [frame!, IntegerType(1, in: &self).zero],
          at: insertionPoint)
        _ = insertUnreachable(at: insertionPoint)
      } else {
        let s = m[i] as! IR.ReturnInstruction
        if !m.type(of: s.object).ast.isVoidOrNever {
          insertStore(llvm(s.object), to: transpilation.parameters[0], at: insertionPoint)
        }
        insertReturn(at: insertionPoint)
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(store i: IR.InstructionID) {
      let s = m[i] as! IR.StoreInstruction
      insertStore(llvm(s.object), to: llvm(s.target), at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(unreachable i: IR.InstructionID) {
      insertUnreachable(at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(wrapAddr i: IR.InstructionID) {
      let s = m[i] as! IR.WrapAddrInstruction
      let t = containerType()
      let a = insertAlloca(t, atEntryOf: transpilation)
      insertStore(container(witness: s.witness, table: s.table), to: a, at: insertionPoint)
      register[.register(i, 0)] = a
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(yield i: IR.InstructionID) {
      let s = m[i] as! IR.YieldInstruction
      let p = llvm(s.projection)

      // The intrinsic will return a non-zero result if the subscript should resume abnormally.
      let i1 = IntegerType(1, in: &self)
      _ = insertCall(
        LLVM.Function(intrinsic(named: Intrinsic.llvm.coro.suspend.retcon, for: [i1])!)!,
        on: [p],
        at: insertionPoint)
    }

    /// Returns the LLVM IR value corresponding to the Val IR operand `o`.
    func llvm(_ o: IR.Operand) -> LLVM.IRValue {
      if case .constant(let c) = o {
        return transpiledConstant(c, usedIn: m, from: ir)
      } else {
        return register[o]!
      }
    }

    /// Returns the contents of the lambda `o`.
    func unpackLambda(_ o: Operand) -> LambdaContents {
      let virType = m.type(of: o)
      let valType = LambdaType(virType.ast)!

      let lambda: LLVM.IRValue
      if virType.isObject {
        lambda = llvm(o)
      } else {
        let t = LLVM.PointerType(in: &self)
        lambda = insertLoad(t, from: llvm(o), at: insertionPoint)
      }

      let llvmType = transpiledType(valType)
      if valType.environment == .void {
        return .init(function: lambda, type: llvmType, environment: nil)
      } else {
        fatalError("not implemented")
      }
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
    let i32 = IntegerType(32, in: &self)
    let id = insertCall(
      LLVM.Function(intrinsic(named: Intrinsic.llvm.coro.id.retcon.once)!)!,
      on: [
        i32.constant(8), i32.constant(8), transpilation.parameters[0],
        slidePrototype(), mallocPrototype(), freePrototype(),
      ],
      at: insertionPoint)
    return insertCall(
      LLVM.Function(intrinsic(named: Intrinsic.llvm.coro.begin)!)!,
      on: [id, PointerType(in: &self).null],
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

  /// A pointer to the lambda's environment, if any.
  let environment: LLVM.IRValue?

}
