import Core
import IR
import LLVM
import Utils

extension LLVM.Module {

  /// Transpiles and incorporates `g`, which is a function of `m` in `ir`.
  mutating func incorporate(_ g: IR.Module.GlobalID, of m: IR.Module, from ir: LoweredProgram) {
    let p = PointerConstant(m.syntax.id, g)
    let v = transpiledConstant(m.globals[g], usedIn: m, from: ir)
    let d = declareGlobalVariable(p.description, v.type)
    setInitializer(v, for: d)
  }

  /// Transpiles and incorporates `f`, which is a function of `m` in `ir`.
  mutating func incorporate(_ f: IR.Function.ID, of m: IR.Module, from ir: LoweredProgram) {
    let d = declare(f, of: m, from: ir)
    transpile(contentsOf: f, of: m, from: ir, into: d)

    if f == m.entryFunctionID {
      defineMain(calling: f, of: m, from: ir)
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

    let transpilation = function(named: ir.abiName(of: f))!

    let val32 = ir.syntax.ast.coreType(named: "Int32")!
    switch m[f].output.ast {
    case val32:
      let t = StructType(ir.syntax.llvm(val32, in: &self))!
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
    _ c: IR.Constant,
    usedIn m: IR.Module,
    from ir: LoweredProgram
  ) -> LLVM.IRValue {
    switch c {
    case .integer(let v):
      guard v.value.bitWidth <= 64 else { fatalError("not implemented") }
      let t = LLVM.IntegerType(v.value.bitWidth, in: &self)
      return t.constant(UInt64(v.value.words[0]))

    case .floatingPoint(let v):
      let t = LLVM.FloatingPointType(ir.syntax.llvm(c.type.ast, in: &self))!
      return t.constant(parsing: v.value)

    case .buffer(let v):
      return LLVM.ArrayConstant(bytes: v.contents, in: &self)

    case .metatype(let v):
      return transpiledMetatype(of: v.value.instance, usedIn: m, from: ir)

    case .witnessTable(let v):
      return transpiledWitnessTable(v, usedIn: m, from: ir)

    case .pointer(let v):
      return global(named: v.description)!

    case .function(let v):
      return declare(v, from: ir)

    case .poison:
      let t = ir.syntax.llvm(c.type.ast, in: &self)
      return LLVM.Poison(of: t)

    case .void:
      fatalError("not implemented")

    case .builtin:
      unreachable()
    }
  }

  /// Returns the LLVM IR value of the witness table `t` used in `m` in `ir`.
  private mutating func transpiledWitnessTable(
    _ t: WitnessTable,
    usedIn m: IR.Module,
    from ir: LoweredProgram
  ) -> LLVM.IRValue {
    // TODO: Handle conformances.
    transpiledMetatype(of: t.witness, usedIn: m, from: ir)
  }

  /// Returns the LLVM IR value of the metatype `t` used in `m` in `ir`.
  private mutating func transpiledMetatype(
    of t: AnyType,
    usedIn m: IR.Module,
    from ir: LoweredProgram
  ) -> LLVM.GlobalVariable {
    let p = ProductType(t) ?? fatalError("not implemented")
    let n = ir.syntax.abiName(of: p.decl) + ".metatype"

    // Check if we already created the metatype's instance.
    if let g = global(named: n) { return g }

    // Create a new instance.
    let metatype = metatypeType()
    let instance = declareGlobalVariable(n, metatype)

    // Initialize the instance if it's being used in the module defining `t`.
    if m.syntax.id != ir.syntax.module(containing: p.decl) { return instance }

    // TODO: compute size, alignment, and representation
    setInitializer(metatype.null, for: instance)
    return instance
  }

  /// Inserts and returns the transpiled declaration of `ref`, which is in `ir`.
  private mutating func declare(_ ref: IR.FunctionRef, from ir: LoweredProgram) -> LLVM.Function {
    let t = transpiledType(LambdaType(ref.type.ast)!)
    return declareFunction(ir.abiName(of: ref.function), t)
  }

  /// Inserts and returns the transpiled declaration of `f`, which is a function of `m` in `ir`.
  private mutating func declare(
    _ f: IR.Function.ID, of m: IR.Module, from ir: LoweredProgram
  ) -> LLVM.Function {
    let ptr = LLVM.PointerType(in: &self)

    // Parameters and return values are passed by reference.
    var parameters: [LLVM.IRType] = []
    if !m[f].output.ast.isVoidOrNever {
      parameters.append(ptr)
    }
    parameters.append(contentsOf: Array(repeating: ptr, count: m[f].inputs.count))
    let result = declareFunction(ir.abiName(of: f), .init(from: parameters, in: &self))

    if m[f].output.ast == .never {
      addAttribute(.init(.noreturn, in: &self), to: result)
    }

    return result
  }

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

    let prologue = appendBlock(named: "prologue", to: transpilation)
    insertionPoint = endOf(prologue)

    let parameterOffset = m[f].output.ast.isVoidOrNever ? 0 : 1
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
      case is IR.LLVMInstruction:
        insert(llvm: i)
      case is IR.LoadInstruction:
        insert(load: i)
      case is IR.PartialApplyInstruction:
        insert(partialApply: i)
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
      default:
        fatalError("not implemented")
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(allocStack i: IR.InstructionID) {
      let s = m[i] as! AllocStackInstruction
      let t = ir.syntax.llvm(s.allocatedType, in: &self)
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
        returnType = ir.syntax.llvm(s.types[0].ast, in: &self)
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
          let t = ir.syntax.llvm(s.types[0].ast, in: &self)
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
      let parameters = s.operands.map({ ir.syntax.llvm(m.type(of: $0).ast, in: &self) })

      let returnType: IRType
      if s.returnType.ast.isVoidOrNever {
        returnType = LLVM.VoidType(in: &self)
      } else {
        returnType = ir.syntax.llvm(s.returnType.ast, in: &self)
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
      let baseType = ir.syntax.llvm(m.type(of: s.base).ast, in: &self)
      let indices = [t.constant(0)] + s.elementPath.map({ t.constant(UInt64($0)) })
      let v = insertGetElementPointerInBounds(
        of: base, typed: baseType, indices: indices, at: insertionPoint)
      register[.register(i, 0)] = v
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(llvm i: IR.InstructionID) {
      let s = m[i] as! IR.LLVMInstruction
      switch s.function.llvmInstruction {
      case "add":
        let (o, l, r) = integerArithmeticOperands(s)
        register[.register(i, 0)] = insertAdd(overflow: o, l, r, at: insertionPoint)

      case "sub":
        let (o, l, r) = integerArithmeticOperands(s)
        register[.register(i, 0)] = insertSub(overflow: o, l, r, at: insertionPoint)

      case "mul":
        let (o, l, r) = integerArithmeticOperands(s)
        register[.register(i, 0)] = insertMul(overflow: o, l, r, at: insertionPoint)

      case "icmp":
        let p = LLVM.IntegerPredicate(s.function.genericParameters[0])!
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        register[.register(i, 0)] = insertIntegerComparison(p, l, r, at: insertionPoint)

      case "trunc":
        let target = ir.syntax.llvm(s.types[0].ast, in: &self)
        let source = llvm(s.operands[0])
        register[.register(i, 0)] = insertTrunc(source, to: target, at: insertionPoint)

      case "fptrunc":
        let target = ir.syntax.llvm(s.types[0].ast, in: &self)
        let source = llvm(s.operands[0])
        register[.register(i, 0)] = insertFPTrunc(source, to: target, at: insertionPoint)

      case "zeroinitializer":
        let t = ir.syntax.llvm(s.types[0].ast, in: &self)
        register[.register(i, 0)] = t.null

      default:
        unreachable("unexpected LLVM instruction '\(s.function.llvmInstruction)'")
      }

      /// Returns the overflow behavior and operands defined of `s`.
      func integerArithmeticOperands(
        _ s: IR.LLVMInstruction
      ) -> (overflow: LLVM.OverflowBehavior, lhs: LLVM.IRValue, rhs: LLVM.IRValue) {
        let o = LLVM.OverflowBehavior(s.function.genericParameters)!
        let l = llvm(s.operands[0])
        let r = llvm(s.operands[1])
        return (o, l, r)
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(load i: IR.InstructionID) {
      let s = m[i] as! LoadInstruction
      let t = ir.syntax.llvm(s.objectType.ast, in: &self)
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
    func insert(record i: IR.InstructionID) {
      let s = m[i] as! IR.RecordInstruction
      let t = ir.syntax.llvm(s.objectType.ast, in: &self)
      var record: LLVM.IRValue = LLVM.Undefined(of: t)
      for (i, part) in s.operands.enumerated() {
        let v = llvm(part)
        record = insertInsertValue(v, at: i, into: record, at: insertionPoint)
      }
      register[.register(i, 0)] = record
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(return i: IR.InstructionID) {
      let s = m[i] as! IR.ReturnInstruction
      if !m.type(of: s.object).ast.isVoidOrNever {
        insertStore(llvm(s.object), to: transpilation.parameters[0], at: insertionPoint)
      }
      insertReturn(at: insertionPoint)
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

}

extension LLVMProgram: CustomStringConvertible {

  public var description: String { "\(list: llvmModules, joinedBy: "\n")" }

}

extension LLVM.OverflowBehavior {

  fileprivate init?(_ parameters: [String]) {
    guard parameters.count <= 1 else { return nil }
    guard let p = parameters.first else {
      self = .ignore
      return
    }

    switch p {
    case "nsw":
      self = .nsw
    case "nuw":
      self = .nuw
    default:
      return nil
    }
  }

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
