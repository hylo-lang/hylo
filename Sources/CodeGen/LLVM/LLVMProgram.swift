import Core
import Foundation
import IR
import LLVM
import Utils

/// A Val program transpiled to LLVM.
public struct LLVMProgram {

  /// The LLVM modules in the program.
  public private(set) var llvmModules: [ModuleDecl.ID: LLVM.Module] = [:]

  /// Creates an empty program.
  public init(_ ir: LoweredProgram, mainModule: ModuleDecl.ID) throws {
    for m in ir.modules.keys {
      let transpilation = transpiled(m, from: ir)
      do {
        try transpilation.verify()
      } catch {
        print(transpilation)
        throw error
      }
      llvmModules[m] = transpilation
    }
  }

  public func write(_ type: LLVM.CodeGenerationResultType, to directory: URL) throws -> [URL] {
    precondition(directory.hasDirectoryPath)

    let host = try LLVM.Target.host()
    let machine = LLVM.TargetMachine(for: host)
    var result: [URL] = []
    for m in llvmModules.values {
      let f = directory.appendingPathComponent(m.name).appendingPathExtension("o")
      try m.write(type, for: machine, to: f.path)
      result.append(f)
    }

    return result
  }

  /// The LLVM transpilation of the Val IR module `ir`.
  private func transpiled(_ module: ModuleDecl.ID, from ir: LoweredProgram) -> LLVM.Module {
    let m = ir.modules[module]!
    var transpilation = LLVM.Module(m.name)
    for f in m.functions.keys {
      transpilation.incorporate(f, of: m, from: ir)
    }
    return transpilation
  }

}

extension LLVM.Module {

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

  /// Inserts and returns the transpiled declaration of `ref`, which is in `ir`.
  private mutating func declare(_ ref: IR.FunctionRef, from ir: LoweredProgram) -> LLVM.Function {
    // Determine the name of the transpilation.
    let name: String
    if case .lowered(let d) = ref.function.value,
      let n = ir.syntax.ast[FunctionDecl.ID(d)]?.foreignName
    {
      name = n
    } else {
      name = ir.abiName(of: ref.function)
    }

    let t = transpiledType(LambdaType(ref.type.ast)!)
    return declareFunction(name, t)
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
      switch s.types[0].ast {
      case .void, .never:
        returnType = nil
      default:
        returnType = ir.syntax.llvm(s.types[0].ast, in: &self)
        arguments.append(insertAlloca(returnType!, atEntryOf: transpilation))
      }

      // Callee is evaluated first.
      let callee: LLVM.IRValue
      let calleeType: LLVM.IRType
      if case .constant(let f) = s.callee {
        callee = llvm(f)
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
        register[.register(i, 0)] = llvm(s.function)
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
      if m.type(of: s.object).ast != .void {
        insertStore(llvm(s.object), to: transpilation.parameters[0], at: insertionPoint)
      }
      insertReturn(at: insertionPoint)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(store i: IR.InstructionID) {
      let s = m[i] as! IR.StoreInstruction
      insertStore(llvm(s.object), to: llvm(s.target), at: insertionPoint)
    }

    /// Returns the LLVM IR value corresponding to the Val IR operand `o`.
    func insert(unreachable i: IR.InstructionID) {
      insertUnreachable(at: insertionPoint)
    }

    /// Returns the LLVM IR value corresponding to the Val IR operand `o`.
    func llvm(_ o: IR.Operand) -> LLVM.IRValue {
      if case .constant(let c) = o {
        return llvm(c)
      } else {
        return register[o]!
      }
    }

    /// Returns the LLVM IR value corresponding to the Val IR constant `c`.
    func llvm(_ c: IR.Constant) -> LLVM.IRValue {
      switch c {
      case .integer(let n):
        guard n.value.bitWidth <= 64 else { fatalError("not implemented") }
        let t = LLVM.IntegerType(n.value.bitWidth, in: &self)
        return t.constant(UInt64(n.value.words[0]))

      case .floatingPoint(let n):
        let t = LLVM.FloatingPointType(ir.syntax.llvm(c.type.ast, in: &self))!
        return t.constant(parsing: n.value)

      case .pointer(let p):
        return declareGlobalVariable(p.description, PointerType(in: &self))

      case .function(let f):
        return declare(f, from: ir)

      case .poison:
        let t = ir.syntax.llvm(c.type.ast, in: &self)
        return LLVM.Poison(of: t)

      default:
        fatalError("not implemented")
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
