import Core
import IR
import LLVM
import Utils

/// A Val program transpiled to LLVM.
public struct LLVMProgram {

  /// The LLVM modules in the program.
  private var llvmModules: [ModuleDecl.ID: LLVM.Module] = [:]

  /// Creates an empty program.
  public init(_ ir: LoweredProgram, mainModule: ModuleDecl.ID) throws {
    let transpilation = transpiled(mainModule, from: ir)
    try transpilation.verify()
    llvmModules[mainModule] = transpilation
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
  }

  /// Inserts and returns the transpiled declaration of `ref`, which is in `ir`.
  private mutating func declare(_ ref: IR.FunctionRef, from ir: LoweredProgram) -> LLVM.Function {
    let ptr = LLVM.PointerType(in: &self)

    // Parameters and return values are passed by reference.
    let functionType = LambdaType(ref.type.astType)!
    var parameters: [LLVM.IRType] = []
    if functionType.output != .void {
      parameters.append(ptr)
    }
    parameters.append(contentsOf: Array(repeating: ptr, count: functionType.inputs.count))
    return declareFunction(ir.abiName(of: ref.function), .init(from: parameters, in: &self))
  }

  /// Inserts and returns the transpiled declaration of `f`, which is a function of `m` in `ir`.
  private mutating func declare(
    _ f: IR.Function.ID, of m: IR.Module, from ir: LoweredProgram
  ) -> LLVM.Function {
    let ptr = LLVM.PointerType(in: &self)

    // Parameters and return values are passed by reference.
    var parameters: [LLVM.IRType] = []
    if m[f].output.astType != .void {
      parameters.append(ptr)
    }
    parameters.append(contentsOf: Array(repeating: ptr, count: m[f].inputs.count))
    return declareFunction(ir.abiName(of: f), .init(from: parameters, in: &self))
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

    let parameterOffset = (m[f].output.astType == .void) ? 0 : 1
    for (i, p) in m[f].inputs.enumerated() {
      let o = Operand.parameter(.init(f, entry), i)
      let s = transpilation.parameters[parameterOffset + i]

      if p.convention == .sink {
        let t = ir.syntax.llvm(p.type.astType, in: &self)
        register[o] = insertLoad(t, from: s, at: insertionPoint)
      } else {
        register[o] = s
      }
    }

    for b in m.blocks(in: f) {
      let transpiledBlock = appendBlock(named: b.description, to: transpilation)
      block[b] = transpiledBlock

      insertionPoint = endOf(transpiledBlock)
      for i in m.instructions(in: b) {
        insert(i)
      }
    }

    insertBr(to: block[.init(f, entry)]!, at: endOf(prologue))

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(_ i: IR.InstructionID) {
      switch m[i] {
      case is IR.BorrowInstruction:
        insert(borrow: i)
      case is IR.CallInstruction:
        insert(call: i)
      case is IR.ElementAddrInstruction:
        insert(elementAddr: i)
      case is IR.EndBorrowInstruction:
        return
      case is IR.LLVMInstruction:
        insert(llvm: i)
      case is IR.ReturnInstruction:
        insert(return: i)
      case is IR.StoreInstruction:
        insert(store: i)
      default:
        fatalError("not implemented")
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(borrow i: IR.InstructionID) {
      let s = m[i] as! BorrowInstruction
      register[.register(i, 0)] = llvm(s.location)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(call i: IR.InstructionID) {
      let s = m[i] as! CallInstruction

      // Callee is evaluated first.
      let callee = llvm(s.callee)
      var arguments: [LLVM.IRValue] = []

      // Return value is passed by reference.
      let returnType: LLVM.IRType?
      if s.types[0].astType != .void {
        returnType = ir.syntax.llvm(s.types[0].astType, in: &self)
        arguments.append(insertAlloca(returnType!, atEntryOf: transpilation))
      } else {
        returnType = nil
      }

      // All arguments are passed by reference.
      for a in s.arguments {
        if m.type(of: a).isObject {
          let t = ir.syntax.llvm(s.types[0].astType, in: &self)
          let l = insertAlloca(t, atEntryOf: transpilation)
          insertStore(llvm(a), to: l, at: insertionPoint)
          arguments.append(l)
        } else {
          arguments.append(llvm(a))
        }
      }

      // Note: the type of a function in Val IR typically doesn't match the type of its transpiled
      // form. The latter always returns `void`, as non-void results are passed by reference.
      let calleeType = LLVM.Function(callee).map(\.valueType) ?? callee.type
      _ = insertCall(callee, typed: calleeType, on: arguments, at: insertionPoint)

      // Load the return value if necessary.
      if let t = returnType {
        register[.register(i, 0)] = insertLoad(t, from: arguments[0], at: insertionPoint)
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(elementAddr i: IR.InstructionID) {
      let s = m[i] as! ElementAddrInstruction
      let t = LLVM.IntegerType(32, in: &self)

      let base = llvm(s.base)
      let baseType = ir.syntax.llvm(m.type(of: s.base).astType, in: &self)
      let indices = [t.constant(0)] + s.elementPath.map({ t.constant(UInt64($0)) })
      let v = insertGetElementPointerInBounds(
        of: base, typed: baseType, indices: indices, at: insertionPoint)
      register[.register(i, 0)] = v
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(llvm i: IR.InstructionID) {
      let s = m[i] as! IR.LLVMInstruction
      switch s.function.llvmInstruction {
      case "zeroinitializer":
        let t = ir.syntax.llvm(s.types[0].astType, in: &self)
        register[.register(i, 0)] = t.null

      default:
        unreachable("unexpected LLVM instruction '\(s.function.llvmInstruction)'")
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(return i: IR.InstructionID) {
      let s = m[i] as! IR.ReturnInstruction
      if s.object != .constant(.void) {
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
    func llvm(_ o: IR.Operand) -> LLVM.IRValue {
      guard case .constant(let c) = o else {
        return register[o]!
      }

      switch c {
      case .integer(let n):
        guard n.value.bitWidth <= 64 else { fatalError("not implemented") }
        let t = LLVM.IntegerType(n.value.bitWidth, in: &self)
        return t.constant(UInt64(n.value.words[0]))

      case .floatingPoint(let n):
        let t = LLVM.FloatingPointType(ir.syntax.llvm(c.type.astType, in: &self))!
        return t.constant(parsing: n.value)

      case .function(let f):
        return declare(f, from: ir)

      case .poison:
        let t = ir.syntax.llvm(c.type.astType, in: &self)
        return LLVM.Poison(of: t)

      default:
        fatalError("not implemented")
      }
    }
  }

}

extension LLVMProgram: CustomStringConvertible {

  public var description: String { "\(list: llvmModules, joinedBy: "\n")" }

}
