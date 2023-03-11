import Core
import IR
import LLVM
import Utils

/// A Val program transpiled to LLVM.
public struct LLVMProgram {

  /// The LLVM modules in the program.
  private var llvmModules: [LLVM.Module] = []

  /// Creates an empty program.
  public init(_ irProgram: [IR.Module]) throws {
    for m in irProgram {
      let transpilation = transpiled(m)
      try transpilation.verify()
      llvmModules.append(transpilation)
    }
  }

  /// The LLVM transpilation of the Val IR module `ir`.
  private func transpiled(_ ir: IR.Module) -> LLVM.Module {
    var transpilation = LLVM.Module(ir.name)
    for f in ir.functions.keys {
      transpilation.incorporate(f, from: ir)
    }
    return transpilation
  }

}

extension LLVM.Module {

  /// Transpiles and incorporates `f`, which is in `ir`.
  mutating func incorporate(_ f: IR.Function.ID, from ir: IR.Module) {
    let d = declare(f, from: ir)
    transpile(f, from: ir, into: d)
  }

  /// Inserts the declaration of the transpilation of `f`, which is in `ir`, and returns it.
  private mutating func declare(_ f: IR.Function.ID, from ir: IR.Module) -> LLVM.Function {
    let ptr = LLVM.PointerType(in: &self)

    // Parameters and return values are passed by reference.
    var parameters: [LLVM.IRType] = []
    if ir[f].output.astType != .void {
      parameters.append(ptr)
    }
    parameters.append(contentsOf: Array(repeating: ptr, count: ir[f].inputs.count))

    let n: String
    switch f.value {
    case .lowered(let d):
      n = ir.program.abiName(of: d)
    case .constructor(let d):
      n = ir.program.abiName(of: d)
    case .synthesized:
      fatalError("not implemented")
    }

    return declareFunction(n, .init(from: parameters, in: &self))
  }

  private mutating func transpile(
    _ f: IR.Function.ID,
    from ir: IR.Module,
    into transpilation: LLVM.Function
  ) {
    guard let entry = ir[f].entry else { return }

    /// Where new LLVM IR instruction are inserted.
    var insertionPoint: LLVM.InsertionPoint!

    /// A map from Val IR basic block to its LLVM counterpart.
    var block: [IR.Block.ID: LLVM.BasicBlock] = [:]

    /// A map from Val IR register to its LLVM counterpart.
    var register: [IR.Operand: LLVM.IRValue] = [:]

    let prologue = appendBlock(named: "prologue", to: transpilation)
    insertionPoint = endOf(prologue)

    let parameterOffset = (ir[f].output.astType == .void) ? 0 : 1
    for (i, p) in ir[f].inputs.enumerated() {
      let o = Operand.parameter(.init(f, entry), i)
      let s = transpilation.parameters[parameterOffset + i]

      if p.convention == .sink {
        let t = ir.program.llvm(p.type.astType, in: &self)
        register[o] = insertLoad(t, from: s, at: insertionPoint)
      } else {
        register[o] = s
      }
    }

    for b in ir.blocks(in: f) {
      let transpiledBlock = appendBlock(named: b.description, to: transpilation)
      block[b] = transpiledBlock

      insertionPoint = endOf(transpiledBlock)
      for i in ir.instructions(in: b) {
        insert(i)
      }
    }

    insertBr(to: block[.init(f, entry)]!, at: endOf(prologue))

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(_ i: IR.InstructionID) {
      switch ir[i] {
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
      let s = ir[i] as! BorrowInstruction
      register[.register(i, 0)] = llvm(s.location)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(call i: IR.InstructionID) {
      let s = ir[i] as! CallInstruction
      let callee = llvm(s.callee)

      print(callee)
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(elementAddr i: IR.InstructionID) {
      let s = ir[i] as! ElementAddrInstruction
      let t = LLVM.IntegerType(32, in: &self)

      let base = llvm(s.base)
      let baseType = ir.program.llvm(ir.type(of: s.base).astType, in: &self)
      let indices = [t.constant(0)] + s.elementPath.map({ t.constant(UInt64($0)) })
      let v = insertGetElementPointerInBounds(
        of: base, typed: baseType, indices: indices, at: insertionPoint)
      register[.register(i, 0)] = v
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(llvm i: IR.InstructionID) {
      let s = ir[i] as! IR.LLVMInstruction
      switch s.function.llvmInstruction {
      case "zeroinitializer":
        let t = ir.program.llvm(s.types[0].astType, in: &self)
        register[.register(i, 0)] = t.null

      default:
        unreachable("unexpected LLVM instruction '\(s.function.llvmInstruction)'")
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(return i: IR.InstructionID) {
      let s = ir[i] as! IR.ReturnInstruction
      if s.object == .constant(.void) {
        insertReturn(at: insertionPoint)
      } else {
        insertReturn(llvm(s.object), at: insertionPoint)
      }
    }

    /// Inserts the transpilation of `i` at `insertionPoint`.
    func insert(store i: IR.InstructionID) {
      let s = ir[i] as! IR.StoreInstruction
      insertStore(llvm(s.object), to: llvm(s.target), at: insertionPoint)
    }

    /// Returns the LLVM IR value corresponding to the Val IR operand `o`.
    func llvm(_ o: IR.Operand) -> LLVM.IRValue {
      guard case .constant(let c) = o else {
        return register[o]!
      }

      let t = ir.program.llvm(c.type.astType, in: &self)
      switch c {
      case .integer(let n):
        guard n.value.bitWidth <= 64 else { fatalError("not implemented") }
        return LLVM.IntegerType(t)!.constant(UInt64(n.value.words[0]))

      case .floatingPoint(let n):
        return LLVM.FloatingPointType(t)!.constant(parsing: n.value)

      case .poison:
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
