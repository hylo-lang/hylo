import LLVM
import Utils

/// A type that translates Val IR to LLVM IR.
public struct LLVMTranslator {

  /// The program being lowered.
  public let program: TypedProgram

  /// The Val IR module to translate.
  public let irModule: Module

  /// The machine target for the generated IR.
  public let target: LLVM.TargetMachine

  /// The builder that is used to generate LLVM IR instructions.
  private var builder: LLVM.IRBuilder

  /// A table mapping Val types to their translation in LLVM IR.
  private var typeTranslations = Cache<Type, LLVM.IRType>()

  /// A table mapping basic blocks addresses to their corresponding LLVM IR block, in the function
  /// being currently translated.
  private var blocks: [Function.BlockAddress: LLVM.BasicBlock] = [:]

  /// A table mapping Val IR operands to their translated LLVM IR value, in the function being
  /// currently translated.
  private var locals: [FunctionLocal: LLVM.IRValue] = [:]

  /// A pointer to the storage of the current function's return value, if any.
  private var returnStorage: LLVM.IRValue?

  /// Creates a new code generator.
  ///
  /// - Parameters:
  ///   - module: The Val IR module to translate.
  ///   - program: The program containing `module`.
  ///   - target: The machine target for the generated LLVM code.
  public init(
    translating module: Module,
    from program: TypedProgram,
    target: LLVM.TargetMachine? = nil
  ) throws {
    self.program = program
    self.irModule = module
    self.target = try target ?? TargetMachine()
    self.builder = IRBuilder(module: LLVM.Module(name: module.name))
  }

  /// Returns a LLVM module translated from `moduleUnderTranslation`.
  ///
  /// Calling this method consumes the translator. It is illegal to perform operations on a
  /// consumed translator.
  ///
  /// - Parameter isEntryModule: If `true`, a startup LLVM function that calls the translated
  ///   module's entry (i.e., `main` in the Val program) is included in the LLVM module. The
  ///   method fails if the translated module does not contain an entry function.
  public mutating func translate(asEntry isEntryModule: Bool = false) throws -> LLVM.Module {
    for i in irModule.functions.indices {
      emit(function: i)
    }

    if isEntryModule {
      guard let entryFunctionID = irModule.entryFunctionID,
            let entryFunction = builder.module.function(named: irModule[entryFunctionID].name)
      else { preconditionFailure() }

      let startup = builder.module.addFunction(
        "main",
        type: FunctionType([IntType.int32, IntType.int8.star.star], IntType.int32))
      startup.addAttribute(.norecurse, to: .function)

      builder.positionAtEnd(of: startup.appendBasicBlock(named: "start"))
      _ = builder.buildCall(entryFunction, args: [])
      builder.buildRet(IntType.int32.constant(0))
    }

    return builder.module
  }

  private mutating func emit(function functionID: Function.ID) {
    let irFunction = irModule[functionID]

    // Create the LLVM function.
    var llvmFunction = getOrAddFunction(
      irFunction.name, type: llvmFunctionType(of: functionID))

    // Determine the function's linkage.
    switch irFunction.linkage {
    case .external:
      llvmFunction.linkage = .external
    case .module:
      llvmFunction.linkage = .private
    }

    // Configure the function's attributes.
    if irFunction.output.astType == .never {
      llvmFunction.addAttribute(.noreturn, to: .function)
    }

    // We're done if the function has no body.
    guard let entry = irFunction.blocks.firstAddress else { return }

    // Create the function's prologue.
    let prologue = llvmFunction.appendBasicBlock(named: "prologue")

    // Configure `self.blocks`.
    blocks.removeAll()
    for i in irFunction.blocks.indices {
      blocks[i.address] = llvmFunction.appendBasicBlock(named: "bb\(blocks.count)")
    }

    // Configure `self.locals` with the parameters of the function.
    if !irFunction.output.astType.isVoid {
      returnStorage = llvmFunction.parameter(at: 0)
    }
    locals = Dictionary(uniqueKeysWithValues: (0 ..< irFunction.inputs.count).map({ i in
      let k: FunctionLocal = .param(block: entry, index: i)
      let v: IRValue = llvmFunction.parameter(at: i + 1)!
      return (key: k, value: v)
    }))

    // Translate the instructions.
    for i in irFunction.blocks.indices {
      builder.positionAtEnd(of: blocks[i.address]!)
      for j in irFunction[i.address].instructions.indices {
        emit(inst: InstID(function: functionID, block: i.address, address: j.address))
      }
    }

    // Branch the prologue to the entry.
    builder.positionAtEnd(of: prologue)
    builder.buildBr(blocks[irFunction.blocks.firstAddress!]!)
  }

  private mutating func emit(operand: Operand) -> LLVM.IRValue {
    switch operand {
    case .result(let inst, let index):
      return locals[.result(block: inst.block, address: inst.address, index: index)]!
    case .parameter(let block, let index):
      return locals[.param(block: block.address, index: index)]!
    case .constant(let constant):
      return emit(constant: constant)
    }
  }

  private mutating func emit(constant: Constant) -> LLVM.IRValue {
    switch constant {
    case .builtin:
      fatalError("cannot wrap built-in function in a closure")

    case .integer(let integerConstant):
      return IntType.constant(integerConstant.bitPattern)

    case .function(let reference):
      let type = LambdaType(converting: reference.type.astType)!
      return getOrAddFunction(reference.name, type: llvmFunctionType(converting: type))

    case .poison(let poison):
      return llvmType(translating: poison.type.astType).undef()

    case .unit:
      return IntType.int8.constant(0)
    }
  }

  private mutating func emit(inst id: InstID) {
    switch irModule[id.function][id.block][id.address] {
    case let inst as AllocStackInst:
      emit(allocStack: inst, id: id)
    case let inst as BorrowInst:
      emit(borrow: inst, id: id)
    case let inst as BranchInst:
      emit(branch: inst, id: id)
    case let inst as CallInst:
      emit(call: inst, id: id)
    case let inst as CondBranchInst:
      emit(condBranch: inst, id: id)
    case let inst as DeinitInst:
      emit(deinit: inst, id: id)
    case let inst as DestructureInst:
      emit(destructure: inst, id: id)
    case let inst as LoadInst:
      emit(load: inst, id: id)
    case let inst as RecordInst:
      emit(record: inst, id: id)
    case let inst as ReturnInst:
      emit(return: inst, id: id)
    case let inst as StoreInst:
      emit(store: inst, id: id)
    case let inst as UnrechableInst:
      emit(unreachable: inst, id: id)
    case is DeallocStackInst, is EndBorrowInst:
      break
    default:
      unreachable("unexpected instruction")
    }
  }

  private mutating func emit(allocStack inst: AllocStackInst, id: InstID) {
    locals[FunctionLocal(id, 0)] = buildPrologueAlloca(
      type: llvmType(translating: inst.allocatedType))
  }

  private mutating func emit(borrow inst: BorrowInst, id: InstID) {
    let basePointer = emit(operand: inst.location)

    if inst.path.isEmpty {
      locals[FunctionLocal(id, 0)] = basePointer
    } else {
      let baseType = llvmType(translating: irModule.type(of: inst.location).astType)
      let indices = [IntType.int32.constant(0)] + inst.path.map({ IntType.int32.constant($0) })
      locals[FunctionLocal(id, 0)] = builder.buildInBoundsGEP(
        basePointer, type: baseType, indices: indices)
    }
  }

  private mutating func emit(branch inst: BranchInst, id: InstID) {
    builder.buildBr(blocks[inst.target.address]!)
  }

  private mutating func emit(call inst: CallInst, id: InstID) {
    let callee: IRValue

    // Compute the callee's address.
    switch inst.callee {
    case .constant(let constant):
      // Handle built-in calls as a special case.
      if case .builtin(let reference) = constant {
        emit(builtinCall: inst, id: id, calling: reference)
        return
      }

      // Callee's address is a constant.
      callee = emit(operand: inst.callee)

    default:
      // TODO: Handle closures
      fatalError("not implemented")
    }

    // Build the argument list.
    var arguments: [IRValue] = []
    arguments.reserveCapacity(inst.operands.count + 1)

    // Allocates storage for the return value if necessary.
    if !inst.returnType.astType.isVoid {
      arguments.append(
        buildPrologueAlloca(type: llvmType(translating: inst.returnType.astType)))
    }

    // Note: all arguments are passed by reference, including arguments to `sink` parameters.
    for i in inst.arguments.indices {
      switch inst.conventions[i] {
      case .let, .inout, .set:
        arguments.append(emit(operand: inst.arguments[i]))

      case .sink:
        let tmp = buildPrologueAlloca(
          type: llvmType(translating: irModule.type(of: inst.arguments[i]).astType))
        builder.buildStore(emit(operand: inst.arguments[i]), to: tmp)
        arguments.append(tmp)

      case .yielded:
        unreachable()
      }
    }

    _ = builder.buildCall(callee, args: arguments)

    if !inst.returnType.astType.isVoid {
      locals[FunctionLocal(id, 0)] = builder.buildLoad(
        arguments[0], type: llvmType(translating: inst.returnType.astType))
    } else {
      locals[FunctionLocal(id, 0)] = IntType.int8.constant(0)
    }
  }

  private mutating func emit(
    builtinCall inst: CallInst, id: InstID, calling reference: BuiltinFunctionRef
  ) {
    let arguments = inst.arguments.map({ emit(operand: $0) })
    let key = FunctionLocal(id, 0)

    switch reference.name {
    case "terminate":
      _ = builder.buildCall(trap, args: [])

    case "i1_copy":
      locals[key] = builder.buildLoad(arguments[0], type: IntType.int1)

    case "i64_copy":
      locals[key] = builder.buildLoad(arguments[0], type: IntType.int64)

    case "i64_mul":
      let lhs = builder.buildLoad(arguments[0], type: IntType.int64)
      let rhs = builder.buildLoad(arguments[1], type: IntType.int64)
      locals[key] = builder.buildMul(lhs, rhs, overflowBehavior: .noSignedWrap)

    case "i64_add":
      let lhs = builder.buildLoad(arguments[0], type: IntType.int64)
      let rhs = builder.buildLoad(arguments[1], type: IntType.int64)
      locals[key] = builder.buildAdd(lhs, rhs, overflowBehavior: .noSignedWrap)

    case "i64_sub":
      let lhs = builder.buildLoad(arguments[0], type: IntType.int64)
      let rhs = builder.buildLoad(arguments[1], type: IntType.int64)
      locals[key] = builder.buildSub(lhs, rhs, overflowBehavior: .noSignedWrap)

    case "i64_lt":
      let lhs = builder.buildLoad(arguments[0], type: IntType.int64)
      let rhs = builder.buildLoad(arguments[1], type: IntType.int64)
      locals[key] = builder.buildICmp(lhs, rhs, .signedLessThan)

    case "i64_print":
      break // TODO

    default:
      unreachable("unexpected built-in function reference")
    }
  }

  private mutating func emit(condBranch inst: CondBranchInst, id: InstID) {
    builder.buildCondBr(
      condition: emit(operand: inst.condition),
      then: blocks[inst.targetIfTrue.address]!,
      else: blocks[inst.targetIfFalse.address]!)
  }

  private mutating func emit(deinit inst: DeinitInst, id: InstID) {
    // TODO
  }

  private mutating func emit(destructure inst: DestructureInst, id: InstID) {
    let object = emit(operand: inst.object)
    for i in 0 ..< inst.types.count {
      locals[FunctionLocal(id, i)] = builder.buildExtractValue(object, index: i)
    }
  }

  private mutating func emit(load inst: LoadInst, id: InstID) {
    var source = emit(operand: inst.source)
    let objectType = llvmType(translating: inst.objectType.astType)

    if !inst.path.isEmpty {
      let sourceType = llvmType(translating: irModule.type(of: inst.source).astType)
      source = builder.buildInBoundsGEP(
        source,
        type: sourceType,
        indices: inst.path.map({ IntType.int32.constant($0) }))
    }

    locals[FunctionLocal(id, 0)] = builder.buildLoad(source, type: objectType)
  }

  private mutating func emit(record inst: RecordInst, id: InstID) {
    // Fast path when the object to create is equivalent to `unit`.
    if inst.operands.isEmpty {
      locals[FunctionLocal(id, 0)] = IntType.int8.constant(0)
      return
    }

    // Create the object in registers.
    var aggregate = llvmType(translating: inst.objectType.astType).undef()
    for i in 0 ..< inst.operands.count {
      aggregate = builder.buildInsertValue(
        aggregate: aggregate, element: emit(operand: inst.operands[i]), index: i)
    }
    locals[FunctionLocal(id, 0)] = aggregate
  }

  private mutating func emit(return inst: ReturnInst, id: InstID) {
    if !irModule.type(of: inst.value).astType.isVoid {
      builder.buildStore(emit(operand: inst.value), to: returnStorage!)
    }
    builder.buildRetVoid()
  }

  private mutating func emit(store inst: StoreInst, id: InstID) {
    builder.buildStore(emit(operand: inst.object), to: emit(operand: inst.target))
  }

  private mutating func emit(unreachable inst: UnrechableInst, id: InstID) {
    builder.buildUnreachable()
  }

  /// Builds an `alloca` at the beginning of the current function.
  private func buildPrologueAlloca(type: LLVM.IRType) -> LLVM.IRInstruction {
    let currentInsertBlock = builder.insertBlock!
    defer { builder.positionAtEnd(of: currentInsertBlock) }

    builder.positionAtEnd(of: builder.currentFunction!.entryBlock!)
    return builder.buildAlloca(type: type)
  }

  /// Returns the LLVM type corresponding to `type`
  private mutating func llvmType(translating type: Type) -> LLVM.IRType {
    switch typeTranslations[type] {
    case .done(let translation):
      return translation
    case .inProgress:
      fatalError("unbounded memory representation")
    case nil:
      typeTranslations[type] = .inProgress
    }

    let translation: IRType
    switch type {
    case .builtin(let builtinType):
      switch builtinType {
      case .i(let width):
        translation = IntType(width: width, in: builder.module.context)
      case .f64:
        translation = FloatType(kind: .double, in: builder.module.context)
      case .pointer:
        translation = VoidType().star
      case .module:
        fatalError("no LLVM type representation")
      }

    case .product(let productType):
      let locator = program.locator(identifying: productType.decl)
      let layout = program.abstractLayout(of: type)

      let structType = builder.createStruct(name: locator.mangled)
      structType.setBody(layout.storedPropertiesTypes.map({
        llvmType(translating: $0)
      }))
      translation = structType

    case .tuple(let tupleType):
      translation = StructType(elementTypes: tupleType.elements.map({
        llvmType(translating: $0.type)
      }))

    case .union(let unionType):
      assert(unionType.elements.isEmpty, "not implemented")
      translation = IntType.int8

    default:
      unreachable("unexpected type")
    }

    typeTranslations[type] = .done(translation)
    return translation
  }

  private mutating func llvmFunctionType(converting type: LambdaType) -> FunctionType {
    var parameterTypes: [IRType] = []

    // Return value comes first, if any.
    if !type.output.isVoid {
      parameterTypes.append(llvmType(translating: type.output).star)
    }

    // Next come implicit parameters.
    for input in type.captures {
      switch input.type {
      case .projection(let t):
        parameterTypes.append(llvmType(translating: t.base).star)
      case .parameter(let t):
        parameterTypes.append(llvmType(translating: t.bareType).star)
      default:
        parameterTypes.append(llvmType(translating: input.type).star)
      }
    }

    // Next come explicit parameters.
    for input in type.inputs {
      switch input.type {
      case .parameter(let t):
        parameterTypes.append(llvmType(translating: t.bareType).star)
      default:
        parameterTypes.append(llvmType(translating: input.type).star)
      }
    }

    return FunctionType(parameterTypes, VoidType())
  }

  private mutating func llvmFunctionType(of functionID: Function.ID) -> FunctionType {
    let irFunction = irModule[functionID]

    var parameterTypes: [IRType] = []
    if !irFunction.output.astType.isVoid {
      parameterTypes.append(llvmType(translating: irFunction.output.astType).star)
    }
    for input in irFunction.inputs {
      parameterTypes.append(llvmType(translating: input.type.astType).star)
    }

    return FunctionType(parameterTypes, VoidType())
  }

  /// Returns the translated function named `name`, adding it to the LLVM module if necessary.
  public func getOrAddFunction(_ name: String, type: FunctionType) -> LLVM.Function {
    if let llvmFunction = builder.module.function(named: name) {
      assert(type == (llvmFunction.type as? PointerType)?.pointee as? FunctionType)
      return llvmFunction
    }

    let llvmFunction = builder.module.addFunction(name, type: type)
    return llvmFunction
  }

  /// The `llvm.trap` intrinsic.
  private var trap: LLVM.Intrinsic {
    builder.module.intrinsic(Intrinsic.ID.llvm_trap)!
  }

}

fileprivate extension Type {

  /// Returns `true` if `self` corresponds to the void type in LLVM.
  var isVoid: Bool { self == .unit || self == .never }

}

fileprivate extension LLVM.IntType {

  static func constant(_ pattern: BitPattern) -> LLVM.Constant<Signed> {
    if let value = Int(bitPattern: pattern) {
      return IntType(width: pattern.width).constant(value)
    } else {
      return IntType(width: pattern.width).constant(pattern.hexadecimalString(), radix: 16)
    }
  }

}

fileprivate extension LLVM.IRType {

  /// Returns `PointerType(pointee: self)`.
  var star: PointerType { PointerType(pointee: self) }

}
