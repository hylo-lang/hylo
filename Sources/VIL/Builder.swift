import AST
import Basic

/// A builder for VIL code.
public struct Builder {

  /// The module that is being edited.
  public var module: Module

  /// The AST context in which the builder is being used.
  public unowned let context: Context

  /// An identifier factory.
  private var idFactory = AutoIncrementFactory()

  /// The current insertion point of the builder.
  ///
  /// The observer of that property maintains the following invariants:
  /// - If `insertionPointer` is set, then it refers to an existing function.
  /// - If `insertionPointer.blockID` is set, then it refers to an existing block.
  public var insertionPointer: InsertionPointer? {
    didSet {
      guard let ip = insertionPointer else { return }
      guard let fun = module.functions[ip.funName] else {
        fatalError("function does not exist")
      }
      guard ip.blockID == nil || fun.blocks[ip.blockID!] != nil else {
        fatalError("block does not exist")
      }
    }
  }

  /// Creates a new VIL instruction builder.
  ///
  /// - Parameters:
  ///   - module: The module to edit with the builder.
  ///   - context: The AST context in which the builder is being used.
  public init(module: Module, context: Context) {
    self.module = module
    self.context = context

    // Initialize the constant value store if necessary.
    if context.allocateBuffer(forKey: .vilConstantStore, ofType: ConstantStore.self) {
      let errorType = VILType.lower(context.errorType)
      let unitType = VILType.lower(context.unitType)
      context.withBuffer(forKey: .vilConstantStore, of: ConstantStore.self, { ptr in
        ptr.initialize(
          to: ConstantStore(
            poison: PoisonValue(type: errorType),
            unit: UnitValue(type: unitType)))
      })
    }
  }

  /// The current function in which the builder is inserting instructions.
  public var currentFun: VILFun? {
    if let name = insertionPointer?.funName {
      return module.functions[name]
    } else {
      return nil
    }
  }

  /// Calls the given closure with the function referenced by the builder's insertion pointer
  /// passed as an inout argument.
  ///
  /// - Parameter action: A closure that accepts the current function. `action` is not called if
  ///   the insertion pointer does not refer to any function.
  /// - Returns: The result of `action` or `nil` if it was not called.
  public mutating func withCurrentFun<R>(_ action: (inout VILFun) -> R) -> R? {
    if let funName = insertionPointer?.funName {
      return action(&module.functions[funName]!)
    } else {
      return nil
    }
  }

  /// Calls the given closure with the basic block referenced by the builder's insertion pointer
  /// passed as an inout argument.
  ///
  /// - Parameter action: A closure that accepts the current basic block. `action` is not called if
  ///   the insertion pointer does not refer to any basic block.
  /// - Returns: The result of `action` or `nil` if it was not called.
  public mutating func withCurrentBasicBlock<R>(_ action: (inout BasicBlock) -> R) -> R? {
    if let blockID = insertionPointer?.blockID {
      return action(&module.functions[insertionPointer!.funName]!.blocks[blockID]!)
    } else {
      return nil
    }
  }

  /// Builds a unique identifier.
  public mutating func makeUID() -> Int {
    return idFactory.makeID()
  }

  /// Builds a unique basic block identifier.
  public mutating func makeBasicBlockID() -> BasicBlock.ID {
    return UInt32(makeUID())
  }

  // MARK: Functions and blocks

  /// Retrieves or creates a function with the specified name and type.
  ///
  /// - Parameters:
  ///   - name: The name of the function to retrieve or create.
  ///   - type: The unapplied type of the function. If the module already contains a function with
  ///     the same name, then it must have the same type as `type`.
  ///   - debugName: An optional debug name describing the function.
  public mutating func getOrCreateFunction(
    name: VILName,
    type: FunType,
    debugName: String? = nil
  ) -> VILFun {
    // Check if the module already contains a module with the specified name.
    let loweredType = VILType.lower(type)
    if let fun = module.functions[name] {
      precondition(
        fun.type.valType == loweredType.valType,
        "function '\(name)' already exists with a different type")
      return fun
    }

    // Create the function object.
    let fun = VILFun(name: name, type: loweredType, debugName: debugName)
    module.functions[name] = fun
    return fun
  }

  /// Retrieves or create a function from the given declaration.
  ///
  /// - Parameter decl: A function declaration.
  public mutating func getOrCreateFunction(from decl: BaseFunDecl) -> VILFun {
    // Mangle the function's name.
    // FIXME: We have to implement an attribute like "@vilname(...)".
    let name: VILName
    if decl.name == "main" {
      name = VILName(decl.name)
    } else {
      var mangler = Mangler()
      mangler.append(funDecl: decl)
      name = VILName(mangler.finalize())
    }

    // Extract the function's unapplied type.
    var unappliedType = decl.unappliedType as! FunType

    // Extend the function's arguments with the type of each captured symbol.
    let captureTable = decl.computeAllCaptures()
    if !captureTable.isEmpty {
      let context = unappliedType.context
      let extra = captureTable.map({ (_, value) -> FunType.Param in
        // Captures with mutable semantics are represented by in-out parameters.
        switch value.semantics {
        case .val:
          // Immutable captures are consuming to leave the closure independent.
          // FIXME: We could relax this constraint if we can guarantee that the closure is local.
          return FunType.Param(policy: .consuming, rawType: value.type)

        case .var:
          // Mutable captures are consuming mutable.
          return FunType.Param(policy: .consumingMutable, rawType: value.type)

        case .mut:
          // Borrowed captures are inout.
          return FunType.Param(policy: .inout, rawType: value.type)
        }
      })

      unappliedType = context.funType(
        params: extra + unappliedType.params,
        retType: unappliedType.retType)
    }

    // Create the function.
    return getOrCreateFunction(name: name, type: unappliedType, debugName: decl.debugID)
  }

  /// Builds a basic block.
  ///
  /// The block is created in the function referenced by the builder's insertion point. The
  /// method fails if that insertion point does not refer to a function.
  ///
  /// - Parameters:
  ///   - paramTypes: The type of each formal argument.
  ///   - isEntry: A Boolean value that indicates whether the new block should be set as the
  ///     function's entry.
  public mutating func buildBasicBlock(
    paramTypes: [VILType] = [],
    isEntry: Bool = false
  ) -> BasicBlock.ID {
    precondition(insertionPointer != nil, "insertion pointer is not configured")

    let blockID = makeBasicBlockID()
    withCurrentFun({ fun in
      fun.blocks[blockID] = BasicBlock(id: blockID, paramTypes: paramTypes)
      if isEntry {
       fun.entryID = blockID
      }
    })

    return blockID
  }

  // MARK: Constants

  /// Builds a poising value.
  public func buildPoison() -> PoisonValue {
    return context.withBuffer(
      forKey: .vilConstantStore, of: ConstantStore.self, { (ptr) -> PoisonValue in
        ptr.pointee.poison
      })!
  }

  /// Builds a unit value.
  public func buildUnit() -> UnitValue {
    return context.withBuffer(
      forKey: .vilConstantStore, of: ConstantStore.self, { (ptr) -> UnitValue in
        ptr.pointee.unit
      })!
  }

  /// Builds an integer value.
  public func buildInt(bitPattern: Int64, bitWidth: Int) -> IntValue {
    return context.withBuffer(
      forKey: .vilConstantStore, of: ConstantStore.self, { (ptr) -> IntValue in
        let (_, instance) = ptr.pointee.intConstants
          .insert(IntValue(bitPattern: bitPattern, bitWidth: bitWidth, context: context))
        return instance
      })!
  }

  /// Builds a built-in `false` constant (i.e., `0` with the type `i1`).
  public func buildFalse() -> IntValue {
    return buildInt(bitPattern: 0, bitWidth: 1)
  }

  /// Builds a built-in `true` constant (i.e., `1` with the type `i1`).
  public func buildTrue() -> IntValue {
    return buildInt(bitPattern: 1, bitWidth: 1)
  }

  /// Builds an integer literal.
  public func buildIntLiteral(value: Int) -> IntLiteralValue {
    return context.withBuffer(
      forKey: .vilConstantStore, of: ConstantStore.self, { (ptr) -> IntLiteralValue in
        if let instance = ptr.pointee.intLiterals[value] {
          return instance
        } else {
          let instance = IntLiteralValue(value: value, context: context)
          ptr.pointee.intLiterals[value] = instance
          return instance
        }
      })!
  }

  /// Builds a literal reference to a built-in function.
  public func buildBuiltinFunRef(decl: FunDecl) -> BuiltinFunRef {
    assert(decl.type.context === context)
    return context.withBuffer(
      forKey: .vilConstantStore, of: ConstantStore.self, { (ptr) -> BuiltinFunRef in
        let name = VILName(decl.name)
        if let instance = ptr.pointee.builtinFunRefs[name] {
          return instance
        } else {
          let instance = BuiltinFunRef(decl: decl)
          ptr.pointee.builtinFunRefs[name] = instance
          return instance
        }
      })!
  }

  /// Builds a literal reference to a VIL function.
  public func buildFunRef(function: VILFun) -> FunRef {
    assert(function.type.valType.context === context)
    return context.withBuffer(
      forKey: .vilConstantStore, of: ConstantStore.self, { (ptr) -> FunRef in
        if let instance = ptr.pointee.funRefs[function.name] {
          return instance
        } else {
          let instance = FunRef(function: function)
          ptr.pointee.funRefs[function.name] = instance
          return instance
        }
      })!
  }

  /// Builds a `nulladdr` literal.
  public func buildNullAddr(type: VILType) -> NullAddr {
    assert(type.valType.context === context)
    return context.withBuffer(
      forKey: .vilConstantStore, of: ConstantStore.self, { (ptr) -> NullAddr in
        if let instance = ptr.pointee.nullAddrs[type.valType] {
          return instance
        } else {
          let instance = NullAddr(type: type)
          ptr.pointee.nullAddrs[type.valType] = instance
          return instance
        }
      })!
  }

  /// Builds a `nil` value.
  public mutating func buildNil() -> Value {
    let decl = context.getTypeDecl(for: .Nil) as! ProductTypeDecl
    return buildRecord(typeDecl: decl, type: .lower(decl.instanceType))
  }

  // MARK: Instructions

  public mutating func buildAllocStack(
    allocType: VILType,
    isReceiver: Bool = false,
    decl: ValueDecl? = nil,
    range: SourceRange? = nil
  ) -> AllocStackInst {
    let inst = AllocStackInst(
      allocType: allocType, isReceiver: isReceiver, decl: decl, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildApply(
    callee: Value,
    args: [Value],
    range: SourceRange? = nil
  ) -> ApplyInst {
    let inst = ApplyInst(callee: callee, args: args, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildAsync(
    ref: FunRef,
    captures: [Value],
    range: SourceRange? = nil
  ) -> AsyncInst {
    let inst = AsyncInst(ref: ref, captures: captures, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildAwait(
    value: Value,
    range: SourceRange? = nil
  ) -> AwaitInst {
    let inst = AwaitInst(value: value, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildBorrowAddr(
    isMutable: Bool = false,
    source: Value,
    range: SourceRange? = nil
  ) -> BorrowAddrInst {
    let inst = BorrowAddrInst(isMutable: isMutable, source: source, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildBorrowExistAddr(
    isMutable: Bool = false,
    container: Value,
    type: VILType,
    range: SourceRange? = nil
  ) -> BorrowExistAddrInst {
    let inst = BorrowExistAddrInst(
      isMutable: isMutable, container: container, type: type, range: range)
    insert(inst)
    return inst
  }

  @discardableResult
  public mutating func buildBorrowExistAddrBranch(
    isMutable: Bool = false,
    container: Value,
    type: VILType,
    succ: BasicBlock.ID,
    fail: BasicBlock.ID,
    range: SourceRange? = nil
  ) -> BorrowExistAddrBranchInst {
    let inst = BorrowExistAddrBranchInst(
      isMutable: isMutable,
      container: container,
      type: type,
      succ: succ,
      fail: fail,
      range: range)
    insert(inst)
    return inst
  }

  @discardableResult
  public mutating func buildBranch(
    dest: BasicBlock.ID,
    args: [Value] = [],
    range: SourceRange? = nil
  ) -> BranchInst {
    let inst = BranchInst(dest: dest, args: args, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildCheckedCast(
    value: Value,
    type: VILType,
    range: SourceRange? = nil
  ) -> CheckedCastInst {
    let inst = CheckedCastInst(value: value, type: type, range: range)
    insert(inst)
    return inst
  }

  @discardableResult
  public mutating func buildCheckedCastBranch(
    value: Value,
    type: VILType,
    succ: BasicBlock.ID,
    fail: BasicBlock.ID,
    range: SourceRange? = nil
  ) -> CheckedCastBranchInst {
    let inst = CheckedCastBranchInst(
      value: value, type: type, succ: succ, fail: fail, range: range)
    insert(inst)
    return inst
  }

  @discardableResult
  public mutating func buildCondBranch(
    cond: Value,
    succ: BasicBlock.ID, succArgs: [Value],
    fail: BasicBlock.ID, failArgs: [Value],
    range: SourceRange? = nil
  ) -> CondBranchInst {
    let inst = CondBranchInst(
      cond: cond, succ: succ, succArgs: succArgs, fail: fail, failArgs: failArgs, range: range)
    insert(inst)
    return inst
  }

  @discardableResult
  public mutating func buildCondFail(
    cond: Value,
    range: SourceRange? = nil
  ) -> CondFail {
    let inst = CondFail(cond: cond, range: range)
    insert(inst)
    return inst
  }

  @discardableResult
  public mutating func buildDeallocStack(
    alloc: AllocStackInst,
    range: SourceRange? = nil
  ) -> DeallocStackInst {
    let inst = DeallocStackInst(alloc: alloc, range: range)
    insert(inst)
    return inst
  }

  @discardableResult
  public mutating func buildDelete(
    value: Value,
    range: SourceRange? = nil
  ) -> DeleteInst {
    let inst = DeleteInst(value: value, range: range)
    insert(inst)
    return inst
  }

  @discardableResult
  public mutating func buildDeleteAddr(
    target: Value,
    range: SourceRange? = nil
  ) -> DeleteAddrInst {
    let inst = DeleteAddrInst(target: target, range: range)
    insert(inst)
    return inst
  }

  @discardableResult
  public mutating func buildInitExistAddr(
    container: Value,
    value: Value,
    range: SourceRange? = nil
  ) -> InitExistAddrInst {
    let inst = InitExistAddrInst(container: container, value: value, range: range)
    insert(inst)
    return inst
  }

  @discardableResult
  public mutating func buildHalt(
    range: SourceRange? = nil
  ) -> HaltInst {
    let inst = HaltInst(range: range)
    insert(inst)
    return inst
  }

  public mutating func buildLoad(
    source: Value,
    range: SourceRange? = nil
  ) -> LoadInst {
    let inst = LoadInst(source: source, range: range)
    insert(inst)
    return inst
  }

  @discardableResult
  public mutating func buildMoveAddr(
    from source: Value,
    to target: Value,
    range: SourceRange? = nil
  ) -> MoveAddrInst {
    let inst = MoveAddrInst(source: source, target: target, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildPackBorrow(
    source: Value,
    type: VILType,
    range: SourceRange? = nil
  ) -> PackBorrowInst {
    let inst = PackBorrowInst(source: source, type: type, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildPartialApply(
    delegator: Value,
    partialArgs: [Value],
    range: SourceRange? = nil
  ) -> PartialApplyInst {
    let inst = PartialApplyInst(delegator: delegator, partialArgs: partialArgs, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildRecord(
    typeDecl: NominalTypeDecl,
    type: VILType,
    range: SourceRange? = nil
  ) -> RecordInst {
    let inst = RecordInst(typeDecl: typeDecl, type: type, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildRecordMember(
    record: Value,
    memberDecl: VarDecl,
    type: VILType,
    range: SourceRange? = nil
  ) -> RecordMemberInst {
    let inst = RecordMemberInst(record: record, memberDecl: memberDecl, type: type, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildRecordMemberAddr(
    record: Value,
    memberDecl: VarDecl,
    type: VILType,
    range: SourceRange? = nil
  ) -> RecordMemberAddrInst {
    let inst = RecordMemberAddrInst(record: record, memberDecl: memberDecl, type: type, range: range)
    insert(inst)
    return inst
  }

  @discardableResult
  public mutating func buildRet(
    value: Value,
    range: SourceRange? = nil
  ) -> RetInst {
    let inst = RetInst(value: value, range: range)
    insert(inst)
    return inst
  }

  @discardableResult
  public mutating func buildStore(
    _ value: Value,
    to target: Value,
    range: SourceRange? = nil
  ) -> StoreInst {
    let inst = StoreInst(value: value, target: target, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildThinToThick(
    ref: FunRef,
    range: SourceRange? = nil
  ) -> ThinToThickInst {
    let inst = ThinToThickInst(ref: ref, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildTuple(
    type: TupleType,
    operands: [Value],
    range: SourceRange? = nil
  ) -> TupleInst {
    let inst = TupleInst(type: type, operands: operands, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildWitnessMethod(
    container: Value,
    decl: BaseFunDecl,
    range: SourceRange? = nil
  ) -> WitnessMethodInst {
    let inst = WitnessMethodInst(container: container, decl: decl, range: range)
    insert(inst)
    return inst
  }

  public mutating func buildWitnessMethodAddr(
    container: Value,
    decl: BaseFunDecl,
    range: SourceRange? = nil
  ) -> WitnessMethodAddrInst {
    let inst = WitnessMethodAddrInst(container: container, decl: decl, range: range)
    insert(inst)
    return inst
  }

  /// Removes an instruction.
  public mutating func remove(at path: InstPath) {
    let inst = module[path]
    if let value = inst as? Value { precondition(value.uses.isEmpty, "instruction has uses") }

    // Update the use lists of the instruction's operands.
    for op in inst.operands {
      op.uses.removeAll(where: { $0.userPath == path })
    }

    // Remove the instruction from the block.
    module.functions[path.funName]!.blocks[path.blockID]?.instructions.remove(at: path.instIndex)
  }

  /// Replaces all uses of a value by another value.
  public mutating func replaceUses(of value: Value, with newValue: Value) {
    fatalError()
  }

  /// Inserts the specified instruction at the current insertion point.
  private mutating func insert(_ inst: Inst) {
    precondition(insertionPointer?.blockID != nil, "invalid insertion pointer")

    // Insert the instruction.
    let (instIndex, insertionPosition) = withCurrentBasicBlock(
      { [ip = insertionPointer!] (block) -> (BasicBlock.Index, InsertionPointer.Position) in
        let instIndex = block.instructions.nextStableIndex

        switch ip.position {
        case .end:
          block.instructions.append(inst)
          return (instIndex, .end)

        case .before(let index):
          block.instructions.insert(inst, before: index)
          return (instIndex, .before(index: instIndex))
        }
      })!

    // Update the insertion pointer.
    insertionPointer!.position = insertionPosition

    // Update the use lists of the instruction's operands.
    let funName = insertionPointer!.funName
    let blockID = insertionPointer!.blockID!
    for i in 0 ..< inst.operands.count {
      let userPath = InstPath(funName: funName, blockID: blockID, instIndex: instIndex)
      inst.operands[i].uses.append(Use(userPath: userPath, index: i))
    }
  }

  private func withConstantValueStore<R>(_ action: (inout ConstantStore) -> R) -> R? {
    return context.withBuffer(
      forKey: .vilConstantStore,
      of: ConstantStore.self,
      { action(&$0.pointee) })
  }

}

fileprivate struct ConstantStore {

  let poison: PoisonValue

  let unit: UnitValue

  var intConstants: Set<IntValue> = []

  var intLiterals: [Int: IntLiteralValue] = [:]

  var builtinFunRefs: [VILName: BuiltinFunRef] = [:]

  var funRefs: [VILName: FunRef] = [:]

  var nullAddrs = ReferenceTable<ValType, NullAddr>()

}
