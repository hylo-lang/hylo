import AST
import Basic

/// An instruction index.
public typealias InstIndex = Slab<Inst>.Index

/// A basic block index.
public typealias BasicBlockIndex = Slab<BasicBlock>.Index

/// A VIL module.
///
/// A module is a collection of VIL functions that have been lowered from a module declaration.
public struct Module {

  // Implementation note: the contents of a module is represented as a list of instructions stored
  // in aa collection stable indices. A basic block is essentially an array of indices into that
  // sequence, while a function is an array of basic block indicies.

  /// The module's identifier.
  public let id: String

  /// The AST context in which module was created.
  public unowned let context: AST.Context

  /// The instructions in the module.
  public private(set) var instructions = Slab<Inst>()

  /// The basic blocks in the module.
  public private(set) var blocks = Slab<BasicBlock>()

  /// The functions in the module.
  public var functions: [String: VILFun] = [:]

  /// The view witness tables in the module.
  public var viewWitnessTables: [ViewWitnessTable] = []

  /// The def-use chains of the values in this module.
  public var uses: [Operand: [Use]] = [:]

  public init(id: String, context: AST.Context) {
    self.id = id
    self.context = context
  }

  /// Returns the type of the specified operand.
  ///
  /// - Parameter operand: An operand that is used in this module.
  public func type(of operand: Operand) -> VILType {
    if let inst = operand.inst {
      return (instructions[inst] as! Value).type
    } else if let value = operand.argument {
      return value.type
    } else if let value = operand.constant {
      return value.type
    } else {
      fatalError("unreachable")
    }
  }

  /// Returns the index of the block into which the given insertion point refers.
  public func block(containing point: InsertionPoint) -> BasicBlockIndex {
    switch point {
    case .startOf(let blockIndex):
      return blockIndex
    case .endOf(let blockIndex):
      return blockIndex
    case .before(let inst):
      return instructions[inst].parent
    case .after(let inst):
      return instructions[inst].parent
    }
  }

  /// Returns the name of the function into which the given instruction resides.
  public func function(containing inst: InstIndex) -> String {
    return blocks[instructions[inst].parent].parent
  }

  /// Retrieves or creates a function with the specified name and type.
  ///
  /// - Parameters:
  ///   - name: The name of the function to retrieve or create.
  ///   - type: The unapplied type of the function. If the module already contains a function with
  ///     the same name, then it must have the same type as `type`.
  ///   - debugName: An optional debug name describing the function.
  public mutating func getOrCreateFunction(
    name: String,
    type: FunType,
    debugName: String? = nil
  ) -> VILFun {
    // Check if the module already contains a module with the specified name.
    let loweredType = VILType.lower(type)
    if let fun = functions[name] {
      assert(
        fun.type.valType == loweredType.valType,
        "function '\(name)' already exists with a different type")
      return fun
    }

    // Create the function object.
    let fun = VILFun(name: name, debugName: debugName, type: loweredType)
    functions[name] = fun
    return fun
  }

  /// Retrieves or create a function from the given declaration.
  ///
  /// - Parameter decl: A function declaration.
  public mutating func getOrCreateFunction(from decl: BaseFunDecl) -> VILFun {
    // Mangle the function's name.
    // FIXME: We have to implement an attribute like "@vilname(...)".
    let name: String
    if decl.name == "main" {
      name = decl.name
    } else {
      var mangler = Mangler()
      mangler.append(funDecl: decl)
      name = mangler.finalize()
    }

    // Extract the function's unapplied type.
    var unappliedType = decl.unappliedType as! FunType

    // Extend the function's arguments with the type of each captured symbol.
    let captureTable = decl.computeAllCaptures()
    if !captureTable.isEmpty {
      let context = unappliedType.context
      let extra = captureTable.map({ (_, value) -> FunType.Param in
        // Captures with mutable semantics are represented by in-out parameters.
        return FunType.Param(policy: value.policy, rawType: value.type)
      })

      unappliedType = context.funType(
        params: extra + unappliedType.params,
        retType: unappliedType.retType)
    }

    // Create the function.
    return getOrCreateFunction(name: name, type: unappliedType, debugName: decl.debugID)
  }

  /// Creates and inserts a basic block in the specified function.
  ///
  /// - Parameters:
  ///   - paramTypes: The type of each formal argument.
  ///   - funName: The name of the function in which the block is created.
  ///   - isEntry: A Boolean value that indicates whether the new block should be set as the
  ///     function's entry.
  public mutating func insertBasicBlock(
    paramTypes: [VILType] = [],
    in funName: String,
    isEntry: Bool = false
  ) -> BasicBlockIndex {
    let blockIndex = blocks.insert(BasicBlock(parent: funName, paramTypes: paramTypes))
    if isEntry {
      functions[funName]!.blocks.insert(blockIndex, at: 0)
    } else {
      functions[funName]!.blocks.append(blockIndex)
    }
    return blockIndex
  }

  public mutating func insertAllocStack(
    allocType: VILType,
    decl: ValueDecl? = nil,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(allocType.isObject, "'allocatedType' must be an object type")

    let inst = AllocStackInst(
      allocType: allocType,
      decl: decl,
      parent: block(containing: point),
      range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertApply(
    callee: Operand,
    args: [Operand],
    range: SourceRange? = nil,
    argsRanges: [SourceRange?]? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: callee).valType is FunType, "'callee' must have a function type")
    assert(type(of: callee).isAddress, "'callee' must have an address type")

    assert(argsRanges == nil || argsRanges!.count == args.count)
    var ranges = argsRanges ?? Array(repeating: nil, count: args.count)
    ranges.append(range)

    let inst = ApplyInst(
      callee: callee,
      args: args,
      type: type(of: callee).retType!,
      parent: block(containing: point),
      ranges: ranges)
    return insert(inst: inst, at: point)
  }

  public mutating func insertAsync(
    ref: FunRef,
    captures: [Operand],
    range: SourceRange? = nil,
    captureRanges: [SourceRange?]? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(captureRanges == nil || captureRanges!.count == captures.count)
    var ranges = captureRanges ?? Array(repeating: nil, count: captures.count)
    ranges.append(range)

    let inst = AsyncInst(
      ref: ref, captures: captures, parent: block(containing: point), ranges: ranges)
    return insert(inst: inst, at: point)
  }

  public mutating func insertAwait(
    value: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: value).isObject, "'value' must have an object type")
    assert(type(of: value).valType is AsyncType, "'value' must have an asynchronous type")

    let inst = AwaitInst(
      value: value,
      type: .lower((type(of: value).valType as! AsyncType).base),
      parent: block(containing: point),
      range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertBorrowAddr(
    isMutable: Bool = false,
    source: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: source).isAddress, "'source' must have an address type")

    let inst = BorrowAddrInst(
      isMutable: isMutable,
      source: source,
      type: type(of: source),
      parent: block(containing: point),
      range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertBorrowExistAddr(
    source: Operand,
    interfaceType: VILType,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: source).isAddress, "'source' must have an address type")
    assert(interfaceType.isAddress, "'interfaceType' must be an address type")
    assert(interfaceType.isExistential, "'interfaceType' must be existential")

    let inst = BorrowExistAddrInst(
      source: source,
      type: interfaceType,
      parent: block(containing: point),
      range: range)
    return insert(inst: inst, at: point)
  }

  @discardableResult
  public mutating func insertBranch(
    dest: BasicBlockIndex,
    args: [Operand] = [],
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    let origin = block(containing: point)
    let inst = BranchInst(dest: dest, args: args, parent: origin, range: range)

    functions[blocks[origin].parent]!.cfg.insertControlEdge(from: origin, to: inst.dest)

    return insert(inst: inst, at: point)
  }

  public mutating func insertCheckedCast(
    value: Operand,
    type: VILType,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(self.type(of: value).isObject, "'value' must have an object type")

    let inst = CheckedCastInst(
      value: value, type: type, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertCheckedCastAddr(
    source: Operand,
    type: VILType,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(self.type(of: source).isAddress, "'source' must have an address type")

    let inst = CheckedCastAddrInst(
      source: source, type: type, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  @discardableResult
  public mutating func insertCheckedCastBranch(
    value: Operand,
    type: VILType,
    succ: BasicBlockIndex,
    fail: BasicBlockIndex,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(self.type(of: value).isObject, "'value' must have an object type")
    assert(type.isObject, "'type' must be an object type")

    let origin = block(containing: point)
    let inst = CheckedCastBranchInst(
      value: value, type: type, succ: succ, fail: fail, parent: origin, range: range)

    modify(value: &functions[blocks[origin].parent]!.cfg, with: { cfg in
      cfg.insertControlEdge(from: origin, to: succ)
      cfg.insertControlEdge(from: origin, to: fail)
    })

    return insert(inst: inst, at: point)
  }

  @discardableResult
  public mutating func insertCondBranch(
    cond: Operand,
    succ: BasicBlockIndex, succArgs: [Operand],
    fail: BasicBlockIndex, failArgs: [Operand],
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: cond).isObject, "'cond' must have an object type")

    let origin = block(containing: point)
    let inst = CondBranchInst(
      cond: cond,
      succ: succ,
      succArgs: succArgs,
      fail: fail,
      failArgs: failArgs,
      parent: origin,
      range: range)

    modify(value: &functions[blocks[origin].parent]!.cfg, with: { cfg in
      cfg.insertControlEdge(from: origin, to: succ)
      cfg.insertControlEdge(from: origin, to: fail)
    })

    return insert(inst: inst, at: point)
  }

  @discardableResult
  public mutating func insertCondFail(
    cond: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: cond).isObject, "'cond' must have an object type")

    let inst = CondFail(cond: cond, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  @discardableResult
  public mutating func insertFail(
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    return insertCondFail(
      cond: Operand(IntValue.makeTrue(context: context)), range: range, at: point)
  }

  @discardableResult
  public mutating func insertDeallocStack(
    alloc: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(
      alloc.inst.map({ instructions[$0] }) is AllocStackInst,
      "'alloc' must be an 'alloc_stack' instruction")

    let inst = DeallocStackInst(alloc: alloc, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  @discardableResult
  public mutating func insertDelete(
    value: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: value).isObject, "'value' must have an object type")

    let inst = DeleteInst(value: value, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  @discardableResult
  public mutating func insertDeleteAddr(
    target: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: target).isAddress, "'target' must have an address type")

    let inst = DeleteAddrInst(target: target, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  @discardableResult
  public mutating func insertEndBorrowAddr(
    source: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: source).isAddress, "'source' must have an address type")

    let inst = EndBorrowInst(source: source, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  @discardableResult
  public mutating func insertInitExist(
    container: Operand,
    object: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: container).isAddress, "'container' must have an address type")
    assert(type(of: object).isObject, "'object' must have an object type")

    let inst = InitExistInst(
      container: container, object: object, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertIsCastableAddr(
    source: Operand,
    type: VILType,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(self.type(of: source).isAddress, "'source' must have an address type")

    let inst = IsCastableAddrInst(
      source: source, type: type, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  @discardableResult
  public mutating func insertHalt(
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    let inst = HaltInst(parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertLoad(
    source: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: source).isAddress, "'source' must have an address type")

    let inst = LoadInst(
      source: source,
      type: type(of: source).object,
      parent: block(containing: point),
      range: range)
    return insert(inst: inst, at: point)
  }

  @discardableResult
  public mutating func insertMoveAddr(
    from source: Operand,
    to target: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: source).isAddress, "'source' must have an address type")
    assert(type(of: target).isAddress, "'target' must have an address type")

    let inst = MoveAddrInst(
      source: source, target: target, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  /// Inserts the creation of a `nil` object.
  public mutating func insertNil(
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    let decl = context.getTypeDecl(for: .Nil) as! ProductTypeDecl
    let type = VILType.lower(decl.instanceType)
    return insertRecord(typeDecl: decl, type: type, parts: [], range: range, at: point)
  }

  public mutating func insertOpenExist(
    container: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: container).isObject, "'container' must have an object type")

    let interfaceType = type(of: container).valType
    assert(interfaceType.isExistential)
    let witness = interfaceType.context.witnessType(interface: interfaceType)

    let inst = OpenExistInst(
      container: container, type: witness, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertOpenExistAddr(
    container: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: container).isAddress, "'container' must have an address type")

    let interfaceType = type(of: container).valType
    assert(interfaceType.isExistential)
    let witness = interfaceType.context.witnessType(interface: interfaceType)

    let inst = OpenExistAddrInst(
      container: container, type: witness, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertPartialApply(
    delegator: FunRef,
    args: [Operand],
    range: SourceRange? = nil,
    argsRanges: [SourceRange?]? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(argsRanges == nil || argsRanges!.count == args.count)
    var ranges = argsRanges ?? Array(repeating: nil, count: args.count)
    ranges.append(range)

    let inst = PartialApplyInst(
      delegator: delegator,
      args: args,
      parent: block(containing: point),
      ranges: ranges)
    return insert(inst: inst, at: point)
  }

  public mutating func insertPointerCast(
    pointer: Operand,
    type: VILType,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(self.type(of: pointer).isAddress, "'pointer' must have an address type")
    assert(type.isAddress, "'type' must be an address type")

    let inst = PointerCastInst(
      pointer: pointer, type: type, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertRecord(
    typeDecl: NominalTypeDecl,
    type: VILType,
    parts: [Operand],
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type.isObject, "'type' must be an object type")

    let inst = RecordInst(
      typeDecl: typeDecl, type: type, parts: parts, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertRecordMember(
    record: Operand,
    memberDecl: VarDecl,
    type: VILType,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(self.type(of: record).isObject, "'record' must have an object type")
    assert(type.isObject, "'type' must be an object type")

    let inst = RecordMemberInst(
      record: record,
      memberDecl: memberDecl,
      type: type,
      parent: block(containing: point),
      range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertRecordMemberAddr(
    record: Operand,
    memberDecl: VarDecl,
    type: VILType,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(self.type(of: record).isAddress, "'record' must have an address type")
    assert(type.isAddress, "'type' must be an address type")

    let inst = RecordMemberAddrInst(
      record: record,
      memberDecl: memberDecl,
      type: type,
      parent: block(containing: point),
      range: range)
    return insert(inst: inst, at: point)
  }

  @discardableResult
  public mutating func insertRet(
    value: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: value).isObject, "'value' must have an object type")

    let inst = RetInst(value: value, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  @discardableResult
  public mutating func insertStore(
    _ value: Operand,
    to target: Operand,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: value).isObject, "'value' must have an object type")
    assert(type(of: target).isAddress, "'target' must have an address type")

    let inst = StoreInst(
      value: value, target: target, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertThinToThick(
    ref: FunRef,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    let inst = ThinToThickInst(ref: ref, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertTuple(
    type: TupleType,
    parts: [Operand],
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(
      parts.allSatisfy({ self.type(of: $0).isObject }),
      "all operands must have an object type")

    let inst = TupleInst(type: type, parts: parts, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertWitnessMethod(
    container: Operand,
    decl: BaseFunDecl,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: container).isObject, "'container' must have an object type")

    let inst = WitnessMethodInst(
      container: container, decl: decl, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  public mutating func insertWitnessMethodAddr(
    container: Operand,
    decl: BaseFunDecl,
    range: SourceRange? = nil,
    at point: InsertionPoint
  ) -> InstIndex {
    assert(type(of: container).isAddress, "'container' must have an object type")

    let inst = WitnessMethodAddrInst(
      container: container, decl: decl, parent: block(containing: point), range: range)
    return insert(inst: inst, at: point)
  }

  /// Removes an instruction.
  public mutating func remove(inst instIndex: InstIndex) {
    assert(uses[Operand(instIndex), default: []].isEmpty, "instruction has uses")
    uses[Operand(instIndex)] = nil

    // Update the use lists of the instruction's operands.
    let inst = instructions[instIndex]
    for op in inst.operands {
      uses[op]!.removeAll(where: { $0.user == instIndex })
    }

    // Remove the instruction from the block.
    modify(value: &blocks[inst.parent].instructions, with: { instructions in
      let i = instructions.firstIndex(of: instIndex)
      instructions.remove(at: i!)
    })

    // Update the function's control-flow graph if necessary.
    switch inst {
    case let inst as BranchInst:
      functions[blocks[inst.parent].parent]!.cfg.removeControlEdge(
        from: inst.parent, to: inst.dest)

    case let inst as CheckedCastBranchInst:
      modify(value: &functions[blocks[inst.parent].parent]!.cfg, with: { cfg in
        cfg.removeControlEdge(from: inst.parent, to: inst.succ)
        cfg.removeControlEdge(from: inst.parent, to: inst.fail)
      })

    case let inst as CondBranchInst:
      modify(value: &functions[blocks[inst.parent].parent]!.cfg, with: { cfg in
        cfg.removeControlEdge(from: inst.parent, to: inst.succ)
        cfg.removeControlEdge(from: inst.parent, to: inst.fail)
      })

    default:
      break
    }
  }

  /// Inserts an instruction at the specified insertion point.
  private mutating func insert(inst: Inst, at point: InsertionPoint) -> InstIndex {
    // Insert the instruction into the module.
    let instIndex = instructions.insert(inst)

    // Update the containing basic block.
    switch point {
    case .startOf(let blockIndex):
      let i = blocks[blockIndex].instructions.startIndex
      blocks[blockIndex].instructions.insert(instIndex, at: i)

    case .endOf(let blockIndex):
      blocks[blockIndex].instructions.append(instIndex)

    case .before(let succIndex):
      let blockIndex = instructions[succIndex].parent
      let i = blocks[blockIndex].instructions.firstIndex(of: succIndex)!
      blocks[blockIndex].instructions.insert(instIndex, at: i)

    case .after(let predIndex):
      let blockIndex = instructions[predIndex].parent
      var i = blocks[blockIndex].instructions.firstIndex(of: predIndex)!
      i = blocks[blockIndex].instructions.index(after: i)
      blocks[blockIndex].instructions.insert(instIndex, at: i)
    }

    // Update the use lists of the instruction's operands.
    for i in 0 ..< inst.operands.count {
      uses[inst.operands[i], default: []].append(Use(user: instIndex, index: i))
    }

    return instIndex
  }

}

/// An insertion point for new instructions.
public enum InsertionPoint {

  /// Inserts at the start of the specified block.
  case startOf(BasicBlockIndex)

  /// Insert at the end of the specified block.
  case endOf(BasicBlockIndex)

  /// Insert before the specified instruction.
  case before(inst: InstIndex)

  ///Insert after the specified instruction.
  case after(inst: InstIndex)

}
