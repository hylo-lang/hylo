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
    if context.allocateBuffer(forKey: .vilConstantValueStore, ofType: ConstantValueStore.self) {
      let errorType = VILType.lower(context.errorType)
      let unitType = VILType.lower(context.unitType)
      context.withBuffer(forKey: .vilConstantValueStore, of: ConstantValueStore.self, { ptr in
        ptr.initialize(
          to: ConstantValueStore(
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

  /// Builds a unique identifier.
  public mutating func makeUID() -> Int {
    return idFactory.makeID()
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
    // Lower the function type.
    let loweredType = VILType.lower(type) as! VILFunType

    // Check if the module already contains a module with the specified name.
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
      let extra = captureTable.map({ (_, value) -> ValType in
        // Captures with mutable semantics are represented by in-out parameters.
        return value.semantics == .val
          ? value.type
          : context.inoutType(of: value.type)
      })

      let paramType = context.tupleType(types: extra + unappliedType.paramTypeList)
      unappliedType = context.funType(paramType: paramType, retType: unappliedType.retType)
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
  ///   - operands: The operands of the basic block.
  ///   - isEntry: A Boolean value that indicates whether the new block should be set as the
  ///     function's entry.
  public mutating func buildBasicBlock(
    params: [ArgumentValue] = [],
    isEntry: Bool = false
  ) -> BasicBlock.ID {
    guard let ip = insertionPointer else { fatalError("invalid insertion pointer") }

    let blockID = makeUID()
    module.functions[ip.funName]!.blocks[blockID] = BasicBlock(params: params)
    if isEntry {
      module.functions[ip.funName]!.entryID = blockID
    }
    return blockID
  }

  /// Builds an argument.
  ///
  /// The argument is created in the function referenced by the builder's insertion point. The
  /// method fails if that insertion point does not refer to a function.
  public mutating func buildArgument(type: VILType) -> ArgumentValue {
    guard let fun = currentFun else { fatalError("invalid insertion pointer") }
    return ArgumentValue(type: type, function: fun.name)
  }

  // MARK: Constants

  /// Builds a poising value.
  public func buildPoison() -> PoisonValue {
    return context.withBuffer(
      forKey: .vilConstantValueStore, of: ConstantValueStore.self, { (ptr) -> PoisonValue in
        ptr.pointee.poison
      })!
  }

  /// Builds a unit value.
  public func buildUnit() -> UnitValue {
    return context.withBuffer(
      forKey: .vilConstantValueStore, of: ConstantValueStore.self, { (ptr) -> UnitValue in
        ptr.pointee.unit
      })!
  }

  /// Builds an integer literal.
  public func buildIntLiteral(value: Int) -> IntLiteralValue {
    return context.withBuffer(
      forKey: .vilConstantValueStore, of: ConstantValueStore.self, { (ptr) -> IntLiteralValue in
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
      forKey: .vilConstantValueStore, of: ConstantValueStore.self, { (ptr) -> BuiltinFunRef in
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
      forKey: .vilConstantValueStore, of: ConstantValueStore.self, { (ptr) -> FunRef in
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
      forKey: .vilConstantValueStore, of: ConstantValueStore.self, { (ptr) -> NullAddr in
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

  /// Builds an `alloc_existential` instruction.
  ///
  /// - Parameters:
  ///   - container: The address of an existential container.
  ///   - witness: The type of the exitential package's witness. `witness` must conform to the
  ///     view(s) described by the type of the container.
  public mutating func buildAllocExistential(
    container: Value,
    witness: VILType
  ) -> AllocExistentialInst {
    precondition(witness.isObject, "'witness' must be an object type")
    precondition(!witness.isExistential, "'witness' cannot be existential")

    let inst = AllocExistentialInst(container: container, witness: witness)
    insert(inst)
    return inst
  }

  /// Builds an `alloc_stack` instruction.
  ///
  /// - Parameter type: The type of the allocated object. `type` must be an object type.
  public mutating func buildAllocStack(type: VILType) -> AllocStackInst {
    precondition(type.isObject, "allocated type must be an object type")

    let inst = AllocStackInst(allocatedType: type)
    insert(inst)
    return inst
  }

  /// Builds an `apply` instruction.
  ///
  /// - Parameters:
  ///   - callee: The function to apply. `callee` must have a function type.
  ///   - args: The arguments of the function application.
  public mutating func buildApply(callee: Value, args: [Value]) -> ApplyInst {
    guard let calleeType = callee.type.valType as? FunType else {
      preconditionFailure("apply on non-function operand")
    }

    // FIXME: Perhaps we could do some argument validation here?

    let inst = ApplyInst(callee: callee, args: args, type: .lower(calleeType.retType))
    insert(inst)
    return inst
  }

  /// Builds a `async` instruction.
  ///
  /// - Parameters:
  ///   - fun: The function that represents the asynchronous execution.
  ///   - args: The values captured by the asynchronous expression.
  public mutating func buildAsync(ref: FunRef, captures: [Value] = []) -> AsyncInst {
    let inst = AsyncInst(ref: ref, captures: captures)
    insert(inst)
    return inst
  }

  /// Builds an `await` instruction.
  ///
  /// - Parameter value: The value being awaited.
  public mutating func buildAwait(value: Value) -> AwaitInst {
    precondition(value.type.valType is AsyncType, "awaited value must have an asynchronous type")

    let inst = AwaitInst(value: value)
    insert(inst)
    return inst
  }

  /// Builds an unconditional `branch` instruction.
  ///
  /// - Parameters:
  ///   - dest: The basic block to which the execution should branch. `dest` must be in the current
  ///     function.
  ///   - args: The value of earch argument passed to the destination block. `args` must match the
  ///     formal arguments expected expected by `dest.`
  @discardableResult
  public mutating func buildBranch(dest: BasicBlock.ID, args: [Value] = []) -> BranchInst {
    guard let fun = currentFun else { fatalError("invalid insertion pointer") }
    precondition(fun.blocks[dest] != nil, "invalid destination")

    let inst = BranchInst(dest: dest, args: args)
    insert(inst)
    return inst
  }

  /// Builds a `check_cast_addr` instruction.
  ///
  /// - Parameters:
  ///   - source: The address to convert.
  ///   - type: The type to which the address is converted.
  public mutating func buildCheckedCastAddr(source: Value, type: VILType) -> CheckedCastAddrInst {
    precondition(type.isAddress, "type must be an address type")

    let inst = CheckedCastAddrInst(source: source, type: type)
    insert(inst)
    return inst
  }

  /// Builds a `cond_branch` instruction.
  ///
  /// - Parameters:
  ///   - cond: A condition that determines the block to which the execution should branch. `cond`
  ///   must be a Boolean value.
  ///   - thenDest: The basic block to which the execution should branch if the condition holds
  ///     `thenDest` must be in the current function, and be different than `elseDest`.
  ///   - thenArgs: The arguments of `thenDest`.
  ///   - elseDest: The basic block to which the execution should branch if the condition doesn not
  ///     hold. `elseDest` must be in the current function, and be different than `thenDest`.
  ///   - elseArgs: The arguments of `elseDest`.
  @discardableResult
  public mutating func buildCondBranch(
    cond: Value,
    thenDest: BasicBlock.ID, thenArgs: [Value] = [],
    elseDest: BasicBlock.ID, elseArgs: [Value] = []
  ) -> CondBranchInst {
    guard let fun = currentFun else { fatalError("invalid insertion pointer") }
    precondition(fun.blocks[thenDest] != nil, "invalid destination")
    precondition(fun.blocks[elseDest] != nil, "invalid destination")

    let inst = CondBranchInst(
      cond: cond, thenDest: thenDest, thenArgs: thenArgs, elseDest: elseDest, elseArgs: elseArgs)
    insert(inst)
    return inst
  }

  /// Builds a `copy_addr` instruction.
  ///
  /// - Parameters:
  ///   - target: The target address of the copy.
  ///   - source: The address of the object to copy.
  @discardableResult
  public mutating func buildCopyAddr(target: Value, source: Value) -> CopyAddrInst {
    let inst = CopyAddrInst(target: target, source: source)
    insert(inst)
    return inst
  }

  /// Builds a `equal_addr` instruction
  ///
  /// - Parameters:
  ///   - lhs: An address.
  ///   - rhs: Another address.
  public mutating func buildEqualAddr(lhs: Value, rhs: Value) -> EqualAddrInst {
    precondition(lhs.type.isAddress, "'lhs' must have an address type")
    precondition(rhs.type.isAddress, "'rhs' must have an address type")

    let inst = EqualAddrInst(lhs: lhs, rhs: rhs)
    insert(inst)
    return inst
  }

  /// Builds a `halt` instruction.
  @discardableResult
  public mutating func buildHalt() -> HaltInst {
    let inst = HaltInst()
    insert(inst)
    return inst
  }

  /// Builds a `load` instruction.
  ///
  /// - Parameter lvalue: The location to load. `lvalue` must have an address type.
  public mutating func buildLoad(location: Value) -> LoadInst {
    precondition(location.type.isAddress, "'location' must have an address type")

    let inst = LoadInst(location: location)
    insert(inst)
    return inst
  }

  /// Builds a `mark_uninitialized` instruction.
  public mutating func buildMarkUninitialized(location: Value) -> MarkUninitializedInst {
    precondition(location.type.isAddress, "'location' must have an address type")

    let inst = MarkUninitializedInst(location: location)
    insert(inst)
    return inst
  }

  /// Builds an `open_existential` instruction.
  ///
  /// - Parameters:
  ///   - container. An existential container.
  ///   - type: The type of the opened value. `type` must be an object type that matches the
  ///     package's witness. It can either be a concrete type, or an "opened" existential.
  public mutating func buildOpenExistential(
    container: Value,
    type: VILType
  ) -> OpenExistentialInst {
    precondition(container.type.isObject, "'container' must have an object type")

    let inst = OpenExistentialInst(container: container, type: type)
    insert(inst)
    return inst
  }

  /// Builds an `open_existential_addr` instruction.
  ///
  /// - Parameters:
  ///   - container: The address of an existential container.
  ///   - type: The type of the opened address. `type` must be an address type that matches the
  ///     package's witness. It can either be a concrete type, or an "opened" existential.
  public mutating func buildOpenExistentialAddr(
    container: Value,
    type: VILType
  ) -> OpenExistentialAddrInst {
    precondition(container.type.isAddress, "'container' must have an address type")
    precondition(type.isAddress, "'type' must be an address type")

    let inst = OpenExistentialAddrInst(container: container, type: type)
    insert(inst)
    return inst
  }

  /// Builds a `partial_apply` instruction.
  ///
  /// - Parameters:
  ///   - delegator: The function being partially applied.
  ///   - partialArgs: The partial list of arguments to the delegator (from left to right).
  public mutating func buildPartialApply(
    delegator: Value,
    partialArgs: [Value]
  ) -> PartialApplyInst {
    precondition(partialArgs.count > 0)

    let inst = PartialApplyInst(delegator: delegator, partialArgs: partialArgs)
    insert(inst)
    return inst
  }

  /// Builds a record value instruction.
  ///
  /// - Parameters:
  ///   - typeDecl: The declaration of a product type.
  ///   - type: The type of the record. `type` must be a value type that matches the instance type
  ///     of `typeDecl`.
  public mutating func buildRecord(typeDecl: ProductTypeDecl, type: VILType) -> RecordInst {
    precondition(type.isObject, "'type' must be an object type")

    let inst = RecordInst(typeDecl: typeDecl, type: type)
    insert(inst)
    return inst
  }

  /// Builds a `record_member` instruction.
  ///
  /// - Parameters:
  ///   - record: The record whose member is being extracted.
  ///   - memberDecl: The declaration of the member being extracted. `memberDecl` must refer to
  ///     a stored member of `record`'s type.
  ///   - type: The type of the member being extracted.
  public mutating func buildRecordMember(
    record: Value,
    memberDecl: VarDecl,
    type: VILType
  ) -> RecordMemberInst {
    precondition(record.type.isObject, "'record' must have an object type")
    precondition(type.isObject, "'type' must have be object type")

    let inst = RecordMemberInst(record: record, memberDecl: memberDecl, type: type)
    insert(inst)
    return inst
  }

  /// Builds a `record_member_addr` instruction.
  ///
  /// - Parameters:
  ///   - record: The address of the the record value for which the member's address is computed.
  ///   - memberDecl: The declaration of the member being extracted. `memberDecl` must refer to
  ///     a stored member of `record`'s type.
  ///   - type: The type of the member being extracted.
  public mutating func buildRecordMemberAddr(
    record: Value,
    memberDecl: VarDecl,
    type: VILType
  ) -> RecordMemberAddrInst {
    precondition(record.type.isAddress, "'record' must have an address type")
    precondition(type.isAddress, "instruction must have an address type")

    let inst = RecordMemberAddrInst(record: record, memberDecl: memberDecl, type: type)
    insert(inst)
    return inst
  }

  /// Builds a `ret` instruction.
  @discardableResult
  public mutating func buildRet(value: Value) -> RetInst {
    precondition(value.type.isObject, "'value' must have an address type")

    let inst = RetInst(value: value)
    insert(inst)
    return inst
  }

  /// Builds a `store` instruction.
  ///
  /// - Parameters:
  ///   - target: The location (or target) of the store.
  ///   - value: The value being stored.
  @discardableResult
  public mutating func buildStore(target: Value, value: Value) -> StoreInst {
    precondition(target.type.isAddress, "'target' must have an address type")
    precondition(value.type.isObject, "'value' must have an object type")
    // FIXME: Additionally, check that the value's type is compatible with the target's.

    let inst = StoreInst(target: target, value: value)
    insert(inst)
    return inst
  }

  /// Builds a `thin_to_thick` instruction.
  ///
  /// - Parameter ref: A reference to a VIL function.
  public mutating func buildThinToThick(ref: FunRef) -> ThinToThickInst {
    let inst = ThinToThickInst(ref: ref)
    insert(inst)
    return inst
  }


  /// Builds a tuple value instruction.
  public mutating func buildTuple(type: TupleType, operands: [Value]) -> TupleInst {
    precondition(operands.allSatisfy({ $0.type.isObject }), "operands must have an object type")

    let inst = TupleInst(type: type, operands: operands)
    insert(inst)
    return inst
  }

  /// Builds a `unsafe_cast_addr` instruction.
  ///
  /// - Parameters:
  ///   - source: The address to convert.
  ///   - type: The type to which the address is converted. `type` must be an address type.
  public mutating func buildUnsafeCastAddr(source: Value, type: VILType) -> UnsafeCastAddrInst {
    precondition(type.isAddress, "'type' must be an address type")

    let inst = UnsafeCastAddrInst(source: source, type: type)
    insert(inst)
    return inst
  }

  /// Builds a `witness_method` instruction.
  public mutating func buildWitnessMethod(
    container: Value,
    decl: BaseFunDecl
  ) -> WitnessMethodInst {
    precondition({
      if let parent = decl.parentDeclSpace as? TypeExtnDecl {
        return parent.extendedDecl is ViewTypeDecl
      } else {
        return decl.parentDeclSpace is ViewTypeDecl
      }
    }(), "'\(decl.debugID)' is not declared in a view")

    let inst = WitnessMethodInst(container: container, decl: decl)
    insert(inst)
    return inst
  }

  private mutating func insert(_ inst: Inst) {
    guard let blockID = insertionPointer?.blockID else { fatalError("invalid insertion pointer") }
    let funName = insertionPointer!.funName

    // Insert the instruction.
    switch insertionPointer!.position {
    case .end:
      module.functions[funName]!.blocks[blockID]!.instructions.append(inst)
    case .at(let index):
      module.functions[funName]!.blocks[blockID]!.instructions.insert(inst, at: index)
    }

    // Update the use lists.
    for i in 0 ..< inst.operands.count {
      inst.operands[i].uses.append(Use(user: inst, index: i))
    }
  }

  private func withConstantValueStore<R>(_ action: (inout ConstantValueStore) -> R) -> R? {
    return context.withBuffer(
      forKey: .vilConstantValueStore,
      of: ConstantValueStore.self,
      { action(&$0.pointee) })
  }

}

fileprivate struct ConstantValueStore {

  let poison: PoisonValue

  let unit: UnitValue

  var intLiterals: [Int: IntLiteralValue] = [:]

  var builtinFunRefs: [VILName: BuiltinFunRef] = [:]

  var funRefs: [VILName: FunRef] = [:]

  var nullAddrs = ReferenceTable<ValType, NullAddr>()

}
