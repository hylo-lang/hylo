import AST

/// A builder for VIL code.
public final class Builder {

  /// The module that is being edited.
  public let module: Module

  /// The current basic block in which new instructions are being inserted.
  public var block: BasicBlock?

  /// The current function in which new instructions are being inserted.
  public var function: Function? { block?.function }

  /// A counter to generate unique IDs.
  private var nextID = 0

  /// Creates a new VIL instruction builder.
  ///
  /// - Parameters:
  ///   - context: The AST context in which the builder was created.
  ///   - module: The module to edit with the builder.
  public init(module: Module) {
    self.module = module
  }

  /// Builds a unique identifier.
  public func buildUniqueID() -> Int {
    defer { nextID += 1 }
    return nextID
  }

  /// Retrieves or creates a function with the specified name and type.
  ///
  /// - Parameters:
  ///   - name: The name of the function to retrieve or create.
  ///   - type: The unapplied type of the function. If the module already contains a function with
  ///     the same name, then it must have the same type as `type`.
  ///   - debugName: An optional debug name describing the function.
  public func getOrCreateFunction(
    name     : String,
    type     : FunType,
    debugName: String? = nil
  ) -> Function {
    // Lower the function type.
    let loweredType = VILType.lower(type) as! VILFunType

    // Check if the module already contains a module with the specified name.
    if let function = module.functions[name] {
      precondition(
        function.type.valType == loweredType.valType,
        "function \(name) already exists with a different type")
      return function
    }

    // Create the function object.
    let function = Function(name: name, type: loweredType, debugName: debugName)
    module.functions[name] = function
    return function
  }

  /// Retrieves or create a function from the given declaration.
  ///
  /// - Parameter decl: A function declaration.
  public func getOrCreateFunction(from decl: BaseFunDecl) -> Function {
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
    let captures = decl.computeCaptures()
    if !captures.isEmpty {
      let context   = unappliedType.context
      let extra     = captures.map({ $0.decl.type })
      let paramType = context.tupleType(types: extra + unappliedType.paramTypeList)
      unappliedType = context.funType(
        paramType: paramType, retType: unappliedType.retType)
    }

    // Create the function.
    return getOrCreateFunction(name: name, type: unappliedType, debugName: decl.debugID)
  }

  /// Builds an `alloc_stack` instruction.
  ///
  /// - Parameter type: The type of the allocated object. `type` must be an object type.
  public func buildAllocStack(type: VILType) -> AllocStackInst {
    let inst = AllocStackInst(allocatedType: type)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds an `alloc_existential` instruction.
  ///
  /// - Parameters:
  ///   - container: The address of an existential container.
  ///   - witness: The type of the exitential package's witness. `witness` must conform to the
  ///     view(s) described by the type of the container.
  public func buildAllocExistential(container: Value, witness: VILType) -> AllocExistentialInst {
    let inst = AllocExistentialInst(container: container, witness: witness)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds an `open_existential` instruction.
  ///
  /// - Parameters:
  ///   - container. An existential container.
  ///   - type: The type of the opened value. `type` must be an object type that matches the
  ///     package's witness. It can either be a concrete type, or an "opened" existential.
  public func buildOpenExistential(container: Value, type: VILType) -> OpenExistentialInst {
    let inst = OpenExistentialInst(container: container, type: type)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds an `open_existential_addr` instruction.
  ///
  /// - Parameters:
  ///   - container: The address of an existential container.
  ///   - type: The type of the opened address. `type` must be an address type that matches the
  ///     package's witness. It can either be a concrete type, or an "opened" existential.
  public func buildOpenExistentialAddr(
    container: Value, type: VILType
  ) -> OpenExistentialAddrInst {
    let inst = OpenExistentialAddrInst(container: container, type: type)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `copy_addr` instruction.
  ///
  /// - Parameters:
  ///   - dest: The target address of the copy.
  ///   - source: The address of the object to copy.
  @discardableResult
  public func buildCopyAddr(dest: Value, source: Value) -> CopyAddrInst {
    let inst = CopyAddrInst(dest: dest, source: source)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `unsafe_cast_addr` instruction.
  ///
  /// - Parameters:
  ///   - source: The address to convert.
  ///   - type: The type to which the address is converted. `type` must be an address type.
  public func buildUnsafeCastAddr(source: Value, type: VILType) -> UnsafeCastAddrInst {
    precondition(type.isAddress, "the type of the cast must be an address")
    let inst = UnsafeCastAddrInst(source: source, type: type)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `check_cast_addr` instruction.
  ///
  /// - Parameters:
  ///   - source: The address to convert.
  ///   - type: The type to which the address is converted.
  public func buildCheckedCastAddr(source: Value, type: VILType) -> CheckedCastAddrInst {
    let inst = CheckedCastAddrInst(source: source, type: type)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `witness_method` instruction.
  public func buildWitnessMethod(container: Value, decl: BaseFunDecl) -> WitnessMethodInst {
    precondition({
      if let parent = decl.parentDeclSpace as? TypeExtnDecl {
        return parent.extendedDecl is ViewTypeDecl
      } else {
        return decl.parentDeclSpace is ViewTypeDecl
      }
    }(), "'\(decl.debugID)' is not declared in a view")

    let inst = WitnessMethodInst(container: container, decl: decl)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a function application.
  ///
  /// - Parameters:
  ///   - fun: The function to apply. `fun` must have a function type.
  ///   - args: The arguments of the function application.
  public func buildApply(fun: Value, args: [Value]) -> ApplyInst {
    guard let funType = fun.type.valType as? FunType else {
      preconditionFailure("apply to non-function type '\(fun.type)'")
    }

    // FIXME: Perhaps we could do some argument validation here?

    let inst = ApplyInst(fun: fun, args: args, type: .lower(funType.retType))
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a partial application.
  ///
  /// - Parameters:
  ///   - fun: The function being partially applied.
  ///   - args: The partial list of arguments of the function application (from left to right).
  public func buildPartialApply(fun: Value, args: [Value]) -> PartialApplyInst {
    let inst = PartialApplyInst(fun: fun, args: args)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a record value instruction.
  ///
  /// - Parameters:
  ///   - typeDecl: The declaration of a product type.
  ///   - type: The type of the record. `type` must be a value type that matches the instance type
  ///     of `typeDecl`.
  public func buildRecord(typeDecl: ProductTypeDecl, type: VILType) -> RecordInst {
    let inst = RecordInst(typeDecl: typeDecl, type: type)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `record_member` instruction.
  ///
  /// - Parameters:
  ///   - record: The record whose member is being extracted.
  ///   - memberDecl: The declaration of the member being extracted. `memberDecl` must refer to
  ///     a stored member of `record`'s type.
  ///   - type: The type of the member being extracted.
  public func buildRecordMember(
    record: Value,
    memberDecl: VarDecl,
    type: VILType
  ) -> RecordMemberInst {
    let inst = RecordMemberInst(record: record, memberDecl: memberDecl, type: type)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `record_member_addr` instruction.
  ///
  /// - Parameters:
  ///   - record: The address of the the record value for which the member's address is computed.
  ///   - memberDecl: The declaration of the member being extracted. `memberDecl` must refer to
  ///     a stored member of `record`'s type.
  ///   - type: The type of the member being extracted.
  public func buildRecordMemberAddr(
    record: Value,
    memberDecl: VarDecl,
    type: VILType
  ) -> RecordMemberAddrInst {
    let inst = RecordMemberAddrInst(record: record, memberDecl: memberDecl, type: type)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a tuple value instruction.
  public func buildTuple(type: TupleType, elems: [Value]) -> TupleInst {
    let inst = TupleInst(type: type, elems: elems)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `async` instruction.
  ///
  /// - Parameters:
  ///   - fun: The function that represents the asynchronous execution.
  ///   - args: The values captured by the asynchronous expression.
  public func buildAsync(fun: Function, args: [Value] = []) -> AsyncInst {
    let inst = AsyncInst(fun: fun, args: args)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds an `await` instruction.
  ///
  /// - Parameter value: The value being awaited.
  public func buildAwait(value: Value) -> AwaitInst {
    let inst = AwaitInst(value: value)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `store` instruction.
  ///
  /// - Parameters:
  ///   - lvalue: The location (or target) of the store.
  ///   - rvalue: The value being stored.
  @discardableResult
  public func buildStore(lvalue: Value, rvalue: Value) -> StoreInst {
    let inst = StoreInst(lvalue: lvalue, rvalue: rvalue)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `load` instruction.
  ///
  /// - Parameter lvalue: The location to load. `lvalue` must have an address type.
  public func buildLoad(lvalue: Value) -> LoadInst {
    let inst = LoadInst(lvalue: lvalue)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `equal_addr` instruction
  ///
  ///  - Parameters:
  ///    - lhs: An address.
  ///    - rhs: Another address.
  public func buildEqualAddr(lhs: Value, rhs: Value) -> EqualAddrInst {
    let inst = EqualAddrInst(lhs: lhs, rhs: rhs)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds an unconditional `branch` instruction.
  ///
  /// - Parameters:
  ///   - dest: The basic block to which the execution should branch. `dest` must be in the current
  ///     function.
  ///   - args: The arguments of the destination block. `args` Should must the number and type of
  ///     the arguments expected by `dest.`
  @discardableResult
  public func buildBranch(dest: BasicBlock, args: [Value] = []) -> BranchInst {
    precondition(dest.function === function, "invalid destination")

    let inst = BranchInst(dest: dest, args: args)
    block!.instructions.append(inst)
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
  public func buildCondBranch(
    cond: Value,
    thenDest: BasicBlock, thenArgs: [Value] = [],
    elseDest: BasicBlock, elseArgs: [Value] = []
  ) -> CondBranchInst {
    precondition(thenDest.function === function, "invalid destination")
    precondition(elseDest.function === function, "invalid destination")

    let inst = CondBranchInst(
      cond: cond, thenDest: thenDest, thenArgs: thenArgs, elseDest: elseDest, elseArgs: elseArgs)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `ret` instruction.
  @discardableResult
  public func buildRet(value: Value) -> RetInst {
    let inst = RetInst(value: value)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `halt` instruction.
  @discardableResult
  public func buildHalt() -> HaltInst {
    let inst = HaltInst()
    block!.instructions.append(inst)
    return inst
  }

}
