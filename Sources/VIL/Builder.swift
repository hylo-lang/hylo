import AST

/// A builder for VIL code.
public final class Builder {

  /// The module that is being edited.
  public let module: Module

  /// The current basic block in which new instructions are being inserted.
  public var block: BasicBlock?

  /// The current function in which new instructions are being inserted.
  public var function: Function? { block?.function }

  /// Creates a new VIL instruction builder.
  ///
  /// - Parameters:
  ///   - context: The AST context in which the builder was created.
  ///   - module: The module to edit with the builder.
  public init(module: Module) {
    self.module = module
  }

  /// Retrieves or creates a function with the specified name and type.
  ///
  /// - Parameters:
  ///   - name: The name of the function to retrieve or create.
  ///   - type: The type of the function. If the module already contains a function `name`, then it
  ///     has to have the same type as `type`.
  public func getOrCreateFunction(name: String, type: FunType) -> Function {
    if let function = module.functions[name] {
      precondition(function.type === type, "function \(name) already exists with a different type")
      return function
    }

    let function = Function(name: name, type: type)
    module.functions[name] = function
    return function
  }

  /// Retrieves or create a function from the given declaration.
  ///
  /// - Parameter funDecl: A function declaration.
  public func getOrCreateFunction(from funDecl: BaseFunDecl) -> Function {
    var mangler = Mangler()
    mangler.append(funDecl: funDecl)
    return getOrCreateFunction(name: mangler.finalize(), type: funDecl.unappliedType as! FunType)
  }

  /// Builds a stack allocation.
  ///
  /// - Parameter type: The type of the allocated object.
  public func buildAllocStack(type: ValType) -> AllocStackInst {
    let inst = AllocStackInst(allocatedType: type)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `pack` instruction.
  ///
  /// - Parameters:
  ///   - value: The value to pack.
  ///   - interface: The interface of the existential container. `value` must conform to all views
  ///     described by `interface`.
  public func buildPack(value: Value, interface: ViewType) -> PackInst {
    let inst = PackInst(value: value, interface: interface)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a function application.
  ///
  /// - Parameters:
  ///   - fun: The function to apply. `fun` must have a function type.
  ///   - args: The arguments of the function application.
  public func buildApply(fun: Value, args: [Value]) -> ApplyInst {
    guard case .object(let funType as FunType) = fun.type else {
      preconditionFailure("apply to non-function type '\(fun.type)'")
    }

    // FIXME: Perhaps we could do some argument validation here?

    let inst = ApplyInst(fun: fun, args: args, type: .object(funType.retType))
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a record value instruction.
  ///
  /// - Parameter typeDecl: The declaration of the type of which the record value is an instance.
  public func buildRecord(typeDecl: NominalTypeDecl) -> RecordInst {
    let inst = RecordInst(typeDecl: typeDecl)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `record_member` instruction.
  ///
  /// - Parameters:
  ///   - record: The record value whose member is extracted.
  ///   - memberDecl: The declaration of the extracted member. `memberDecl` must describe a stored
  ///     member of the product type.
  public func buildRecordMember(record: Value, memberDecl: VarDecl) -> RecordMemberInst {
    let inst = RecordMemberInst(record: record, memberDecl: memberDecl)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a `record_member_addr` instruction.
  ///
  /// - Parameters:
  ///   - record: The record value for which the member's address is computed.
  ///   - memberDecl: The declaration of the member whose address is computed. `memberDecl` must
  ///     describe a stored member of the product type.
  public func buildRecordMemberAddr(record: Value, memberDecl: VarDecl) -> RecordMemberAddrInst {
    let inst = RecordMemberAddrInst(record: record, memberDecl: memberDecl)
    block!.instructions.append(inst)
    return inst
  }

  /// Builds a tuple value instruction.
  public func buildTuple(type: TupleType, elems: [Value]) -> TupleInst {
    let inst = TupleInst(type: type, elems: elems)
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

  /// Builds an unconditional `branch` instruction.
  ///
  /// - Parameters:
  ///   - dest: The basic block to which the execution should branch. `dest` must be in the current
  ///     function.
  ///   - args: The arguments of the destination block. `args` Should must the number and type of
  ///     the arguments expected by `dest.`
  @discardableResult
  public func buildBranch(dest: BasicBlock, args: [Value]) -> BranchInst {
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
    thenDest: BasicBlock, thenArgs: [Value],
    elseDest: BasicBlock, elseArgs: [Value]
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

}
