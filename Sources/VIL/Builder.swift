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
  ///   has to have the same type as `type`.
  public func getOrCreateFunction(name: String, type: FunType) -> Function {
    if let function = module.functions[name] {
      precondition(function.type === type, "function \(name) already exists with a different type")
      return function
    }

    let function = Function(name: name, type: type)
    module.functions[name] = function
    return function
  }

  /// Buils a stack allocation.
  ///
  /// - Parameter type: The type of the allocated object.
  public func buildAllocStack(type: ValType) -> AllocStackInst {
    let inst = AllocStackInst(type: .address(type), allocatedType: type)
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

  /// Builds a `record_member_addr` instruction.
  ///
  /// - Parameters:
  ///   - record: The record value for which the member's address is computed.
  ///   - memberDecl: The declaration of the member whose address is computed.
  public func buildRecordMemberAddr(record: Value, memberDecl: VarDecl) -> RecordMemberAddrInst {
    let inst = RecordMemberAddrInst(record: record, memberDecl: memberDecl)
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

}
