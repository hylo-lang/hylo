import AST

/// A builder for VIL code.
public final class Builder {

  /// The module that is being built.
  public let module: Module

  /// The current basic block in which new instructions are being inserted.
  public var block: BasicBlock?

  /// The current function in which new instructions are being inserted.
  public var function: Function? { block?.function }

  public init(module: Module) {
    self.module = module
  }

  /// Buils a stack allocation.
  ///
  /// - Parameter type: The type of the allocated object.
  public func buildAllocStack(type: ValType) -> AllocStackInst {
    let inst = AllocStackInst(type: .address(type), allocatedType: type)
    block!.instructions.append(inst)
    return inst
  }

}
