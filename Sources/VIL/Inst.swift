import AST

/// A VIL instruction.
public protocol Inst {
}

/// A stack allocation.
public struct AllocStackInst: Inst, Value {

  /// The type of the value produced by the allocation.
  public let type: VILType

  /// The type of the allocated object.
  public let allocatedType: ValType

}
