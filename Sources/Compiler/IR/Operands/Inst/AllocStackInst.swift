/// Allocates memory on the stack.
public struct AllocStackInst: Inst {

  /// The type of the object for which storage is allocated.
  public let objectType: Type

  init(_ objectType: Type) {
    self.objectType = objectType
  }

  public var type: LoweredType { .address(objectType) }

  public var operands: [Operand] { [] }

}
