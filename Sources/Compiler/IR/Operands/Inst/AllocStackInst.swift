/// Allocates memory on the stack.
public struct AllocStackInst: Inst {

  /// The type of the object for which storage is allocated.
  public var objectType: Type

  public var range: SourceRange?

  init(_ objectType: Type) {
    self.objectType = objectType
  }

  public var type: LoweredType { .address(objectType) }

  public var operands: [Operand] { [] }

  public func check(in module: Module) -> Bool {
    true
  }

}
