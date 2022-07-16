/// Allocates memory on the stack.
public struct AllocStackInst: Inst {

  /// The type for which storage is allocated.
  public var allocatedType: Type

  public var range: SourceRange?

  init(_ allocatedType: Type, range: SourceRange? = nil) {
    self.allocatedType = allocatedType
    self.range = range
  }

  public var types: [LoweredType] { [.address(allocatedType)] }

  public var operands: [Operand] { [] }

  public func check(in module: Module) -> Bool {
    true
  }

}
