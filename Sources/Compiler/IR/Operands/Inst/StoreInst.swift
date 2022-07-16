/// Stores `object` at the specified location.
public struct StoreInst: Inst {

  /// The object to store.
  public var object: Operand

  /// The location at which the object is stored.
  public var target: Operand

  public var range: SourceRange?

  init(_ object: Operand, to target: Operand) {
    self.object = object
    self.target = target
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [target] }

  public func check(in module: Module) -> Bool {
    true
  }

}
