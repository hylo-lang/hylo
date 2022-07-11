/// Stores `object` at the specified location.
public struct StoreInst: Inst {

  /// The object to store.
  public let object: Operand

  /// The location at which the object is stored.
  public let target: Operand

  public var type: LoweredType { .object(.unit) }

  public var operands: [Operand] { [target] }

}
