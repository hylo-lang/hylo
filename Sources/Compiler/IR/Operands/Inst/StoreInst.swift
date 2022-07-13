/// Stores `object` at the specified location.
public struct StoreInst: Inst {

  /// The object to store.
  public var object: Operand

  /// The location at which the object is stored.
  public var target: Operand

  public var range: SourceRange?

  public var type: LoweredType { .object(.unit) }

  public var operands: [Operand] { [target] }

}
