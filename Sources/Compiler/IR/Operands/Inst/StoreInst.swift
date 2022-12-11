/// Stores `object` at the specified location.
public struct StoreInst: Inst {

  /// The object to store.
  public let object: Operand

  /// The location at which the object is stored.
  public let target: Operand

  public let range: SourceRange?

  init(_ object: Operand, to target: Operand, range: SourceRange? = nil) {
    self.object = object
    self.target = target
    self.range = range
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [target] }

  public var isTerminator: Bool { false }

  public func check(in module: Module) -> Bool { true }

}
