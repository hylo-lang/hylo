// Takes a member from a record.
public struct TakeMemberInst: Inst {

  /// The type of the member being taken.
  public var memberType: LoweredType

  /// The type of the object from which the object is taken.
  public var objectType: LoweredType

  /// The value of the record whose member is take.
  public var value: Operand

  /// A non-empty sequence of indices identifying a sub-object of `value`.
  public var path: [Int]

  public var range: SourceRange?

  public var types: [LoweredType] { [memberType, objectType] }

  public var operands: [Operand] { [value] }

  public func check(in module: Module) -> Bool {
    true
  }

}
