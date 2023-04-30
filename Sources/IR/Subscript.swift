/// A subscript lowered to Val IR.
public struct Subscript {

  /// The identity of the function implementing the opening subscript.
  public let opening: Function.ID

  /// The identity of the function implementing the closing subscript.
  public let closing: Function.ID

}
