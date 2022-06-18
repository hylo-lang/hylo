/// The ID of a basic block in a VIR function.
public struct BlockID: Hashable {

  /// The ID of the function containing the block.
  public var function: FunctionID

  /// The index of the block in the containing function.
  public var index: Function.Index

}
