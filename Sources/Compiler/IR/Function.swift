import Utils

/// A function lowered to VIR.
public struct Function {

  /// The blocks in the function.
  public var blocks: StableArray<Block>

}

extension Function {

  public typealias Index = StableArray<Block>.Index

}
