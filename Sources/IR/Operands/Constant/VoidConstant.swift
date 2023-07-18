import Core

/// A void constant.
public struct VoidConstant: Constant, Hashable {

  public init() {}

  public var type: IR.LoweredType { .object(AnyType.void) }

}

extension VoidConstant: CustomStringConvertible {

  public var description: String { "void" }

}
