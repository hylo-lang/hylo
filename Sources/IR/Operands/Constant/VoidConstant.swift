import FrontEnd

/// A void constant.
public struct VoidConstant: Constant, Hashable {

  public init() {}

  public var type: IR.`Type` { .object(AnyType.void) }

}

extension VoidConstant: CustomStringConvertible {

  public var description: String { "void" }

}
