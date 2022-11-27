/// A void constant.
public struct VoidConstant: ConstantProtocol, Hashable {

  public init() {}

  public var type: LoweredType { .object(AnyType.void) }

}

extension VoidConstant: CustomStringConvertible {

  public var description: String { "void" }

}
