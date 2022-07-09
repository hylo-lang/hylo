/// A unit constant.
public struct UnitConstant: ConstantProtocol, Hashable {

  public init() {}

  public var type: LoweredType { .object(.unit) }

}

extension UnitConstant: CustomStringConvertible {

  public var description: String { "unit" }

}
