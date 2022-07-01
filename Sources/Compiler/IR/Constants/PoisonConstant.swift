/// A poison value.
public struct PoisonConstant: ConstantProtocol, Hashable {

  /// The type of the poison.
  public let type: IRType

}

extension PoisonConstant: CustomStringConvertible {

  public var description: String { "poison" }

}
