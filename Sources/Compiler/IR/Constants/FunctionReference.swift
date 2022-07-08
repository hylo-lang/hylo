/// A Val IR reference to a user function.
public struct FunctionRef: ConstantProtocol, Hashable {

  /// The name of the function.
  public let name: String

  public let type: LoweredType

}

extension FunctionRef: CustomStringConvertible {

  public var description: String {
    "@\(name)"
  }

}
