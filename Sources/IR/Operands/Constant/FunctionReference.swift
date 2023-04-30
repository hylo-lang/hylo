/// A Val IR reference to a user function.
public struct FunctionRef: Constant, Hashable {

  /// The ID of the referred IR function.
  public let function: Function.ID

  /// The type of the referred IR function.
  public let type: LoweredType

  /// Creates an instance with the given properties.
  public init(to function: Function.ID, type: LoweredType) {
    self.function = function
    self.type = type
  }

}

extension FunctionRef: CustomStringConvertible {

  public var description: String { "@\(function)" }

}
