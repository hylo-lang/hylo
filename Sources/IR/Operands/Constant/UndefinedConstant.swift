import Core

/// An undefined value in Val IR.
public struct UndefinedConstant: Constant, Hashable {

  /// The Val IR type of the constant.
  public let type: LoweredType

  /// Creates an instance representing an undefined value of `type`.
  ///
  /// - Requires: `type` a concrete, canonical type.
  public init(of type: AnyType) {
    precondition(type[.isCanonical])
    self.type = .object(type)
  }

}

extension UndefinedConstant: CustomStringConvertible {

  public var description: String { "undefined" }

}
