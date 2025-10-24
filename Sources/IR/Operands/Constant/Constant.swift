/// A constant value in Hylo IR.
public protocol Constant: Hashable, Sendable {

  /// The type of the value.
  var type: IR.`Type` { get }

  /// Returns `true` if `self` is equal to `other`.
  func equals(_ other: any Constant) -> Bool

}

extension Constant {

  public func equals(_ other: any Constant) -> Bool {
    self == (other as? Self)
  }

}
