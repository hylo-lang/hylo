/// A constant value in Val IR.
public protocol Constant: Hashable {

  /// The type of the value.
  var type: LoweredType { get }

  /// Returns `true` if `self` is equal to `other`.
  func equals(_ other: any Constant) -> Bool

}

extension Constant {

  public func equals(_ other: any Constant) -> Bool {
    self == (other as? Self)
  }

}
