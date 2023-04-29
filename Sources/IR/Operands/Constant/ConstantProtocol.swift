/// A protocol describing the API of a Val IR constant's associated value.
public protocol ConstantProtocol: Hashable {

  /// The type of the value.
  var type: LoweredType { get }

  /// Returns `true` if `self` is equal to `other`.
  func equals(_ other: any ConstantProtocol) -> Bool

}

extension ConstantProtocol {

  public func equals(_ other: any ConstantProtocol) -> Bool {
    self == (other as? Self)
  }

}
