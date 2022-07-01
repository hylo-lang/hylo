/// A protocol describing the API of a Val IR constant's associated value.
public protocol ConstantProtocol {

  /// The type of the value.
  var type: IRType { get }

}
