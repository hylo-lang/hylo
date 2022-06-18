/// A protocol describing the API of a VIR constant's associated value.
public protocol ConstantProtocol: Hashable {

  /// The type of the value.
  var type: IRType { get }

}
