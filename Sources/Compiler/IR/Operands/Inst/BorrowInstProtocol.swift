/// A borrowing instruction.
public protocol BorrowInstProtocol {

  /// The capability being borrowed.
  var capability: RemoteType.Capability { get }

}
