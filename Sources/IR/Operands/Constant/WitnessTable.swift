import Core
import Foundation

/// A constant buffer of bytes in Val IR.
public struct WitnessTable: ConstantProtocol, Hashable {

  /// The identifier of this table.
  public let id: UUID

  /// The type of the witness described by this table.
  public let witness: AnyType

  /// Creates an instance describing `witness`.
  ///
  /// - Requires: `witness` is canonical.
  public init(describing witness: AnyType) {
    self.id = UUID()
    self.witness = witness
  }

  /// The Val IR type of this instance.
  public var type: LoweredType { .object(WitnessTableType(self)) }

}

extension WitnessTable: CustomStringConvertible {

  public var description: String {
    "WitnessTable(\(id))"
  }

}
