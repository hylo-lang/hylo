import Core
import Foundation

/// The type of a witness table.
public struct WitnessTableType: TypeProtocol {

  /// The unique identifier of table that inhabits this type.
  public let id: UUID

  /// Creates the type `t`.
  public init(_ t: WitnessTable) {
    self.id = t.id
  }

  public var flags: TypeFlags { .isCanonical }

}

extension WitnessTableType: CustomStringConvertible {

  public var description: String { "WitnessTable(\(id)" }

}
