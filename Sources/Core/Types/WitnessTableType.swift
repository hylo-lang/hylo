import Foundation

/// The type of a witness table.
public struct WitnessTableType: TypeProtocol {

  /// The unique identifier of this type.
  public let id: UUID

  /// Creates an instance.
  public init() {
    self.id = UUID()
  }

  public var flags: TypeFlags { .isCanonical }

}

extension WitnessTableType: CustomStringConvertible {

  public var description: String { "WitnessTable(\(id)" }

}
