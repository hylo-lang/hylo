import Core
import Foundation

/// A constant buffer of bytes in Val IR.
public struct WitnessTable: Constant {

  /// The identifier of this table.
  public let id: UUID

  /// The type of the witness described by this table.
  public let witness: AnyType

  /// The conformances described by this table.
  public let conformances: [Conformance]

  /// Creates an instance describing `witness`.
  ///
  /// - Requires: `witness` is canonical.
  public init(for witness: AnyType, conformingTo conformances: [Conformance]) {
    self.id = UUID()
    self.witness = witness
    self.conformances = conformances
  }

  /// The Val IR type of this instance.
  public var type: LoweredType { .object(WitnessTableType(self)) }

}

extension WitnessTable: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    l.id == r.id
  }

}

extension WitnessTable: Hashable {

  public func hash(into hasher: inout Hasher) {
    id.hash(into: &hasher)
  }

}

extension WitnessTable: CustomStringConvertible {

  public var description: String {
    "WitnessTable(\(witness): \(list: conformances.map(\.concept)))"
  }

}
