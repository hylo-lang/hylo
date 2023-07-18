import Core

/// The type and conformances of a value acting as the witness of an existential container.
public struct WitnessTable: Constant, Hashable {

  /// The type of the witness described by this table.
  public let witness: AnyType

  /// The conformances described by this table.
  public let conformances: Set<Conformance>

  /// Creates an instance describing `witness` and its `conformances`.
  ///
  /// - Requires: `witness` is canonical.
  public init(for witness: AnyType, conformingTo conformances: Set<Conformance>) {
    self.witness = witness
    self.conformances = conformances
  }

  /// The Val IR type of this instance.
  public var type: LoweredType { .object(WitnessTableType()) }

}

extension WitnessTable: CustomStringConvertible {

  public var description: String {
    "WitnessTable(\(witness): \(list: conformances.map(\.concept)))"
  }

}
