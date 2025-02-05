import FrontEnd

/// The type and conformances of a value acting as the witness of an existential container.
public struct WitnessTable: Constant, Hashable, Sendable {

  /// The scope in which this table has been created.
  public let scope: AnyScopeID

  /// The type of the witness described by this table.
  public let witness: AnyType

  /// The conformances described by this table.
  public let conformances: Set<IR.Conformance>

  /// Creates an instance describing `witness` and its `conformances` exposed to `scope`.
  ///
  /// - Requires: `witness` is canonical.
  public init(
    for witness: AnyType, conformingTo conformances: Set<IR.Conformance>, in scope: AnyScopeID
  ) {
    self.scope = scope
    self.witness = witness
    self.conformances = conformances
  }

  /// The Hylo IR type of this instance.
  public var type: IR.`Type` { .object(WitnessTableType()) }

}

extension WitnessTable: CustomStringConvertible {

  public var description: String {
    "WitnessTable(\(witness): \(list: conformances.map(\.concept)))"
  }

}
