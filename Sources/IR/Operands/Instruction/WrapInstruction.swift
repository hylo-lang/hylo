import Core

/// Creates existential container wrapping a witness.
public struct WrapInstruction: Instruction {

  /// The address wrapped in the existential container.
  public private(set) var witness: Operand

  /// The witness table of the wrapped value.
  public private(set) var table: Operand

  /// The type of the existential container.
  public let interface: LoweredType

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(witness: Operand, table: Operand, interface: LoweredType, site: SourceRange) {
    self.witness = witness
    self.table = table
    self.interface = interface
    self.site = site
  }

  public var types: [LoweredType] { [interface] }

  public var operands: [Operand] { [witness, table] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    switch i {
    case 0: witness = new
    case 1: table = new
    default:
      preconditionFailure()
    }
  }

}

extension WrapInstruction: CustomStringConvertible {

  public var description: String {
    "wrap \(witness), \(table) as \(interface)"
  }

}

extension Module {

  /// Creates a `wrap` anchored at `anchor` that creates an existential container of type
  /// `interface` wrapping `witness` and `table`.
  ///
  /// - Parameters:
  ///   - witness: The object wrapped in the container.
  ///   - interface: The type of the container.
  ///   - table: The witness table of the wrapped value. Must be a pointer to a witness table.
  func makeWrap(
    _ witness: Operand,
    _ table: Operand,
    as interface: ExistentialType,
    anchoredAt anchor: SourceRange
  ) -> WrapInstruction {
    precondition(type(of: witness).isAddress)
    return .init(witness: witness, table: table, interface: .object(interface), site: anchor)
  }

}
