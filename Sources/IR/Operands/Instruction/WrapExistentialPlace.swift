import FrontEnd
import Utils

/// Creates existential container wrapping the place of a witness.
public struct WrapExistentialPlace: Instruction {

  /// The place wrapped in the existential container.
  public private(set) var witness: Operand

  /// The witness table of the wrapped value.
  public private(set) var table: Operand

  /// The type of the existential container.
  public let interface: IR.`Type`

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(witness: Operand, table: Operand, interface: IR.`Type`, site: SourceRange) {
    self.witness = witness
    self.table = table
    self.interface = interface
    self.site = site
  }

  public var result: IR.`Type`? {
    interface
  }

  public var operands: [Operand] {
    [witness, table]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    switch i {
    case 0: witness = new
    case 1: table = new
    default:
      preconditionFailure()
    }
  }

}

extension WrapExistentialPlace: CustomStringConvertible {

  public var description: String {
    "wrap_existential_place \(witness), \(table) as \(interface)"
  }

}

extension Module {

  /// Creates a `wrap_existential_place` anchored at `site` that creates an existential container of
  /// type `interface` wrapping `witness` and `table`.
  ///
  /// - Parameters:
  ///   - witness: The place of the object wrapped in the container.
  ///   - interface: The type of the container.
  ///   - table: The witness table of the wrapped value. Must be a pointer to a witness table.
  func makeWrapExistentialPlace(
    _ witness: Operand, _ table: Operand, as interface: ExistentialType,
    in f: Function.ID, at site: SourceRange
  ) -> WrapExistentialPlace {
    precondition(self[f].type(of: witness).isPlace)
    return .init(witness: witness, table: table, interface: .place(interface), site: site)
  }

}
