import FrontEnd
import Utils

/// Creates existential container wrapping the address of a witness.
public struct WrapExistentialAddr: Instruction {

  /// The address wrapped in the existential container.
  public private(set) var witness: Operand

  /// The witness table of the wrapped value.
  public private(set) var table: Operand

  /// The type of the existential container.
  public let interface: IR.`Type`

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  init(witness: Operand, table: Operand, interface: IR.`Type`, site: SourceRange) {
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

extension WrapExistentialAddr: CustomStringConvertible {

  public var description: String {
    "wrap_existential_addr \(witness), \(table) as \(interface)"
  }

}
