import FrontEnd

/// Initializes or assigns storage with a value.
public struct Move: Instruction {

  /// The value moved into `target`.
  public private(set) var object: Operand

  /// The location to initialize or assign.
  public private(set) var target: Operand

  /// The conformance of `target`'s type to `Movable` implementing its move operators.
  public let movable: FrontEnd.Conformance

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  init(object: Operand, target: Operand, movable: FrontEnd.Conformance, site: SourceRange) {
    self.object = object
    self.target = target
    self.movable = movable
    self.site = site
  }

  public var operands: [Operand] {
    [object, target]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    switch i {
    case 0: object = new
    case 1: target = new
    default:
      preconditionFailure()
    }
  }

}
