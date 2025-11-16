import FrontEnd

/// Stores `object` at the specified location.
public struct Store: Instruction {

  /// The object to store.
  public private(set) var object: Operand

  /// The location at which the object is stored.
  public private(set) var target: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  init(object: Operand, at target: Operand, site: SourceRange) {
    self.object = object
    self.target = target
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
