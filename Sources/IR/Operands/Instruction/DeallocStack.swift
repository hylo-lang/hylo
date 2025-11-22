import FrontEnd

/// Deallocates memory previously allocated by `alloc_stack`.
public struct DeallocStack: Instruction {

  /// The location of the memory being deallocated.
  public private(set) var location: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  init(location: Operand, site: SourceRange) {
    self.location = location
    self.site = site
  }

  public var operands: [Operand] {
    [location]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    location = new
  }

}
