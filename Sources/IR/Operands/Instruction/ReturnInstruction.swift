import Core

/// A return instruction.
public struct ReturnInstruction: Terminator {

  /// The returned value.
  public private(set) var object: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(value: Operand = .void, site: SourceRange) {
    self.object = value
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [object] }

  public var successors: [Block.ID] { [] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    object = new
  }

  func replaceSuccessor(_ old: Block.ID, with new: Block.ID) -> Bool {
    false
  }

}

extension Module {

  /// Creates a `return` anchored at `site` that returns `object`.
  ///
  /// - Parameters:
  ///   - object: The return value. Must have an object type.
  func makeReturn(_ object: Operand, at site: SourceRange) -> ReturnInstruction {
    precondition(type(of: object).isObject)
    return .init(value: object, site: site)
  }

}
