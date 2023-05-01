import Core

/// A return instruction.
public struct ReturnInstruction: Terminator {

  /// The returned value.
  public let object: Operand

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(value: Operand = .void, site: SourceRange) {
    self.object = value
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [object] }

  public var successors: [Block.ID] { [] }

  func replaceSuccessor(_ old: Block.ID, _ new: Block.ID) -> Bool {
    false
  }

}

extension Module {

  /// Creates a `return` anchored at `anchor` that returns `object`.
  ///
  /// - Parameters:
  ///   - object: The return value. Must have an object type.
  func makeReturn(_ object: Operand, anchoredAt anchor: SourceRange) -> ReturnInstruction {
    precondition(type(of: object).isObject)
    return ReturnInstruction(value: object, site: anchor)
  }

}
