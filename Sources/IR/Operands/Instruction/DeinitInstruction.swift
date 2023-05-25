import Core

/// Deinitializes an object.
public struct DeinitInstruction: Instruction {

  /// The object being deinitialized.
  public private(set) var object: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(object: Operand, site: SourceRange) {
    self.object = object
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [object] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    object = new
  }

}

extension Module {

  /// Creates a `deinit` anchored at `site` that deinitializes `object`.
  ///
  /// - Parameters:
  ///   - object: The object to deinitialize. Must be a local register with an object type.
  func makeDeinit(_ object: Operand, at site: SourceRange) -> DeinitInstruction {
    precondition(type(of: object).isObject)
    precondition(object.instruction != nil)
    return .init(object: object, site: site)
  }

}
