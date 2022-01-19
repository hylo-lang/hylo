/// A VIL module.
///
/// A VIL module is essentially a collection of VIL functions that have been lowered from a module
/// declaration.
public struct Module {

  /// The module's identifier.
  public let id: String

  /// The functions in the module.
  public var functions: [VILName: VILFun] = [:]

  /// The view witness tables in the module.
  public var viewWitnessTables: [ViewWitnessTable] = []

  public init(id: String) {
    self.id = id
  }

  /// Returns an instruction path referencing the instruction immediately preceeding the one
  /// referenced by the given path
  public func path(before p: InstPath) -> InstPath {
    let b = block(containing: p)
    let i = b.instructions.index(before: p.instIndex)
    return InstPath(funName: p.funName, blockID: p.blockID, instIndex: i)
  }

  /// Returns an instruction path referencing the instruction immediately succeeding the one
  /// referenced by the given path
  public func path(after p: InstPath) -> InstPath {
    let b = block(containing: p)
    let i = b.instructions.index(after: p.instIndex)
    return InstPath(funName: p.funName, blockID: p.blockID, instIndex: i)
  }

  /// Returns the basic block containing the instruction referenced by the given path.
  public func block(containing path: InstPath) -> BasicBlock {
    return functions[path.funName]!.blocks[path.blockID]!
  }

  /// Dereference an instruction path, assuming it refers into this module.
  public subscript(path: InstPath) -> Inst {
    return functions[path.funName]!.blocks[path.blockID]!.instructions[path.instIndex]
  }

}
