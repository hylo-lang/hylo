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

  /// Dereference an instruction path, assuming it refers into this module.
  public subscript(path: InstPath) -> Inst {
    return functions[path.funName]!.blocks[path.blockID]!.instructions[path.instIndex]
  }

}
