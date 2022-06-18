/// A module lowered to VIR.
public struct Module {

  /// The module's identifier.
  public let id: String

  /// The def-use chains of the values in this module.
  public private(set) var uses: [Any] = []

  /// The functions in the module.
  public private(set) var functions: [Function] = []

  /// A table mapping function declarations to their ID in the module.
  private var loweredFunctions: [NodeID<FunDecl>: FunctionID] = [:]

  public init(id: String) {
    self.id = id
  }

  /// Accesses the basic block identified by `blockID`.
  public subscript(blockID: BlockID) -> Block {
    _read {
      yield functions[blockID.function].blocks[blockID.index]
    }
    _modify {
      yield &functions[blockID.function].blocks[blockID.index]
    }
  }

  /// Accesses the instruction identified by `instID`.
  public subscript(instID: InstID) -> Inst {
    _read {
      yield functions[instID.block.function]
        .blocks[instID.block.index]
        .instructions[instID.index]
    }
  }

  /// Returns the identifier of the VIR function corresponding to `decl`.
  mutating func getOrCreateFunction(from decl: NodeID<FunDecl>, ast: AST) -> FunctionID {
    if let id = loweredFunctions[decl] { return id }

    // Declare a new function in the module.
    let id = functions.count
    functions.append(Function(blocks: []))

    // Update the cache and return the ID of the newly created function.
    loweredFunctions[decl] = id
    return id
  }

}
