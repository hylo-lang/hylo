import Utils

/// A module lowered to Val IR.
public struct Module {

  /// The module's declaration.
  public let decl: NodeID<ModuleDecl>

  /// The module's name.
  public let name: String

  /// The def-use chains of the values in this module.
  public private(set) var uses: [Operand: [Use]] = [:]

  /// The functions in the module.
  public private(set) var functions: [Function] = []

  /// A table mapping function declarations to their ID in the module.
  private var loweredFunctions: [NodeID<FunDecl>: Function.ID] = [:]

  public init(decl: NodeID<ModuleDecl>, id: String) {
    self.decl = decl
    self.name = id
  }

  /// Returns the type of `operand`.
  public func type(of operand: Operand) -> LoweredType {
    switch operand {
    case .inst(let id):
      return functions[id.function][id.block][id.index].type

    case .parameter(let block, let index):
      return functions[block.function][block.index].inputs[index]

    case .constant(let constant):
      return constant.type
    }
  }

  /// Returns the identifier of the Val IR function corresponding to `declID`.
  mutating func getOrCreateFunction(
    from declID: NodeID<FunDecl>,
    ast: AST,
    withScopeHierarchy scopeHierarchy: ScopeHierarchy,
    withDeclTypes declTypes: DeclMap<Type>
  ) -> Function.ID {
    if let id = loweredFunctions[declID] { return id }

    // Determine the type of the function.
    var inputs: [LoweredType] = []
    let output: LoweredType

    switch declTypes[declID] {
    case .lambda(let declType):
      inputs.reserveCapacity(declType.captures!.count + declType.inputs.count)
      for capture in ast[declID].implicitParameterDecls {
        inputs.append(LoweredType(lowering: declTypes[capture.decl]!))
      }
      for parameter in ast[declID].parameters {
        inputs.append(LoweredType(lowering: declTypes[parameter]!))
      }
      output = LoweredType(lowering: declType.output)

    default:
      unreachable()
    }

    // Declare a new function in the module.
    let id = functions.count
    let locator = DeclLocator(
      identifying: declID,
      in: ast,
      withScopeHierarchy: scopeHierarchy,
      withDeclTypes: declTypes)
    let function = Function(
      name: locator.mangled,
      debugName: ast[declID].identifier?.value,
      inputs: inputs,
      output: output,
      blocks: [])
    functions.append(function)

    // Update the cache and return the ID of the newly created function.
    loweredFunctions[declID] = id
    return id
  }

  /// Creates a basic block at the end of the specified function and returns its identifier.
  @discardableResult
  mutating func createBasicBlock(
    accepting inputs: [LoweredType] = [],
    atEndOf function: Function.ID
  ) -> Block.ID {
    let index = functions[function].blocks.append(Block(inputs: inputs))
    return Block.ID(function: function, index: index)
  }

  /// Inserts `inst` at the specified insertion point.
  @discardableResult
  mutating func insert<I: Inst>(_ inst: I, at ip: InsertionPoint) -> InstID {
    // Inserts the instruction.
    let index: Int
    switch ip.position {
    case .end:
      index = functions[ip.block.function][ip.block.index].instructions.append(inst)
    case .after(let i):
      index = functions[ip.block.function][ip.block.index].instructions.insert(inst, after: i)
    }

    // Generate an instruction identifier.
    let userID = InstID(function: ip.block.function, block: ip.block.index, index: index)

    // Update the use lists of the instruction's operands.
    for i in 0 ..< inst.operands.count {
      uses[inst.operands[i], default: []].append(Use(user: userID, index: i))
    }

    return userID
  }

}

extension Module {

  public typealias FunctionIndex = Int

  public subscript(_ position: FunctionIndex) -> Function {
    _read   { yield functions[position] }
    _modify { yield &functions[position] }
  }

}

extension Module: CustomStringConvertible {

  public var description: String {
    var output = "// module \(name)"

    for functionID in 0 ..< functions.count {
      let function = functions[functionID]

      output.write("\n\n")
      if let debugName = function.debugName {
        output.write("// \(debugName)\n")
      }
      output.write("@lowered fun \(function.name) {\n")

      var printer = IRPrinter()
      for blockIndex in function.blocks.indices {
        let blockID = Block.ID(function: functionID, index: blockIndex)
        let block = function.blocks[blockIndex]

        output.write("\(printer.translate(block: blockID))(")
        output.write(block.inputs.enumerated()
          .map({ (i, input) in "%\(i + printer.instNames.nextDiscriminator): \(input)" })
          .joined(separator: ", "))
        output.write("):\n")

        printer.instNames.nextDiscriminator += block.inputs.count
        for instIndex in block.instructions.indices {
          let instID = InstID(function: functionID, block: blockIndex, index: instIndex)
          let inst = block.instructions[instIndex]
          output.write("  \(printer.translate(inst: instID)) = ")
          inst.dump(into: &output, with: &printer)
          output.write("\n")
        }
      }

      output.write("}")
    }

    return output
  }

}
