import Utils

extension Module: CustomStringConvertible {

  public var description: String {
    (0 ..< functions.count).reduce(into: "", { (output, i) in
      if i > 0 { output.write("\n\n") }
      output.write(describe(function: i))
    })
  }

  /// Returns a human-readable representation of the specified function.
  public func describe(function functionID: Function.ID) -> String {
    let function = functions[functionID]

    // Generate unique names for all the basic blocks, parameters, and instructions.
    var blockNames: [Block.ID: String] = [:]
    var operandNames: [Operand: String] = [:]
    for i in function.blocks.indices {
      let blockID = Block.ID(function: functionID, address: i.address)
      blockNames[blockID] = "bb\(blockNames.count)"

      for j in 0 ..< function.blocks[i.address].inputs.count {
        operandNames[.parameter(block: blockID, index: j)] = "%\(operandNames.count)"
      }
      for j in function.blocks[i.address].instructions.indices {
        let instID = InstID(function: functionID, block: i.address, address: j.address)
        operandNames[.inst(instID)] = "%\(operandNames.count)"
      }
    }

    /// Returns a human-readable representation of `operand`.
    func describe(operand: Operand) -> String {
      switch operand {
      case .inst, .parameter:
        return operandNames[operand]!
      case .constant(let value):
        return String(describing: value)
      }
    }

    // Dumps the function in the module.
    var output = ""
    if let debugName = function.debugName { output.write("// \(debugName)\n") }
    output.write("@lowered fun \(function.name) {\n")

    for i in function.blocks.indices {
      let blockID = Block.ID(function: functionID, address: i.address)
      let block = function.blocks[i]

      output.write(blockNames[blockID]!)
      output.write("(")
      output.write(block.inputs.enumerated().lazy
        .map({ (j, t) in operandNames[.parameter(block: blockID, index: j)]! + " : \(t)" })
        .joined(separator: ", "))
      output.write("):\n")

      for j in block.instructions.indices {
        let instID = InstID(function: functionID, block: i.address, address: j.address)
        output.write("  ")
        output.write(operandNames[.inst(instID)]!)
        output.write(" = ")

        switch block.instructions[j.address] {
        case let inst as AllocStackInst:
          output.write("alloc_stack \(inst.objectType)")

        case let inst as BorrowInst:
          output.write("borrow [\(inst.capability)] ")
          output.write(describe(operand: inst.value))
          output.write(", \(inst.path.descriptions())")

        case let inst as BranchInst:
          output.write("branch ")
          output.write(blockNames[inst.target]!)

        case let inst as CallInst:
          output.write("call [")
          output.write(inst.conventions.descriptions())
          output.write("] ")
          output.write(describe(operand: inst.callee))
          for operand in inst.arguments {
            output.write(", ")
            output.write(describe(operand: operand))
          }

        case let inst as CondBranchInst:
          output.write("cond_branch ")
          output.write(describe(operand: inst.condition))
          output.write(", ")
          output.write(blockNames[inst.targetIfTrue]!)
          output.write(", ")
          output.write(blockNames[inst.targetIfFalse]!)

        case let inst as LoadInst:
          output.write("load ")
          output.write(describe(operand: inst.source))

        case let inst as RecordInst:
          output.write("record \(inst.objectType)")
          for operand in inst.operands {
            output.write(", ")
            output.write(describe(operand: operand))
          }

        case let inst as ReturnInst:
          output.write("return ")
          output.write(describe(operand: inst.value))

        case let inst as StoreInst:
          output.write("store ")
          output.write(describe(operand: inst.object))
          output.write(", ")
          output.write(describe(operand: inst.target))

        case let inst as TakeMemberInst:
          output.write("take_member ")
          output.write(describe(operand: inst.value))
          output.write(", \(inst.path.descriptions())")

        default:
          unreachable("unexpected instruction")
        }

        output.write("\n")
      }
    }

    output.write("}")

    return output
  }

}
