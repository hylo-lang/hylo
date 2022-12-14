import Utils

extension Module: CustomStringConvertible, TextOutputStreamable {

  public var description: String {
    var output = ""
    write(to: &output)
    return output
  }

  /// Returns a textual representation of the specified function.
  public func describe(function functionID: Function.ID) -> String {
    var output = ""
    write(function: functionID, to: &output)
    return output
  }

  public func write<Target: TextOutputStream>(to output: inout Target) {
    for i in 0 ..< functions.count {
      if i > 0 {
        output.write("\n\n")
      }
      write(function: i, to: &output)
    }
  }

  /// Writes a textual representation of the specified function into `output`.
  public func write<Target: TextOutputStream>(
    function functionID: Function.ID,
    to output: inout Target
  ) {
    let function = functions[functionID]

    // Generate unique names for all the basic blocks, parameters, and instructions.
    var blockNames: [Block.ID: String] = [:]
    var operandNames: [Operand: String] = [:]
    for i in function.blocks.indices {
      let blockID = Block.ID(function: functionID, address: i.address)
      blockNames[blockID] = "bb\(blockNames.count)"

      for j in 0 ..< function[i.address].inputs.count {
        operandNames[.parameter(block: blockID, index: j)] = "%\(operandNames.count)"
      }
      for j in function[i.address].instructions.indices {
        let instID = InstructionID(function: functionID, block: i.address, address: j.address)
        for k in 0 ..< function[i.address][j.address].types.count {
          operandNames[.result(inst: instID, index: k)] = "%\(operandNames.count)"
        }
      }
    }

    /// Returns a human-readable representation of `operand`.
    func describe(operand: Operand) -> String {
      switch operand {
      case .result, .parameter:
        return operandNames[operand]!
      case .constant(let value):
        return String(describing: value)
      }
    }

    // Dumps the function in the module.
    if let debugName = function.debugName { output.write("// \(debugName)\n") }
    output.write("@lowered fun \(function.name)(")
    output.write(
      function.inputs.lazy
        .map({ (c, t) in "\(c) \(t)" })
        .joined(separator: ", "))
    output.write(") -> \(function.output) {\n")

    for i in function.blocks.indices {
      let blockID = Block.ID(function: functionID, address: i.address)
      let block = function[i.address]

      output.write(blockNames[blockID]!)
      output.write("(")
      output.write(
        block.inputs.enumerated().lazy
          .map({ (j, t) in operandNames[.parameter(block: blockID, index: j)]! + " : \(t)" })
          .joined(separator: ", "))
      output.write("):\n")

      for j in block.instructions.indices {
        let instID = InstructionID(function: functionID, block: i.address, address: j.address)

        output.write("  ")
        if !block[j.address].types.isEmpty {
          output.write(
            (0 ..< block[j.address].types.count)
              .map({ k in operandNames[.result(inst: instID, index: k)]! })
              .joined(separator: ", "))
          output.write(" = ")
        }

        switch block.instructions[j.address] {
        case let inst as AllocStackInstruction:
          output.write("alloc_stack \(inst.allocatedType)")

        case let inst as BorrowInstruction:
          output.write("borrow [\(inst.capability)] ")
          output.write(describe(operand: inst.location))
          if !inst.path.isEmpty {
            output.write(", \(inst.path.descriptions())")
          }

        case let inst as BranchInstruction:
          output.write("branch ")
          output.write(blockNames[inst.target]!)

        case let inst as CallInstruction:
          output.write("call [")
          output.write(inst.conventions.descriptions())
          output.write("] ")
          output.write(describe(operand: inst.callee))
          for operand in inst.arguments {
            output.write(", ")
            output.write(describe(operand: operand))
          }

        case let inst as CondBranchInstruction:
          output.write("cond_branch ")
          output.write(describe(operand: inst.condition))
          output.write(", ")
          output.write(blockNames[inst.targetIfTrue]!)
          output.write(", ")
          output.write(blockNames[inst.targetIfFalse]!)

        case let inst as EndBorrowInstruction:
          output.write("end_borrow ")
          output.write(describe(operand: inst.borrow))

        case let inst as DeallocStackInstruction:
          output.write("dealloc_stack ")
          output.write(describe(operand: inst.location))

        case let inst as DeinitInstruction:
          output.write("deinit ")
          output.write(describe(operand: inst.object))

        case let inst as DestructureInstruction:
          output.write("destructure ")
          output.write(describe(operand: inst.object))

        case let inst as LoadInstruction:
          output.write("load ")
          output.write(describe(operand: inst.source))
          if !inst.path.isEmpty {
            output.write(", \(inst.path.descriptions())")
          }

        case let inst as RecordInstruction:
          output.write("record \(inst.objectType)")
          for operand in inst.operands {
            output.write(", ")
            output.write(describe(operand: operand))
          }

        case let inst as ReturnInstruction:
          output.write("return ")
          output.write(describe(operand: inst.value))

        case let inst as StoreInstruction:
          output.write("store ")
          output.write(describe(operand: inst.object))
          output.write(", ")
          output.write(describe(operand: inst.target))

        case is UnrechableInstruction:
          output.write("unreachable")

        default:
          unreachable("unexpected instruction")
        }

        output.write("\n")
      }
    }

    output.write("}")
  }

}
