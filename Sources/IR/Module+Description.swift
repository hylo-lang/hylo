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
        let instID = InstructionID(functionID, i.address, j.address)
        for k in 0 ..< function[i.address][j.address].types.count {
          operandNames[.result(instruction: instID, index: k)] = "%\(operandNames.count)"
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
        let instID = InstructionID(functionID, i.address, j.address)

        output.write("  ")
        if !block[j.address].types.isEmpty {
          output.write(
            (0 ..< block[j.address].types.count)
              .map({ k in operandNames[.result(instruction: instID, index: k)]! })
              .joined(separator: ", "))
          output.write(" = ")
        }

        switch block.instructions[j.address] {
        case let instruction as AllocStackInstruction:
          output.write("alloc_stack \(instruction.allocatedType)")

        case let instruction as BorrowInstruction:
          output.write("borrow [\(instruction.capability)] ")
          output.write(describe(operand: instruction.location))

        case let instruction as BranchInstruction:
          output.write("branch ")
          output.write(blockNames[instruction.target]!)

        case let instruction as CallInstruction:
          output.write("call ")
          output.write(describe(operand: instruction.callee))
          for operand in instruction.arguments {
            output.write(", ")
            output.write(describe(operand: operand))
          }

        case let instruction as CondBranchInstruction:
          output.write("cond_branch ")
          output.write(describe(operand: instruction.condition))
          output.write(", ")
          output.write(blockNames[instruction.targetIfTrue]!)
          output.write(", ")
          output.write(blockNames[instruction.targetIfFalse]!)

        case let instruction as EndBorrowInstruction:
          output.write("end_borrow ")
          output.write(describe(operand: instruction.borrow))

        case let instruction as DeallocStackInstruction:
          output.write("dealloc_stack ")
          output.write(describe(operand: instruction.location))

        case let instruction as DeinitInstruction:
          output.write("deinit ")
          output.write(describe(operand: instruction.object))

        case let instruction as DestructureInstruction:
          output.write("destructure ")
          output.write(describe(operand: instruction.whole))

        case let instruction as ElementAddrInstruction:
          output.write("element_addr ")
          output.write(describe(operand: instruction.base))
          output.write(", \(list: instruction.elementPath, joinedBy: ", ")")

        case let instruction as LoadInstruction:
          output.write("load ")
          output.write(describe(operand: instruction.source))

        case let instruction as LLVMInstruction:
          output.write("\(instruction.function.llvmInstruction)")
          if instruction.function.genericParameters.isEmpty {
            output.write(" ")
          } else {
            output.write("_\(list: instruction.function.genericParameters, joinedBy: "_") ")
          }
          for operand in instruction.operands {
            output.write(", ")
            output.write(describe(operand: operand))
          }

        case let instruction as RecordInstruction:
          output.write("record \(instruction.objectType)")
          for operand in instruction.operands {
            output.write(", ")
            output.write(describe(operand: operand))
          }

        case let instruction as ReturnInstruction:
          output.write("return ")
          output.write(describe(operand: instruction.object))

        case let instruction as StoreInstruction:
          output.write("store ")
          output.write(describe(operand: instruction.object))
          output.write(", ")
          output.write(describe(operand: instruction.target))

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
