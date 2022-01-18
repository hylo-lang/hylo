import AST
import Basic

extension Module {

  /// Dumps a textual representation of this module.
  public func dump() {
    var stream = StandardOutput()
    dump(to: &stream)
  }

  /// Dumps a textual representation of this module to the given output stream.
  public func dump<S>(to stream: inout S) where S: TextOutputStream {
    stream.write("// module \(id)\n")

    // Dump the functions in the module.
    functions
      .sorted(by: { (a, b) -> Bool in
        return !a.value.blocks.isEmpty && !b.value.blocks.isEmpty
          ? a.key < b.key
          : !a.value.hasEntry
      })
      .forEach({ (name, _) in dump(function: name, to: &stream) })
  }

  /// Dumps a textual representation of the specified function.
  public func dump<S>(function funName: String, to stream: inout S) where S: TextOutputStream {
    let fun = functions[funName] ?< fatalError("function does not exist")

    // Dump the function's prologue.
    if let debugName = fun.debugName {
      stream.write("// \(debugName)\n")
    }
    stream.write("vilfun \(funName): \(fun.type)")

    // Dump the function's body, if any.
    guard fun.hasEntry else {
      stream.write("\n\n")
      return
    }

    stream.write(" {\n")
    withUnsafePointer(to: self, { ptr in
      var printer = PrinterContext<S>(module: ptr)
      for block in fun.blocks {
        printer.dump(block: block, to: &stream)
      }
    })
    stream.write("}\n\n")
  }

}

/// The state of a module printer.
public struct PrinterContext<S> where S: TextOutputStream {

  /// A pointer to the module being printed.
  private let module: UnsafePointer<Module>

  /// The current indentation level.
  private var indentation = 0

  /// The printer is about to write at the start of a new line.
  private var isAtLineStart = true

  /// A table mapping basic block indices to unique numeric identifiers.
  private var blockIDs: [BasicBlockIndex: Int] = [:]

  /// A table mapping operands to unique numeric identifiers.
  private var operandIDs: [Operand: Int] = [:]

  fileprivate init(module: UnsafePointer<Module>) {
    self.module = module
  }

  mutating func numericID(of block: BasicBlockIndex) -> Int {
    if let id = blockIDs[block] {
      return id
    } else {
      blockIDs[block] = blockIDs.count
      return blockIDs[block]!
    }
  }

  mutating func numericID(of operand: Operand) -> Int {
    if let id = operandIDs[operand] {
      return id
    } else {
      operandIDs[operand] = operandIDs.count
      return operandIDs[operand]!
    }
  }

  /// Dumps a textual representation of the specified block.
  mutating func dump(block blockIndex: BasicBlockIndex, to stream: inout S) {
    let block = module.pointee.blocks[blockIndex]

    write("bb\(numericID(of: blockIndex))", to: &stream)
    let params = block.params
      .map({ arg -> String in
        return "%\(numericID(of: Operand(arg))): \(arg.type)"
      })
      .joined(separator: ", ")
    write("(\(params)):\n", to: &stream)

    indentation += 1
    for instIndex in block.instructions {
      dump(inst: instIndex, to: &stream)
    }
    indentation -= 1
  }

  /// Dumps a textual representation of the specified instruction.
  mutating func dump(inst instIndex: InstIndex, to stream: inout S) {
    let inst = module.pointee.instructions[instIndex]
    if inst is Value {
      write("%\(numericID(of: Operand(instIndex))) = ", to: &stream)
    }
    inst.dump(to: &stream, with: &self)
  }

  /// Returns the textual representation of and operand.
  mutating func describe(_ operand: Operand, withType: Bool = true) -> String {
    let description: String
    let type: VILType

    if let index = operand.inst {
      description = "%\(numericID(of: operand))"
      type = (module.pointee.instructions[index] as! Value).type
    } else if let value = operand.argument {
      description = "%\(numericID(of: operand))"
      type = value.type
    } else if let value = operand.constant {
      description = String(describing: value)
      type = value.type
    } else {
      fatalError("unreachable")
    }

    if withType {
      return "\(description) : \(type)"
    } else {
      return description
    }
  }

  /// Writes the specified strling to specified stream.
  mutating func write(_ string: String, to stream: inout S) {
    guard !string.isEmpty else { return }

    if indentation == 0 {
      stream.write(string)
    } else {
      let lines = string.split(separator: "\n", omittingEmptySubsequences: false)
      if isAtLineStart {
        stream.write(String(repeating: "  ", count: indentation))
      }
      stream.write(String(lines[0]))

      for line in lines[1...] {
        stream.write("\n")
        if !line.isEmpty {
          stream.write(String(repeating: "  ", count: indentation))
          stream.write(String(line))
        }
      }

      if string.last!.isNewline {
        isAtLineStart = true
      } else {
        isAtLineStart = false
      }
    }
  }

}
