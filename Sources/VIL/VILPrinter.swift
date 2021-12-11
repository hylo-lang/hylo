import AST
import Basic

extension Module {

  /// Dumps a textual representation of the module.
  public func dump() {
    var stream = StandardOutput()
    dump(to: &stream)
  }

  /// Dumps a textual representation of the module to the given output stream.
  public func dump<S>(to stream: inout S) where S: TextOutputStream {
    stream.write("// module \(id)\n")

    // Dump the functions in the module.
    functions.values
      .sorted(by: { (a, b) -> Bool in
        return !a.blocks.isEmpty && !b.blocks.isEmpty
          ? a.name < b.name
          : a.entryID == nil
      })
      .forEach({ $0.dump(to: &stream) })
  }

}

extension VILFun {

  /// Dumps a textual representation of the function.
  public func dump() {
    var stream = StandardOutput()
    dump(to: &stream)
  }

  /// Dumps a textual representation of the function to the given output stream.
  public func dump<S>(to stream: inout S) where S: TextOutputStream {
    // Dump the function's prologue.
    if let debugName = self.debugName {
      stream.write("// \(debugName)\n")
    }
    stream.write("vilfun \(name): \(type)")

    // Dump the function's body, if any.
    if blocks.isEmpty {
      stream.write("\n\n")
    } else {
      stream.write(" {\n")
      if let blockID = entryID {
        dump(blockID: blockID, to: &stream)
      }
      for blockID in blocks.keys where blockID != entryID {
        dump(blockID: blockID, to: &stream)
      }
      stream.write("}\n\n")
    }
  }

  private func dump<S>(blockID: BasicBlock.ID, to stream: inout S)  where S: TextOutputStream {
    let block = blocks[blockID]!
    var printer = PrinterContext()

    printer.write("bb\(blockID)", to: &stream)
    let params = block.params
      .map({ printer.describe($0, withType: true) })
      .joined(separator: ", ")
    printer.write("(\(params)):\n", to: &stream)

    printer.indentation += 1
     for inst in block.instructions {
       inst.dump(to: &stream, with: &printer)
     }
    printer.indentation -= 1
   }

}

public struct PrinterContext {

  /// The current indentation level.
  fileprivate var indentation = 0

  /// The printer is about to write at the start of a new line.
  private var isAtLineStart = true

  /// The next auto-generated value identifier.
  private var nextValueID = 0

  /// A table of unique identifiers.
  private var valueIDs: ReferenceTable<Value, Int> = [:]

  /// Returns a unique identifier for the given VIL value in this context.
  mutating func uniqueID(of value: Value) -> Int {
    if let id = valueIDs[value] {
      return id
    } else {
      defer { nextValueID += 1 }
      valueIDs[value] = nextValueID
      return nextValueID
    }
  }

  /// Produces a representation of the specified value.
  mutating func describe(_ value: Value, withType: Bool = true) -> String {
    let description: String
    switch value {
    case is UnitValue, is PoisonValue, is LiteralValue:
      description = "\(value)"
    default:
      description = "%\(uniqueID(of: value))"
    }

    return withType
      ? "\(value.type) \(description)"
      : description
  }

  /// Writes the specified string to specified stream.
  mutating func write<S>(_ string: String, to stream: inout S) where S: TextOutputStream {
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
