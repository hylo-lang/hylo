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
    for function in functions.values {
      function.dump(to: &stream)
    }
  }

}

extension Function {

  /// Dumps a textual representation of the function to the given output stream.
  public func dump<S>(to stream: inout S) where S: TextOutputStream {
    stream.write("vilfun \(name) : \(type) {\n")

    withUnsafeMutablePointer(to: &stream, { ptr in
      var context = PrintContext(stream: ptr)
      for (i, argument) in arguments.enumerated() {
        context.valueIDTable[ObjectIdentifier(argument)] = i
      }
      context.nextValueID = arguments.count
      for block in blocks {
        context.dump(block: block)
      }
    })

    stream.write("}\n")
  }

}

fileprivate struct PrintContext<S> where S: TextOutputStream {

  var stream: UnsafeMutablePointer<S>

  var isAtLineStart = true

  var indentation = 0

  var nextBlockID = 0

  var nextValueID = 0

  var valueIDTable: [ObjectIdentifier: Int] = [:]

  mutating func makeID(for value: Value) -> Int {
    if let id = valueIDTable[ObjectIdentifier(value)] {
      return id
    }

    let valueID = nextValueID
    nextValueID += 1
    valueIDTable[ObjectIdentifier(value)] = valueID
    return valueID
  }

  mutating func dump(block: BasicBlock) {
    let id = nextBlockID
    nextBlockID += 1
    self << "bb\(id)("
    for argument in block.arguments {
      self << IDAndType(id: makeID(for: argument), type: argument.type)
    }
    self << "):\n"

    indentation += 1
    for inst in block.instructions {
      dump(inst: inst)
    }
    indentation -= 1
  }

  mutating func dump(inst: Inst) {
    switch inst {
    case let alloc as AllocStackInst:
      let id = makeID(for: alloc)
      self << "_\(id) = alloc_stack \(alloc.allocatedType)\n"

    case let tuple as TupleInst:
      let id = makeID(for: tuple)
      self << "_\(id) = tuple \(tuple.type) ("
      for elem in tuple.elems {
        self << IDAndType(id: makeID(for: elem), type: elem.type)
      }
      self << ")\n"

    case let store as StoreInst:
      self << "store "
      self << IDAndType(id: makeID(for: store.rvalue), type: store.rvalue.type)
      self << " to "
      self << IDAndType(id: makeID(for: store.lvalue), type: store.lvalue.type)
      self << "\n"

    case let load as LoadInst:
      let id = makeID(for: load)
      self << "_\(id) = load "
      self << IDAndType(id: makeID(for: load.lvalue), type: load.lvalue.type)
      self << "\n"

    default:
      fatalError()
    }
  }

  mutating func write(_ string: String) {
    guard !string.isEmpty else { return }

    if indentation == 0 {
      stream.pointee.write(string)
    } else {
      let lines = string.split(separator: "\n", omittingEmptySubsequences: false)
      if isAtLineStart {
        stream.pointee.write(String(repeating: "  ", count: indentation))
      }
      stream.pointee.write(String(lines[0]))

      for line in lines[1...] {
        stream.pointee.write("\n")
        if !line.isEmpty {
          stream.pointee.write(String(repeating: "  ", count: indentation))
          stream.pointee.write(String(line))
        }
      }

      if string.last!.isNewline {
//        stream.pointee.write("\n")
        isAtLineStart = true
      } else {
        isAtLineStart = false
      }
    }
  }

  static func << (lhs: inout PrintContext, rhs: String) {
    lhs.write(rhs)
  }

  static func << (lhs: inout PrintContext, rhs: IDAndType) {
    lhs.write("_\(rhs.id) : \(rhs.type)")
  }

  struct IDAndType {

    let id: Int

    let type: VILType

  }

}

//fileprivate struct IndentedStream<Base>: TextOutputStream where Base: TextOutputStream {
//
//  var base: UnsafeMutablePointer<Base>
//
//  var isAtLineStart: Bool = true
//
//  mutating func write(_ string: String) {
//    guard !string.isEmpty else { return }
//
//    let lines = string.split(separator: "\n")
//    if isAtLineStart {
//      base.pointee.write("  ")
//      base.pointee.write(String(lines[0]))
//    }
//
//    for line in lines[1...] {
//      base.pointee.write("\n  ")
//      base.pointee.write(String(line))
//    }
//
//    if string.last!.isNewline {
//      base.pointee.write("\n")
//      isAtLineStart = true
//    } else {
//      isAtLineStart = false
//    }
//  }
//
//}
