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

  var nextValueID = 0

  var valueIDTable: [ObjectIdentifier: Int] = [:]

  func makeID(for block: BasicBlock) -> Int {
    return block.function.blocks.firstIndex(where: { $0 === block })!
  }

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
    self << "bb\(makeID(for: block))("
    self << block.arguments
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

    case let alloc as AllocExistentialInst:
      let id = makeID(for: alloc)
      self << "_\(id) = alloc_existential "
      self << alloc.container
      self << ", \(alloc.witness)\n"

    case let witnessFun as WitnessFunInst:
      let id = makeID(for: witnessFun)
      self << "_\(id) = witness_fun \(witnessFun.base), \(witnessFun.decl.name)\n"

    case let apply as ApplyInst:
      let id = makeID(for: apply)
      self << "_\(id) = apply "
      self << apply.fun
      self << " to ("
      self << apply.args
      self << ")\n"

    case let member as RecordMemberInst:
      let id = makeID(for: member)
      self << "_\(id) = record_member "
      self << member.record
      self << ", \(member.memberDecl.name)\n"

    case let addr as RecordMemberAddrInst:
      let id = makeID(for: addr)
      self << "_\(id) = record_member_addr "
      self << addr.record
      self << ", \(addr.memberDecl.name)\n"

    case let tuple as TupleInst:
      let id = makeID(for: tuple)
      self << "_\(id) = tuple \(tuple.type) ("
      self << tuple.elems
      self << ")\n"

    case let store as StoreInst:
      self << "store "
      self << store.rvalue
      self << " to "
      self << store.lvalue
      self << "\n"

    case let load as LoadInst:
      let id = makeID(for: load)
      self << "_\(id) = load "
      self << load.lvalue
      self << "\n"

    case let branch as BranchInst:
      self << "branch bb\(makeID(for: branch.dest))("
      self << branch.args
      self << ")\n"

    case let branch as CondBranchInst:
      self << "cond_branch "
      self << branch.cond
      self << " bb\(makeID(for: branch.thenDest))("
      self << branch.thenArgs
      self << ") bb\(makeID(for: branch.elseDest))("
      self << branch.elseArgs
      self << ")\n"

    case let ret as RetInst:
      self << "ret "
      self << ret.value
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

  static func << (lhs: inout PrintContext, rhs: Value) {
    if rhs is LiteralValue {
      lhs.write("\(rhs) : \(rhs.type)")
    } else {
      let id = lhs.makeID(for: rhs)
      lhs.write("_\(id) : \(rhs.type)")
    }
  }

  static func << (lhs: inout PrintContext, rhs: [Value]) {
    for i in 0 ..< rhs.count {
      lhs << rhs[i]
      if i < rhs.count - 1 {
        lhs.write(", ")
      }
    }
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
