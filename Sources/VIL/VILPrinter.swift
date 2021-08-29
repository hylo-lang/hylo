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
    for function in functions.values.sorted(by: { a, b in a.name < b.name }) {
      function.dump(to: &stream)
    }
  }

}

extension Function {

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
      withUnsafeMutablePointer(to: &stream, { ptr in
        var context = PrintContext(stream: ptr)
        for blockID in order {
          context.dump(blockID: blockID, in: self)
        }
      })
      stream.write("}\n\n")
    }
  }

}

fileprivate struct PrintContext<S> where S: TextOutputStream {

  var stream: UnsafeMutablePointer<S>

  var isAtLineStart = true

  var indentation = 0

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

  mutating func dump(blockID: BasicBlock.ID, in function: Function) {
    self << "bb\(function.index(of: blockID)!)("
    self << function.blocks[blockID]!.arguments
    self << "):\n"

    indentation += 1
    for inst in function.blocks[blockID]!.instructions {
      dump(inst: inst)
    }
    indentation -= 1
  }

  mutating func dump(inst: Inst) {
    switch inst {
    case let inst as AllocStackInst:
      let id = makeID(for: inst)
      self << "_\(id) = alloc_stack \(inst.allocatedType)\n"

    case let inst as AllocExistentialInst:
      let id = makeID(for: inst)
      self << "_\(id) = alloc_existential "
      self << inst.container
      self << ", \(inst.witness)\n"

    case let inst as OpenExistentialInst:
      let id = makeID(for: inst)
      self << "_\(id) = open_existential "
      self << inst.container
      self << " as \(inst.type)\n"

    case let inst as OpenExistentialAddrInst:
      let id = makeID(for: inst)
      self << "_\(id) = open_existential_addr "
      self << inst.container
      self << " as \(inst.type)\n"

    case let inst as CopyAddrInst:
      self << "copy_addr "
      self << inst.source
      self << " to "
      self << inst.dest
      self << "\n"

    case let inst as UnsafeCastAddrInst:
      let id = makeID(for: inst)
      self << "_\(id) = unsafe_cast_addr "
      self << inst.source
      self << " as \(inst.type)\n"

    case let inst as CheckedCastAddrInst:
      let id = makeID(for: inst)
      self << "_\(id) = checked_cast_addr "
      self << inst.source
      self << " as \(inst.type)\n"

    case let inst as WitnessMethodInst:
      let id = makeID(for: inst)
      self << "_\(id) = witness_method "
      self << inst.container
      self << ", \(inst.decl.debugID)\n"

    case let inst as ApplyInst:
      let id = makeID(for: inst)
      self << "_\(id) = apply "
      self << describe(inst.fun, withType: false)
      self << "("
      self << inst.args
      self << ")\n"

    case let inst as PartialApplyInst:
      let id = makeID(for: inst)
      self << "_\(id) = partial_apply "
      self << describe(inst.fun, withType: false)
      self << "("
      self << inst.args
      self << ")\n"

    case let inst as ThinToThickInst:
      let id = makeID(for: inst)
      self << "_\(id) = thin_to_thick "
      self << describe(inst.ref, withType: false)
      self << "\n"

    case let inst as RecordInst:
      let id = makeID(for: inst)
      self << "_\(id) = record \(inst.type)\n"

    case let inst as RecordMemberInst:
      let id = makeID(for: inst)
      self << "_\(id) = record_member "
      self << inst.record
      self << ", \(inst.memberDecl.debugID)\n"

    case let inst as RecordMemberAddrInst:
      let id = makeID(for: inst)
      self << "_\(id) = record_member_addr "
      self << inst.record
      self << ", \(inst.memberDecl.debugID)\n"

    case let inst as TupleInst:
      let id = makeID(for: inst)
      self << "_\(id) = tuple \(inst.type) ("
      self << inst.elems
      self << ")\n"

    case let inst as AsyncInst:
      let id = makeID(for: inst)
      self << "_\(id) = async \(inst.fun.name) ("
      self << inst.args
      self << ")\n"

    case let inst as AwaitInst:
      let id = makeID(for: inst)
      self << "_\(id) = await "
      self << inst.value
      self << "\n"

    case let inst as StoreInst:
      self << "store "
      self << inst.rvalue
      self << " to "
      self << inst.lvalue
      self << "\n"

    case let inst as LoadInst:
      let id = makeID(for: inst)
      self << "_\(id) = load "
      self << inst.lvalue
      self << "\n"

    case let inst as EqualAddrInst:
      let id = makeID(for: inst)
      self << "_\(id) = equal_addr "
      self << inst.lhs
      self << ", "
      self << inst.rhs
      self << "\n"

    case let inst as BranchInst:
      self << "branch bb\(inst.dest)("
      self << inst.args
      self << ")\n"

    case let inst as CondBranchInst:
      self << "cond_branch "
      self << inst.cond
      self << " bb\(inst.thenDest)("
      self << inst.thenArgs
      self << ") bb\(inst.elseDest)("
      self << inst.elseArgs
      self << ")\n"

    case let inst as RetInst:
      self << "ret "
      self << inst.value
      self << "\n"

    case is HaltInst:
      self << "halt\n"

    default:
      fatalError()
    }
  }

  mutating func describe(_ value: Value, withType: Bool = true) -> String {
    let description: String

    switch value {
    case is UnitValue, is PoisonValue:
      return String(describing: value)
    case is LiteralValue:
      description = String(describing: value)
    default:
      description = "_" + String(describing: makeID(for: value))
    }

    return withType
      ? "\(value.type) \(description)"
      : description
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
    lhs.write(lhs.describe(rhs))
  }

  static func << (lhs: inout PrintContext, rhs: [Value]) {
    for i in 0 ..< rhs.count {
      lhs << rhs[i]
      if i < rhs.count - 1 {
        lhs.write(", ")
      }
    }
  }

}
