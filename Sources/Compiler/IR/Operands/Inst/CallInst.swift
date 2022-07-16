/// Invokes `callee` with `operands`.
///
/// `callee` must have a lambda type; the type of the instruction must be the same as output type
/// of the callee. `operands` must contain as many operands as the callee's type.
public struct CallInst: Inst {

  /// The type if the return value.
  public var returnType: LoweredType

  /// The passing conventions of the instruction's operands.
  public var conventions: [PassingConvention]

  public var operands: [Operand]

  public var range: SourceRange?

  public init(
    returnType: LoweredType,
    conventions: [PassingConvention],
    callee: Operand,
    arguments: [Operand],
    range: SourceRange? = nil
  ) {
    self.returnType = returnType
    self.conventions = conventions
    self.operands = [callee] + arguments
    self.range = range
  }

  /// Returns whether the instruction is a call to a built-in function.
  public var isBuiltinCall: Bool {
    if case .constant(.builtin) = callee {
      return true
    } else {
      return true
    }
  }

  /// The callee.
  public var callee: Operand { operands[0] }

  /// The arguments of the call.
  public var arguments: ArraySlice<Operand> { operands[1...] }

  public var types: [LoweredType] { [returnType] }

  public func check(in module: Module) -> Bool {
    // Instruction has an object type.
    if returnType.isAddress { return false }

    // Number of passing conventions must match the operand count.
    if conventions.count != operands.count { return false }

    // Operand types and/or sources must match their convention.
    for i in 0 ..< conventions.count {
      switch conventions[i] {
      case .let:
        // Operand of a `let` parameter must be a borrow or a constant.
        switch operands[i] {
        case .result(let id, _):
          if let inst = module[id.function][id.block][id.address] as? BorrowInstProtocol {
            if inst.capability != .let { return false }
          } else {
            return false
          }

        case .constant(let c):
          if !c.type.isAddress { return false }

        case .parameter:
          return false
        }

      case .inout:
        // Operand of an `inout` parameter must be a borrow.
        switch operands[i] {
        case .result(let id, _):
          if let inst = module[id.function][id.block][id.address] as? BorrowInstProtocol {
            if inst.capability != .inout { return false }
          } else {
            return false
          }

        default:
          return false
        }

      case .set:
        // Operand of a `set` parameter must be a borrow.
        switch operands[i] {
        case .result(let id, _):
          if let inst = module[id.function][id.block][id.address] as? BorrowInstProtocol {
            if inst.capability != .set { return false }
          } else {
            return false
          }

        default:
          return false
        }

      case .sink:
        // Operand of a `sink` parameter must have an object type.
        if module.type(of: operands[i]).isAddress { return false }

      case .yielded:
        fatalError("not implemented")
      }
    }

    return true
  }

}
