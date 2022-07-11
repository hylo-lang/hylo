/// Invokes `callee` with `operands`.
///
/// `callee` must have a lambda type; the type of the instruction must be the same as output type
/// of the callee. `operands` must contain as many operands as the callee's type.
public struct CallInst: Inst {

  public let type: LoweredType

  /// The passing conventions of the instruction's operands.
  public let conventions: [PassingConvention]

  public let operands: [Operand]

  public init(
    type: LoweredType,
    conventions: [PassingConvention],
    callee: Operand,
    arguments: [Operand]
  ) {
    self.type = type
    self.conventions = conventions
    self.operands = [callee] + arguments
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

  public func check() -> Bool {
    return conventions.count == operands.count
  }

}
