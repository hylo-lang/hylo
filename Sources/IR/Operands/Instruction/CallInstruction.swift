import Core

/// Invokes `callee` with `operands`.
///
/// `callee` must have a lambda type; the type of the instruction must be the same as output type
/// of the callee. `operands` must contain as many operands as the callee's type.
public struct CallInstruction: Instruction {

  /// The type if the return value.
  public let returnType: LoweredType

  /// The passing conventions of the instruction's operands.
  public let conventions: [AccessEffect]

  /// The arguments of the call.
  public let operands: [Operand]

  public let site: SourceRange

  /// Creates an instance with the given properties.
  public init(
    returnType: LoweredType,
    calleeConvention: AccessEffect,
    callee: Operand,
    argumentConventions: [AccessEffect],
    arguments: [Operand],
    site: SourceRange
  ) {
    self.returnType = returnType
    self.conventions = [calleeConvention] + argumentConventions
    self.operands = [callee] + arguments
    self.site = site
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

  public var isTerminator: Bool { false }

  public func isWellFormed(in module: Module) -> Bool {
    // Instruction result has an object type.
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
          if let instruction = module[id] as? BorrowInstruction {
            if instruction.capability != .let { return false }
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
          if let instruction = module[id] as? BorrowInstruction {
            if instruction.capability != .inout { return false }
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
          if let instruction = module[id] as? BorrowInstruction {
            if instruction.capability != .set { return false }
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

extension Module {

  /// Creates a `call` anchored at `anchor` applies `callee` using convention `calleeConvention` on
  /// `arguments` using `argumentConventions`.
  ///
  /// - Parameters:
  ///   - callee: The function to call. Must have a thin lambda type.
  ///   - arguments: The arguments of the call; one of each input of `callee`'s type.
  func makeCall(
    applying callee: Operand,
    to arguments: [Operand],
    anchoredAt anchor: SourceRange
  ) -> CallInstruction {
    let calleeType = LambdaType(type(of: callee).astType)!
    precondition(calleeType.environment == .void)

    let argumentConventions = calleeType.inputs.map({ ParameterType($0.type)!.access })
    return CallInstruction(
      returnType: .object(program.relations.canonical(calleeType.output)),
      calleeConvention: calleeType.receiverEffect,
      callee: callee,
      argumentConventions: argumentConventions,
      arguments: arguments,
      site: anchor)
  }

}
