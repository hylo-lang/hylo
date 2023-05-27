import Core
import Utils

/// Invokes `callee` with `operands`.
///
/// `callee` must have a lambda type; the type of the instruction must be the same as output type
/// of the callee. `operands` must contain as many operands as the callee's type.
public struct CallInstruction: Instruction {

  /// The type if the return value.
  public let returnType: LoweredType

  /// The callee and arguments of the call.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    returnType: LoweredType,
    callee: Operand,
    arguments: [Operand],
    site: SourceRange
  ) {
    self.returnType = returnType
    self.operands = [callee] + arguments
    self.site = site
  }

  /// The callee.
  public var callee: Operand { operands[0] }

  /// The arguments of the call.
  public var arguments: ArraySlice<Operand> { operands[1...] }

  /// The types of the instruction's results.
  public var types: [LoweredType] { [returnType] }

  /// `true` iff the instruction denotes a call to a generic function.
  public var isGeneric: Bool {
    if let f = callee.constant as? FunctionReference {
      return !f.arguments.isEmpty
    } else {
      return false
    }
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension Module {

  /// Creates a `call` anchored at `site` that applies `callee` on `arguments`.
  ///
  /// - Parameters:
  ///   - callee: The function to call.
  ///   - arguments: The arguments of the call; one for each input of `callee`'s type.
  func makeCall(
    applying callee: Operand, to arguments: [Operand],
    at site: SourceRange
  ) -> CallInstruction {
    let calleeType = LambdaType(type(of: callee).ast)!.strippingEnvironment
    precondition(calleeType.inputs.count == arguments.count)
    return .init(
      returnType: .object(program.relations.canonical(calleeType.output)),
      callee: callee,
      arguments: arguments,
      site: site)
  }

}
