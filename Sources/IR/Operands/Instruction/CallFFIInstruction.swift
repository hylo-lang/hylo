import Core
import Utils

/// Invokes `callee`, which is a foreign function interface, with `operands`.
public struct CallFIIInstruction: Instruction {

  /// The type of the return value.
  public let returnType: LoweredType

  /// The name of the foreign function.
  public let callee: String

  /// The arguments of the call.
  public private(set) var operands: [Operand]

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    returnType: LoweredType,
    callee: String,
    arguments: [Operand],
    site: SourceRange
  ) {
    self.returnType = returnType
    self.callee = callee
    self.operands = arguments
    self.site = site
  }

  public var types: [LoweredType] { [returnType] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension CallFIIInstruction: CustomStringConvertible {

  public var description: String {
    let s = "call_ffi \(callee)"
    return operands.isEmpty ? s : "\(s), \(list: operands)"
  }

}

extension Module {

  /// Creates a `call_fii` anchored at `anchor` applies `callee` using convention `calleeConvention` on
  /// `arguments` using `argumentConventions`.
  ///
  /// - Parameters:
  ///   - returnType: The return type of the callee.
  ///   - callee: The name of the foreign function to call
  ///   - arguments: The arguments of the call.
  func makeCallFII(
    returning returnType: LoweredType,
    applying callee: String,
    to arguments: [Operand],
    anchoredAt anchor: SourceRange
  ) -> CallFIIInstruction {
    precondition(returnType.isObject)
    return .init(returnType: returnType, callee: callee, arguments: arguments, site: anchor)
  }

}
