import Core
import Utils

/// Invokes `callee`, which is a foreign function interface, with `operands`.
public struct CallFFIInstruction: Instruction {

  /// The type of the return value.
  public let returnType: IRType

  /// The name of the foreign function.
  public let callee: String

  /// The arguments of the call.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    returnType: IRType,
    callee: String,
    arguments: [Operand],
    site: SourceRange
  ) {
    self.returnType = returnType
    self.callee = callee
    self.operands = arguments
    self.site = site
  }

  public var types: [IRType] { [returnType] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension CallFFIInstruction: CustomStringConvertible {

  public var description: String {
    let s = "call_ffi \(callee)"
    return operands.isEmpty ? s : "\(s), \(list: operands)"
  }

}

extension Module {

  /// Creates a `call_ffi` anchored at `site` that applies `callee` on `arguments` using to return
  /// and returns a value of `returnType`.
  ///
  /// - Parameters:
  ///   - returnType: The return type of the callee.
  ///   - callee: The name of the foreign function to call
  ///   - arguments: The arguments of the call.
  func makeCallFFI(
    returning returnType: IRType, applying callee: String, to arguments: [Operand],
    at site: SourceRange
  ) -> CallFFIInstruction {
    precondition(returnType.isObject)
    return .init(returnType: returnType, callee: callee, arguments: arguments, site: site)
  }

}
