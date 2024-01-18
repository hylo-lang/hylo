import Core
import Foundation

/// Creates a pointer to a constant string allocated statically.
public struct ConstantString: Instruction {

  /// The value of the string, encoded in UTF-8.
  public let value: Data

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(value: Data, site: SourceRange) {
    self.value = value
    self.site = site
  }

  public var result: IR.`Type`? {
    .object(BuiltinType.ptr)
  }

  public var operands: [Operand] { [] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    preconditionFailure()
  }

}

extension ConstantString: CustomStringConvertible {

  public var description: String {
    "constant_string \(String(data: value, encoding: .utf8)!)"
  }

}

extension Module {

  /// Creates a `constant_string` anchored at `site` that returns a pointer to a statically
  /// allocated string with given `value`, encoded in UTF8.
  func makeConstantString(utf8 value: Data, at site: SourceRange) -> ConstantString {
    .init(value: value, site: site)
  }

}
