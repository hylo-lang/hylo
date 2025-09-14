import FrontEnd

/// Invokes the built-in function `callee`, passing `operands` as the argument list.
public struct CallBuiltinFunction: Instruction {

  /// The function to be invoked.
  public let callee: BuiltinFunction

  /// The arguments to the invocation, in order.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// An instance with the given properties.
  fileprivate init(applying s: BuiltinFunction, to operands: [Operand], site: SourceRange) {
    self.callee = s
    self.operands = operands
    self.site = site
  }

  public var result: IR.`Type`? {
    .object(callee.output)
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension CallBuiltinFunction: CustomStringConvertible {

  public var description: String {
    operands.isEmpty ? "\(callee)" : "\(callee) \(list: operands)"
  }

}

extension Function {

  /// Creates an instruction anchored at `site` that applies `s` to `operands`.
  ///
  /// - Parameters:
  ///   - s: A built-in function.
  ///   - operands: A collection of built-in objects.
  func makeCallBuiltin(
    applying s: BuiltinFunction, to operands: [Operand], at site: SourceRange
  ) -> CallBuiltinFunction {
    precondition(
      operands.allSatisfy { (o) in
        let t = type(of: o)
        return t.isObject && (t.ast.base is BuiltinType)
      })

    return .init(applying: s, to: operands, site: site)
  }

  /// Creates an instruction anchored at `site` that applies `s` to `operands`, inserting it at `p`.
  ///
  /// - Parameters:
  ///   - s: A built-in function.
  ///   - operands: A collection of built-in objects.
  mutating func makeCallBuiltin(
    applying s: BuiltinFunction, to operands: [Operand], at site: SourceRange,
    insertingAt p: InsertionPoint
  ) -> InstructionID {
    insert(makeCallBuiltin(applying: s, to: operands, at: site), at: p)
  }

}
