import FrontEnd

/// Computes a `source` address advanced by `count` strides of its referred type.
///
/// The stride of a type is the number of bytes from the start of an instance to the start of the
/// next when stored in contiguous memory.
public struct AdvancedByStrides: Instruction {

  /// The address to be advanced.
  public private(set) var base: Operand

  /// The number of strides by which to advance the source value.
  public let offset: Int

  /// The type of the instruction's result.
  public let result: IR.`Type`?

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    source: Operand,
    offset: Int,
    result: IR.`Type`,
    site: SourceRange
  ) {
    self.base = source
    self.offset = offset
    self.result = result
    self.site = site
  }

  public var operands: [Operand] {
    [base]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    base = new
  }

}

extension AdvancedByStrides: CustomStringConvertible {

  public var description: String {
    "\(base) advanced by \(offset) strides"
  }

}

extension Module {

  /// Creates an `advanced by strides` instruction anchored at `site` computing the `source`
  /// address advanced by `n` strides of its referred type.
  func makeAdvanced(
    _ source: Operand, byStrides n: Int, in f: Function.ID, at site: SourceRange
  ) -> AdvancedByStrides {
    guard let b = sourceType(source, in: f) else {
      preconditionFailure("source must be the address of a buffer")
    }

    return .init(source: source, offset: n, result: .place(b.element), site: site)
  }

  /// Returns the AST type of `source` iff it is the address of a buffer.
  private func sourceType(_ source: Operand, in f: Function.ID) -> BufferType? {
    let s = self[f].type(of: source)
    if s.isPlace {
      return BufferType(s.ast)
    } else {
      return nil
    }
  }

}
