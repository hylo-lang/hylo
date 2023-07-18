import Core

/// Unsafely converts the type of an object.
public struct UnsafeCastInstruction: Instruction {

  /// The object to convert.
  public private(set) var source: Operand

  /// The type of the converted object.
  public let target: AnyType

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(source: Operand, target: AnyType, site: SourceRange) {
    self.source = source
    self.target = target
    self.site = site
  }

  public var types: [IRType] { [.object(target)] }

  public var operands: [Operand] { [source] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    source = new
  }

}

extension Module {

  /// Creates an `unsafe_cast` anchored at `site` that unsafely converts `source` to an object of
  /// type `target`.
  ///
  /// The type of `source` must have a layout compatible with that of `target`, otherwise behavior
  /// is undefined.
  func makeUnsafeCast(
    _ source: Operand, to target: AnyType, at site: SourceRange
  ) -> UnsafeCastInstruction {
    precondition(type(of: source).isObject)
    return .init(source: source, target: target, site: site)
  }

}
