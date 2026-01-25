import FrontEnd

/// Converts a built-in pointer value to a place.
///
/// This instruction doesn't extend the lifetime of its operand. The place unsafely refers to the
/// memory referenced by the pointer.
public struct PointerToPlace: Instruction {

  /// The pointer to convert.
  public private(set) var source: Operand

  /// The type of the converted place.
  public let target: RemoteType

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(source: Operand, target: RemoteType, site: SourceRange) {
    self.source = source
    self.target = target
    self.site = site
  }

  public var result: IR.`Type`? {
    .place(target.bareType)
  }

  public var operands: [Operand] {
    [source]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    source = new
  }

}

extension PointerToPlace: CustomStringConvertible {

  public var description: String {
    "pointer_to_place \(source) as \(target)"
  }

}

extension Module {

  /// Creates a `pointer_to_place` anchored at `site` that converts `source`, which is a
  /// built-in pointer value, to a place of type `target`.
  func makePointerToPlace(
    _ source: Operand, to target: RemoteType, at site: SourceRange
  ) -> PointerToPlace {
    precondition(target.access != .yielded)
    return .init(source: source, target: target, site: site)
  }

}
