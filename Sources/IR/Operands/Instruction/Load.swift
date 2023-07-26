import Core

/// A load instruction.
public struct Load: Instruction {

  /// The type of the object being loaded.
  public let objectType: IR.`Type`

  /// The location of the object is being loaded.
  public private(set) var source: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(objectType: IR.`Type`, from source: Operand, site: SourceRange) {
    self.objectType = objectType
    self.source = source
    self.site = site
  }

  public var types: [IR.`Type`] { [objectType] }

  public var operands: [Operand] { [source] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    source = new
  }

}

extension Module {

  /// Creates a `load` anchored at `site` that loads the object at `source`.
  ///
  /// - Parameters:
  ///   - source: The location from which the object is loaded. Must have an address type.
  func makeLoad(_ source: Operand, at site: SourceRange) -> Load {
    let t = type(of: source)
    precondition(t.isAddress)
    return .init(objectType: .object(t.ast), from: source, site: site)
  }

}
