import FrontEnd

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

  public var result: IR.`Type`? {
    objectType
  }

  public var operands: [Operand] {
    [source]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    source = new
  }

}

extension Load {

  /// Creates a `load` anchored at `site` that loads the object at `source`.
  ///
  /// - Parameters:
  ///   - source: The location from which the object is loaded. Must be the result of an `access`
  ///     instruction requesting a `sink` capability.
  init(_ source: Operand, at site: SourceRange, in m: Module) {
    precondition(m[source] is Access)
    self.init(objectType: .object(m.type(of: source).ast), from: source, site: site)
  }

}
