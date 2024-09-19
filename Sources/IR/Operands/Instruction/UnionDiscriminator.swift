import FrontEnd

/// Copy the discriminator of a union container.
public struct UnionDiscriminator: Instruction {

  /// The union container whose discriminator is copied.
  public private(set) var container: Operand

  /// The site of the code corresponding to that instruction.
  public var site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(container: Operand, site: SourceRange) {
    self.container = container
    self.site = site
  }

  public var result: IR.`Type`? {
    .object(BuiltinType.discriminator)
  }

  public var operands: [Operand] {
    [container]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    container = new
  }

}

extension Module {

  /// Creates a `union_discriminator` anchored at `site` that returns the discriminator of the
  /// element stored in `container`.
  func makeUnionDiscriminator(
    _ container: Operand, at site: SourceRange
  ) -> UnionDiscriminator {
    precondition(type(of: container).isAddress)
    return .init(container: container, site: site)
  }

}
