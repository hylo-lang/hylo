import Core

/// Creates existential container wrapping `witness` and exposing `interface`.
public struct WrapAddrInstruction: Instruction {

  /// The address wrapped in the existential container.
  public let witness: Operand

  /// The type of the existential container.
  public let interface: LoweredType

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(witness: Operand, interface: LoweredType, site: SourceRange) {
    self.witness = witness
    self.interface = interface
    self.site = site
  }

  public var types: [LoweredType] { [interface] }

  public var operands: [Operand] { [witness] }

}

extension WrapAddrInstruction: CustomStringConvertible {

  public var description: String {
    "wrap_addr \(witness) as \(interface)"
  }

}

extension Module {

  /// Creates a `wrap` anchored at `anchor` that creates an existential container wrapping `witness`
  /// and presenting `interface`.
  ///
  /// - Parameters:
  ///   - witness: The object wrapped in the container. Its type must be subtype of `interface`.
  ///   - interface: The type of the container.
  func makeWrapAddr(
    _ witness: Operand,
    as interface: ExistentialType,
    anchoredAt anchor: SourceRange
  ) -> WrapAddrInstruction {
    precondition(type(of: witness).isAddress)
    return .init(
      witness: witness,
      interface: .address(interface),
      site: anchor)
  }

}
