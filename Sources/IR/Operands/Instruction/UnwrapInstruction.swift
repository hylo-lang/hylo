import Core

/// Unwraps the witness packaged in an existential container.
public struct UnwrapInstruction: Instruction {

  /// The existential container whose witness should be unwrapped.
  public let container: Operand

  /// The type of the witness to unwrap.
  public let type: LoweredType

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(container: Operand, type: LoweredType, site: SourceRange) {
    self.container = container
    self.type = type
    self.site = site
  }

  public var types: [LoweredType] { [type] }

  public var operands: [Operand] { [container] }

}

extension UnwrapInstruction: CustomStringConvertible {

  public var description: String {
    "unwrap \(container) as \(type)"
  }

}

extension Module {

  /// Creates an `unwrap` anchored at `anchor` that unwraps the witness of `container` as a value
  /// of type `t`.
  ///
  /// - Parameters:
  ///   - container: An existential container. Must have an existential type.
  ///   - type: The type of the witness packaged in `container`.
  func makeUnwrap(
    _ container: Operand,
    as t: AnyType,
    anchoredAt anchor: SourceRange
  ) -> UnwrapInstruction {
    precondition(type(of: container).isObject)
    return .init(
      container: container,
      type: .object(t),
      site: anchor)
  }

}
