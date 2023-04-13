import Core

/// Returns the address of the witness packaged in an existential container converted to a given
/// type along with a flag indicating whether the conversion is legal.
public struct OpenInstruction: Instruction {

  /// The existential container whose witness should be unwrapped.
  public let container: Operand

  /// The type of the witness to unwrap.
  public let type: AnyType

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(container: Operand, type: AnyType, site: SourceRange) {
    self.container = container
    self.type = type
    self.site = site
  }

  public var types: [LoweredType] { [.address(type), .object(BuiltinType.i(1))] }

  public var operands: [Operand] { [container] }

}

extension OpenInstruction: CustomStringConvertible {

  public var description: String {
    "open \(container) as \(type)"
  }

}

extension Module {

  /// Creates an `unwrap` anchored at `anchor` that unwraps the witness of `container` as a value
  /// of type `t`.
  ///
  /// - Parameters:
  ///   - container: An existential container. Must have an existential type.
  ///   - type: The type of the witness packaged in `container`.
  func makeOpen(
    _ container: Operand,
    as t: AnyType,
    anchoredAt anchor: SourceRange
  ) -> OpenInstruction {
    precondition(type(of: container).isObject)
    precondition(t[.isCanonical])
    return .init(container: container, type: t, site: anchor)
  }

}
