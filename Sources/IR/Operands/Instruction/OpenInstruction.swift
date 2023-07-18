import Core

/// Returns the address of the witness packaged in an existential container converted to a given
/// type along with a flag indicating whether the conversion is legal.
public struct OpenInstruction: Instruction {

  /// The existential container whose witness should be unwrapped.
  public private(set) var container: Operand

  /// The type of the witness to unwrap.
  public let type: AnyType

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(container: Operand, type: AnyType, site: SourceRange) {
    self.container = container
    self.type = type
    self.site = site
  }

  public var types: [IR.LoweredType] { [.address(type), .object(BuiltinType.i(1))] }

  public var operands: [Operand] { [container] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    container = new
  }

}

extension OpenInstruction: CustomStringConvertible {

  public var description: String {
    "open \(container) as \(type)"
  }

}

extension Module {

  /// Creates an `unwrap` anchored at `site` that unwraps the witness of `container` as a value
  /// of type `t`.
  ///
  /// - Parameters:
  ///   - container: An existential container. Must have an existential type.
  ///   - type: The type of the witness packaged in `container`.
  func makeOpen(_ container: Operand, as t: AnyType, at site: SourceRange) -> OpenInstruction {
    precondition(type(of: container).isObject)
    precondition(t[.isCanonical])
    return .init(container: container, type: t, site: site)
  }

}
