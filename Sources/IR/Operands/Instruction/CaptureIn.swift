import Core
import Utils

/// Captures and stores a remote part.
public struct CaptureIn: Instruction {

  /// The access capability being captured.
  public let capability: AccessEffect

  /// The operands of the instruction.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(capability: AccessEffect, source: Operand, target: Operand, site: SourceRange) {
    self.capability = capability
    self.operands = [source, target]
    self.site = site
  }

  /// The captured remote part.
  public var source: Operand { operands[0] }

  /// The location at which the remote part is stored.
  public var target: Operand { operands[1] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension CaptureIn: CustomStringConvertible {

  public var description: String {
    "capture [\(capability)] \(source) in \(target)"
  }

}

extension Module {

  /// Creates a `capture in` anchored at `site` that captures `capability` on `source` and stores
  /// it in `target`.
  func makeCaptureIn(
    _ capability: AccessEffect, from source: Operand, in target: Operand,
    at site: SourceRange
  ) -> CaptureIn {
    precondition(type(of: source).isAddress)
    precondition(type(of: target).isAddress)
    return .init(capability: capability, source: source, target: target, site: site)
  }

}
