import FrontEnd
import Utils

/// Captures and stores a remote part.
///
/// Well-formed IR guarantees the following invariants:
/// - The provenances of `target` contain a single `alloc_stack`.
/// - A `capture ... in` is post-dominated by at least one `release_captures` on the storage in
///   which it has stored an access.
public struct CaptureIn: Instruction, Sendable {

  /// The operands of the instruction.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(source: Operand, target: Operand, site: SourceRange) {
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
    "capture \(source) in \(target)"
  }

}

extension Module {

  /// Creates a `capture ... in` anchored at `site` that captures `source`, which is an access, and
  /// stores it in `target`.
  func makeCapture(_ source: Operand, in target: Operand, at site: SourceRange) -> CaptureIn {
    precondition(type(of: source).isAddress)
    precondition(type(of: target).isAddress)
    return .init(source: source, target: target, site: site)
  }

}
