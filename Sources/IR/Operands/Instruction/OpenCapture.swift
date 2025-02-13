import FrontEnd
import Utils

/// Exposes a captured access.
public struct OpenCapture: RegionEntry {

  public typealias Exit = CloseCapture

  /// The type of the address being loaded.
  public let result: IR.`Type`?

  /// The operands of the instruction.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(result: IR.`Type`, source: Operand, site: SourceRange) {
    self.result = result
    self.operands = [source]
    self.site = site
  }

  /// The location of the storage containing the address to load.
  public var source: Operand {
    operands[0]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[0] = new
  }

}

extension OpenCapture {

  /// Creates an `open_capture` anchored at `site` that loads the address stored at `source`.
  init(_ source: Operand, at site: SourceRange, in m: Module) {
    let t = RemoteType(m.type(of: source).ast) ?? preconditionFailure()
    self.init(result: .address(t.bareType), source: source, site: site)
  }

}
