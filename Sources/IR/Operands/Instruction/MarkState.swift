import FrontEnd

/// Unsafely sets the initialization state of storage.
public struct MarkState: Instruction {

  /// The storage whose initialization state is updated.
  public private(set) var storage: Operand

  /// `true` iff `storage` is marked initialized; `false` if it is marked uninitialized.
  public let initialized: Bool

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(storage: Operand, initialized: Bool, site: SourceRange) {
    self.storage = storage
    self.initialized = initialized
    self.site = site
  }

  public var operands: [Operand] {
    [storage]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    storage = new
  }

}

extension MarkState: CustomStringConvertible {

  public var description: String {
    let s = initialized ? "initialized" : "deinitialized"
    return "mark_state \(s) \(storage)"
  }

}

extension Function {

  /// Creates a `mark_state` instruction anchored at `site` that marks `storage` has being fully
  /// initialized if `initialized` is `true` or fully uninitialized otherwise.
  func makeMarkState(_ storage: Operand, initialized: Bool, at site: SourceRange) -> MarkState {
    precondition(type(of: storage).isAddress)
    return .init(storage: storage, initialized: initialized, site: site)
  }

  /// Creates a `mark_state` instruction anchored at `site` that marks `storage` has being fully
  /// initialized if `initialized` is `true` or fully uninitialized otherwise, inserting it at `p`.
  mutating func makeMarkState(
    _ storage: Operand, initialized: Bool, at site: SourceRange, insertingAt p: InsertionPoint
  ) -> InstructionID {
    insert(makeMarkState(storage, initialized: initialized, at: site), at: p)
  }

}
