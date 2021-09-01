import AST
import Basic

/// A VIL function.
public struct VILFun {

  /// The mangled name of the function.
  public let name: VILName

  /// An optional debug name describing the function.
  public let debugName: String?

  /// The VIL type of the function.
  public let type: VILFunType

  /// The basic blocks of the function.
  ///
  /// Do not add or remove blocks using this property. Use `createBlock(arguments:before:)` or
  /// `removeBasicBlock(_:)` instead.
  public var blocks: [BasicBlock.ID: BasicBlock] = [:]

  /// The identifier of the entry block.
  public var entryID: BasicBlock.ID?

  /// A dominator tree representing the dominance relation between the function's basic blocks.
  ///
  /// Do not modify this property directly.
  public internal(set) var blockDominance = AdjacencyList<BasicBlock.ID, Void>()

  /// A dominator tree representing the dominance relation between the function's instructions.
  ///
  /// Do not modify this property directly.
  public internal(set) var instDominance = AdjacencyList<ObjectIdentifier, Void>()

  /// Creates a new VIL function.
  ///
  /// - Parameters:
  ///   - name: The name of the function.
  ///   - type: The unapplied type of the function.
  ///   - debugName: An optional debug name describing the function.
  init(name: VILName, type: VILFunType, debugName: String? = nil) {
    self.name = name
    self.debugName = debugName
    self.type = type
  }

  /// The entry block of the function.
  public var entry: BasicBlock? {
    guard let entryID = self.entryID else { return nil }
    return blocks[entryID]
  }

}
