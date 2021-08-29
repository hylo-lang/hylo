import AST
import Basic

/// A VIL function.
public final class Function {

  /// The mangled name of the function.
  public let name: String

  /// An optional debug name describing the function.
  public let debugName: String?

  /// The VIL type of the function.
  public let type: VILFunType

  /// The basic blocks of the function.
  ///
  /// Do not add or remove blocks using this property. Use `createBlock(arguments:before:)` or
  /// `removeBasicBlock(_:)` instead.
  public var blocks: [BasicBlock.ID: BasicBlock] = [:]

  /// The order in which the blocks of the functions are laid out.
  public private(set) var order: [BasicBlock.ID] = []

  /// The ID of the next basic block created by the function.
  private var nextBlockID = 0

  /// Creates a new VIL function.
  ///
  /// - Parameters:
  ///   - name: The name of the function.
  ///   - type: The unapplied type of the function.
  ///   - debugName: An optional debug name describing the function.
  init(name: String, type: VILFunType, debugName: String? = nil) {
    self.name = name
    self.debugName = debugName
    self.type = type
  }

  /// The ID of the function's entry.
  public var entryID: BasicBlock.ID? {
    return order.first
  }

  /// The entry block of the function.
  public var entry: BasicBlock? {
    guard !order.isEmpty else { return nil }
    return blocks[order[0]]
  }

  /// Returns the index of the specified block in the function.
  public func index(of blockID: BasicBlock.ID) -> Int? {
    return order.firstIndex(of: blockID)
  }

  /// Creates a new basic block at the end of the function.
  ///
  /// - Parameters:
  ///   - arguments: The arguments of the basic block.
  ///   - successor: The ID of the basic block before which the new block should be inserted.
  public func createBasicBlock(
    arguments: [Value] = [],
    before successor: BasicBlock.ID? = nil
  ) -> BasicBlock.ID {
    let blockID = nextBlockID
    nextBlockID += 1
    blocks[blockID] = BasicBlock(arguments: arguments)

    if let s = successor {
      let i = index(of: s) ?< fatalError("specified block is not in the function")
      order.insert(blockID, at: i)
    } else {
      order.append(blockID)
    }

    return blockID
  }

  /// Removes a basic block from the function.
  public func removeBasicBlock(_ blockID: BasicBlock.ID) {
    let i = index(of: blockID) ?< fatalError("specified block is not in the function")
    order.remove(at: i)
    blocks[blockID] = nil
  }

}
